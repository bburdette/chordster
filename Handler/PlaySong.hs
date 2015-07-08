module Handler.PlaySong where

import Import
import Control.Concurrent.MVar
import SongControl
import qualified Data.Text as T
import Control.Monad (forever)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.IORef
import Data.Maybe
import Database.Persist.Sql
import Data.Traversable as TR
import PlaySong
import Yesod.WebSockets
import Text.Julius
import Database.Persist.Sql 

-- startWhateverThread :: TChan Text -> Handler ThreadId
-- startWhateverThread textchan = do
-- playSong :: TChan Text -> Song -> [PlaySongChord] -> [(String,Int)] -> [(String,Int)] -> IO ()

playWhateverWs :: (TChan Text -> IO ()) -> Maybe Text -> Whatever -> WebSocketsT Handler ()
playWhateverWs whateverftn mbnewthreadtext whateverid = do 
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  -- info for old whatever currently playing.
  oldinfo <- liftIO $ readIORef $ whateverThread $ songControl app
  liftIO $ print $ "oldinfo: " ++ show oldinfo
  case oldinfo of 
    Just (tid, id) | id == whateverid -> do  
      liftIO $ print $ "playwhatever thread exists; doing nothing. " ++ show (tid, id)
      case mbnewthreadtext of
        Just text -> do
          liftIO $ print ("at sendtextdata: " ++ show text)
          sendTextData text
          return () 
        _ -> return ()
    Just (tid, _) -> do  
      -- kill the old thread. 
      liftIO $ print $ "killing old thread: " ++ show tid
      liftIO $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
      -- start a new song thread
      threadid <- liftIO $ forkIO $ whateverftn writeChan 
      -- save the new thread id and song id in the MVar.
      -- _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
      res <- liftIO $ writeIORef (whateverThread $ songControl app) $ Just (threadid, whateverid)
      liftIO $ print $ "res= " ++ show res
      return ()
    Nothing -> do 
      liftIO $ print $ "starting new song thread"  
      -- start a new song thread
      threadid <- liftIO $ forkIO $ whateverftn writeChan 
      -- save the new thread id and song id in the MVar.
      res <- liftIO $ writeIORef (whateverThread $ songControl app) $ Just (threadid, whateverid)
      liftIO $ print $ "nothing res= " ++ show res
      return ()
  liftIO $ print "pre readTChan, etc"
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
  liftIO $ print "post readTChan, etc"
  return ()

playSongWhateverWs :: SongId -> WebSocketsT Handler ()
playSongWhateverWs sid = do 
  -- info for new song to play.
  mbsonginfo <- lift $ getSongInfo sid 
  case mbsonginfo of 
    Just (song, chords) -> do  
      -- liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
      -- actually, send song info if here.
      let websong = toWebSong 0 song chords
          wsjs = toJSON websong
      -- sendTextData (toJsonText wsjs)
      chorddests <- lift $ runDB $ selectList [OSCDestType ==. T.pack "chords"] [] 
      lightdests <- lift $ runDB $ selectList [OSCDestType ==. T.pack "lights"] [] 
      let chordips = map (\(Entity _ dest) -> 
                            (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                          chorddests
          lightips = map (\(Entity _ dest) -> 
                            (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                          lightdests
          whateverid = WhatSong sid
          whateverftn tc = playSong tc song chords chordips lightips
      playWhateverWs whateverftn (Just (toJsonText wsjs)) whateverid     
    _ -> 
      return ()

  
playSongWs :: SongId -> WebSocketsT Handler ()
playSongWs sid = do 
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  -- info for new song to play.
  mbsonginfo <- lift $ getSongInfo sid 
  -- info for old song currenty playing.
  oldinfo <- liftIO $ readIORef $ playThread $ songControl app
  liftIO $ print $ "oldinfo: " ++ show oldinfo
  case (mbsonginfo, oldinfo) of 
    (Nothing, _) -> do
      liftIO $ print $ "song not found: " ++ show sid 
      return ()
    (Just (song, chords), Just (tid, id)) | id == sid -> do  
      liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
      -- actually, send song info if here.
      let websong = toWebSong 0 song chords
          wsjs = toJSON websong
      sendTextData (toJsonText wsjs)
      return () 
    (Just (song, chords), Just (tid, _)) -> do  
      -- kill the old song thread. 
      liftIO $ print $ "killing old thread: " ++ show tid
      liftIO $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
      -- start a new song thread
      threadid <- lift $ startSongThread writeChan song chords 
      -- save the new thread id and song id in the MVar.
      -- _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
      res <- liftIO $ writeIORef (playThread $ songControl app) $ Just (threadid, sid)
      liftIO $ print $ "res= " ++ show res
      return ()
    (Just (song, chords), Nothing) -> do 
      liftIO $ print $ "starting new song thread"  
      -- start a new song thread
      threadid <- lift $ startSongThread writeChan song chords 
      -- save the new thread id and song id in the MVar.
      res <- liftIO $ writeIORef (playThread $ songControl app) $ Just (threadid, sid)
      liftIO $ print $ "nothing res= " ++ show res
      return ()
  liftIO $ print "pre readTChan, etc"
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
  liftIO $ print "post readTChan, etc"
  return ()

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do
  webSockets $ playSongWhateverWs sid
  mbsong <- runDB $ get sid
  let _ = $(juliusFileReload "templates/playback.julius")
  case mbsong of 
    (Just song) ->
      defaultLayout $ do
        aDomId <- newIdent
        setTitle "Song Playback!"
        $(widgetFile "playback")
    Nothing -> error "song not found"
 
getSongInfo :: SongId -> Handler (Maybe (Song, [PlaySongChord]))
getSongInfo sid = do
  app <- getYesod 
  mbsong <- runDB $ get sid
  chords <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  songchords <- makePscs (map entityVal chords)
  case mbsong of 
    (Just song) -> return (Just (song, catMaybes songchords))
    Nothing -> return Nothing

startSongThread :: TChan Text -> Song -> [PlaySongChord] -> Handler ThreadId
startSongThread textchan song chords = do
  chorddests <- runDB $ selectList [OSCDestType ==. T.pack "chords"] [] 
  lightdests <- runDB $ selectList [OSCDestType ==. T.pack "lights"] [] 
  let chordips = map (\(Entity _ dest) -> 
                        (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                      chorddests
      lightips = map (\(Entity _ dest) -> 
                        (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                      lightdests
  liftIO $ forkIO $ 
    playSong textchan song chords chordips lightips

postPlaySongR :: SongId -> Handler Html
postPlaySongR sid = do 
  meh <- lookupPostParam "stop"
  case meh of 
    Nothing -> do 
      redirect SongsR
    Just _ -> do 
      app <- getYesod 
      -- to do: make this whole thing atomic.
      oldinfo <- liftIO $ readIORef $ whateverThread $ songControl app
      case oldinfo of 
        Nothing -> return ()
        Just (tid,_) -> do
          lift $ print $ show $ fromSqlKey sid 
          (liftIO . atomically) $ 
            writeTChan (songLine app) (toJsonText $ toJSON (WsStop (fromSqlKey sid)))
          lift $ killThread tid 
          liftIO $ writeIORef (whateverThread $ songControl app) $ Nothing 
      redirect SongsR
