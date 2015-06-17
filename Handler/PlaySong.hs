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
import Data.Maybe
import Data.Traversable as TR
import PlaySong
import Yesod.WebSockets
import Text.Julius

playSongWs :: SongId -> WebSocketsT Handler ()
playSongWs sid = do 
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  -- info for new song to play.
  mbsonginfo <- lift $ getSongInfo sid 
  -- info for old song currenty playing.
  oldinfo <- liftIO $ tryReadMVar $ playThread $ songControl app
  liftIO $ print $ "oldinfo: " ++ show oldinfo
  case (mbsonginfo, oldinfo) of 
    (Nothing, _) -> do
      liftIO $ print $ "song not found: " ++ show sid 
      return ()
    (Just (song, chords), Just (tid, id)) | id == sid -> do  
      liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
      -- actually, send song info if here.
      let websong = toWebSong song chords
          wsjs = toJSON websong
      sendTextData (toJsonText wsjs)
      -- (liftIO . atomically) $ writeTChan textchan (toJsonText wsjs)
      return () 
    (Just (song, chords), Just (tid, _)) -> do  
      -- kill the old song thread. 
      liftIO $ print $ "killing old thread: " ++ show tid
      liftIO $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
      -- start a new song thread
      threadid <- lift $ startSongThread' writeChan song chords 
      -- threadid <- liftIO $ forkIO $ 
      --   playSong writeChan song chords chordips lightips
      -- save the new thread id and song id in the MVar.
      _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
      res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
      liftIO $ print $ "res= " ++ show res
      {-
      -}
      return ()
    (Just (song, chords), Nothing) -> do 
      liftIO $ print $ "starting new song thread"  
      -- start a new song thread
      threadid <- lift $ startSongThread' writeChan song chords 
      -- store in the mvar.
      res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
      liftIO $ print $ "nothing res= " ++ show res
      {-
      threadid <- liftIO $ forkIO $ 
        playSong (songLine app) song (catMaybes pscs) chordips lightips
      -}
      return ()
  -- lift $ startSongThread sid
  liftIO $ print "pre readTChan, etc"
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
  liftIO $ print "post readTChan, etc"
  return ()

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do
  webSockets $ playSongWs sid
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

-- playSong :: TChan Text -> Song -> [PlaySongChord] -> [(String,Int)] -> [(String,Int)] -> IO ()
-- playSong textchan song chords chorddests lightdests = do

startSongThread' :: TChan Text -> Song -> [PlaySongChord] -> Handler ThreadId
startSongThread' textchan song chords = do
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
  {-
  -- kill the old song, if any.
  oldinfo <- liftIO $ tryReadMVar $ playThread $ songControl app
  liftIO $ print $ "oldinfo: " ++ show oldinfo
  case oldinfo of 
    Just (tid, id) | id == sid -> do  
      liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
      return () 
    Just (tid, _) -> do
      -- kill the old song thread. 
      liftIO $ print $ "killing old thread: " ++ show tid
      lift $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
      -- start a new song thread
      threadid <- liftIO $ forkIO $ 
        playSong (songLine app) song (catMaybes pscs) chordips lightips
      -- save the new thread id and song id in the MVar.
      _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
      res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
      liftIO $ print $ "res= " ++ show res
      return ()
    Nothing -> do
      liftIO $ print $ "starting new song thread"  
      -- start a new song thread
      threadid <- liftIO $ forkIO $ 
        playSong (songLine app) song (catMaybes pscs) chordips lightips
      -- store in the mvar.
      res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
      liftIO $ print $ "nothing res= " ++ show res
      return ()
 -}

startSongThread :: SongId -> Handler ()
startSongThread sid = do
  app <- getYesod 
  mbsong <- runDB $ get sid
  chords <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  chorddests <- runDB $ selectList [OSCDestType ==. T.pack "chords"] [] 
  lightdests <- runDB $ selectList [OSCDestType ==. T.pack "lights"] [] 
  case (mbsong,chorddests) of 
    (Nothing,_) -> 
      error "song not found"
    (_,[]) ->
      error "no osc IP destinations set up"
    (Just song, dests) -> do
      let chordips = map (\(Entity _ dest) -> 
                            (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                          chorddests
          lightips = map (\(Entity _ dest) -> 
                            (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                          lightdests
      pscs <- makePscs (map entityVal chords)
      -- kill the old song, if any.
      oldinfo <- liftIO $ tryReadMVar $ playThread $ songControl app
      liftIO $ print $ "oldinfo: " ++ show oldinfo
      case oldinfo of 
        Just (tid, id) | id == sid -> do  
          liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
          return () 
        Just (tid, _) -> do
          -- kill the old song thread. 
          liftIO $ print $ "killing old thread: " ++ show tid
          lift $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
          -- start a new song thread
          threadid <- liftIO $ forkIO $ 
            playSong (songLine app) song (catMaybes pscs) chordips lightips
          -- save the new thread id and song id in the MVar.
          _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
          res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
          liftIO $ print $ "res= " ++ show res
          return ()
        Nothing -> do
          liftIO $ print $ "starting new song thread"  
          -- start a new song thread
          threadid <- liftIO $ forkIO $ 
            playSong (songLine app) song (catMaybes pscs) chordips lightips
          -- store in the mvar.
          res <- liftIO $ putMVar (playThread $ songControl app) (threadid, sid)
          liftIO $ print $ "nothing res= " ++ show res
          return ()
 
postPlaySongR :: SongId -> Handler Html
postPlaySongR sid = do 
  meh <- lookupPostParam "stop"
  case meh of 
    Nothing -> do 
      redirect SongsR
    Just _ -> do 
      app <- getYesod 
      oldinfo <- liftIO $ tryTakeMVar $ playThread $ songControl app
      case oldinfo of 
         Nothing -> return ()
         Just (tid,_) -> lift $ killThread tid 
      redirect SongsR


