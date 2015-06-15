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
  lift $ startSongThread sid
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)

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
      oldinfo <- liftIO $ tryTakeMVar $ playThread $ songControl app
      case oldinfo of 
        Just (tid, id) | id == sid -> return () 
        _ -> do 
          lift $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
          -- start a new song thread
          threadid <- liftIO $ forkIO $ 
            playSong (songLine app) song (catMaybes pscs) chordips lightips
          -- save the new thread id and song id in the MVar.
          res <- liftIO $ tryPutMVar (playThread $ songControl app) (threadid, sid)
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


