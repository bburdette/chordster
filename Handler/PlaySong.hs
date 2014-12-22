module Handler.PlaySong where

import Import
import Control.Concurrent.MVar
import SongControl
import qualified Data.Text as T
import Control.Concurrent
import Data.Maybe
import PlaySong
{-
meh :: MVar SongControl 
meh <- newEmptyMVar
-}

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do
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
      threadid <- liftIO $ forkIO $ playSong song (catMaybes pscs) chordips lightips
      oldid <- liftIO $ tryTakeMVar $ playThread $ songControl app
      case oldid of 
         Nothing -> return ()
         Just id -> lift $ killThread id 
      res <- liftIO $ tryPutMVar (playThread $ songControl app) threadid
      -- blam, start a thread for song playing.
      -- report back that there is a song playing, or something.
      -- track whatever it is so that it can be stopped later.  
      defaultLayout $ [whamlet|
        <h1> Song Playback: #{songName song}
        <form method=post>
          <input type=submit name="stop" value="stop">
        |] 


postPlaySongR :: SongId -> Handler Html
postPlaySongR sid = do 
  meh <- lookupPostParam "stop"
  case meh of 
    Nothing -> do 
      redirect SongsR
    Just _ -> do 
      app <- getYesod 
      oldid <- liftIO $ tryTakeMVar $ playThread $ songControl app
      case oldid of 
         Nothing -> return ()
         Just tid -> lift $ killThread tid 
      redirect SongsR


