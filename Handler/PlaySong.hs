module Handler.PlaySong where

import Import
import Control.Concurrent.MVar
import SongControl
import qualified Data.Text as T
import Control.Concurrent
import Data.Maybe

{-
meh :: MVar SongControl 
meh <- newEmptyMVar
-}

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do 
  mbsong <- runDB $ get sid
  chords <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  dests <- runDB $ selectList [OSCDestType ==. T.pack "chords"] [] 
  case (mbsong,dests) of 
    (Nothing,_) -> 
      error "song not found"
    (_,[]) ->
      error "no osc IP destinations set up"
    (Just song, dests) -> do
      let ips = map (\(Entity _ dest) -> (T.unpack $ oSCDestIp dest, oSCDestPort dest)) dests
      pscs <- makePscs (map entityVal chords)
      liftIO $ forkIO $ playSong song (catMaybes pscs) ips
      -- blam, start a thread for song playing.
      -- report back that there is a song playing, or something.
      -- track whatever it is so that it can be stopped later.  
      defaultLayout $ [whamlet|
        <h1> Song Playback: #{songName song}
        <form method=post>
          <input type=submit name="stop" value="stop">
        |] 


postPlaySongR :: SongId -> Handler Html
postPlaySongR = error "Not yet implemented: postPlaySongR"
