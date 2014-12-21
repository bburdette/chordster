module Handler.PlaySong where

import Import
import Control.Concurrent.MVar
import SongControl

{-
meh :: MVar SongControl 
meh <- newEmptyMVar
-}

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do 
  mbsong <- runDB $ get sid
  chords <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  case mbsong of 
    Nothing -> 
      error "song not found"
    Just song -> 
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
