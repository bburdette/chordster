module Handler.CopySong where

import Import
import qualified Data.Text as T

getCopySongR :: SongId -> Handler Html
getCopySongR sid = do 
  mbsong <- runDB $ get sid 
  case mbsong of 
    Just song -> do 
      chords <- runDB $ selectList [SongChordSong ==. sid] []
      runDB $ do 
        id <- insert (song { songName = T.append (songName song) " copy" })
        mapM (\(Entity _ ch) -> 
          insert (ch { songChordSong = id } )) chords
      redirect SongsR
    Nothing -> error "song id not found"
