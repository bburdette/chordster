module Handler.ExportSong where

import Import
import PlaySong

getExportSongR :: SongId -> Handler Html
getExportSongR sid = do 
  mbtextsong <- loadTextSong sid   
  case mbtextsong of 
    Just ts -> defaultLayout $ [whamlet|#{show ts}|]
    Nothing -> defaultLayout $ [whamlet|not found!|]
