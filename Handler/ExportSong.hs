module Handler.ExportSong where

import Import
import PlaySong
import Text.Show.Pretty

getExportSongR :: SongId -> Handler Import.Html
getExportSongR sid = do 
  mbtextsong <- loadTextSong sid   
  case mbtextsong of 
    Just ts -> defaultLayout $ [whamlet|#{ppShow ts}|]
    Nothing -> defaultLayout $ [whamlet|not found!|]
