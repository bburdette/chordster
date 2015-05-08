module Handler.ExportSong where

import Import
import PlaySong
import Text.Show.Pretty
import qualified Data.Text as T

getExportSongR :: SongId -> Handler RepPlain 
getExportSongR sid = do 
  mbtextsong <- loadTextSong sid   
  case mbtextsong of 
    Just ts -> return $ RepPlain $ toContent $ T.pack (ppShow ts)
    Nothing -> return $ RepPlain $ toContent $ T.pack "song not found"


