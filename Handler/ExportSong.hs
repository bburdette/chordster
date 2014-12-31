module Handler.ExportSong where

import Import
import PlaySong
import Text.Show.Pretty

--getExportSongR :: SongId -> Handler Import.Html
getExportSongR :: SongId -> Handler Import.Html
getExportSongR sid = do 
  mbtextsong <- loadTextSong sid   
  case mbtextsong of 
    Just ts -> defaultLayout [whamlet|#{ppShow ts}|]
    Nothing -> defaultLayout [whamlet|not found!|]

{-
getExportSongR :: SongId -> Handler Text
getExportSongR sid = do 
  mbtextsong <- loadTextSong sid   
  case mbtextsong of 
    Just ts -> renderHtml [whamlet|#{ppShow ts}|]
    Nothing -> renderHtml [whamlet|not found!|]

-}

{-
textLayout :: WidgetT site IO () -> HandlerT site IO Import.Html
textLayout w = do
    p <- widgetToPageContent w
    withUrlRenderer [hamlet|
        $newline never
        $doctype 5
        ^{pageBody p}
        |]
-}
