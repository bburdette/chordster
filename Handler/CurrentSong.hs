module Handler.CurrentSong where

import Import
import Control.Concurrent.MVar
import PlaySong
import SongControl

getCurrentSongR :: Handler Html
getCurrentSongR = do 
  app <- getYesod 
  wat <- liftIO $ tryReadMVar $ playThread $ songControl app
  case wat of 
    Just (_,songid) -> redirect $ PlaySongR songid
    Nothing -> error "no song is playing right now."
