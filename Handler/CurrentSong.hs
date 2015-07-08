module Handler.CurrentSong where

import Import
import Control.Concurrent.MVar
import PlaySong
import SongControl
import Data.IORef

getCurrentSongR :: Handler Html
getCurrentSongR = do 
  app <- getYesod 
  wat <- liftIO $ readIORef $ whateverThread $ songControl app
  case wat of 
    Just (_,WhatSong songid) -> redirect $ PlaySongR songid
    Just (_,WhatSongSequence _) -> error "current songsequene unimplemented" 
    Nothing -> error "no song is playing right now."
