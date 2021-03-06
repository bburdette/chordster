module Handler.CurrentSong where

import Import
import SongControl
import Data.IORef
import Yesod.WebSockets
import Text.Julius
import PlayWhatever

getCurrentSongR :: Handler Html
getCurrentSongR = do 
  app <- getYesod 
  -- is there a current song?
  mbsid <- liftIO $ readIORef ((currentSong . songControl) app)
  case mbsid of 
    (Just sid) -> 
      redirect $ PlaySongR sid 
    Nothing -> do 
      webSockets $ listenWs 
      let _ = $(juliusFileReload "templates/playback.julius")
      defaultLayout $ do
        setTitle "Song Playback!"
        $(widgetFile "playback")

