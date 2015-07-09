module Handler.CurrentSong where

import Import
import Control.Concurrent.MVar
import PlaySong
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
        aDomId <- newIdent
        setTitle "Song Playback!"
        $(widgetFile "playback")

