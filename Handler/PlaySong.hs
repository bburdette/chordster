module Handler.PlaySong where

import Import
import SongControl
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.IORef
import Database.Persist.Sql
import PlaySong
import Yesod.WebSockets
import Text.Julius
import PlayWhatever


playSongWhateverWs :: SongId -> WebSocketsT Handler ()
playSongWhateverWs sid = do 
  -- info for new song to play.
  app <- getYesod
  mbsonginfo <- lift $ getSongInfo sid 
  case mbsonginfo of 
    Just (song, chords) -> do  
      -- liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
      -- actually, send song info if here.
      (chordips, lightips) <- lift $ getDests
      let websong = toWebSong 0 song chords
          wsjs = toJSON websong
      -- sendTextData (toJsonText wsjs)
          whateverid = WhatSong sid
          whateverftn tc = playSong sid ((currentSong . songControl) app) tc song chords chordips lightips
      playWhateverWs whateverftn (Just (toJsonText wsjs)) whateverid     
    _ -> 
      return ()

getPlaySongR :: SongId -> Handler Html
getPlaySongR sid = do
  webSockets $ playSongWhateverWs sid
  mbsong <- runDB $ get sid
  let _ = $(juliusFileReload "templates/playback.julius")
  case mbsong of 
    (Just _) ->
      defaultLayout $ do
        setTitle "Song Playback!"
        $(widgetFile "playback")
    Nothing -> error "song not found"

postPlaySongR :: SongId -> Handler Html
postPlaySongR sid = do 
  meh <- lookupPostParam "stop"
  case meh of 
    Nothing -> do 
      redirect SongsR
    Just _ -> do 
      app <- getYesod 
      -- to do: make this whole thing atomic.
      oldinfo <- liftIO $ readIORef $ whateverThread $ songControl app
      case oldinfo of 
        Nothing -> return ()
        Just (tid,_) -> do
          lift $ print $ show $ fromSqlKey sid 
          (liftIO . atomically) $ 
            writeTChan (songLine app) (toJsonText $ toJSON (WsStop (fromSqlKey sid)))
          lift $ killThread tid 
          liftIO $ writeIORef (whateverThread $ songControl app) $ Nothing 
      redirect SongsR
