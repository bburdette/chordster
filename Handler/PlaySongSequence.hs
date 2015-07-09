module Handler.PlaySongSequence where

import Import
import PlayWhatever
import Yesod.WebSockets
import Handler.PlaySong
import SongControl
import PlaySong
import Data.Maybe
import Text.Julius
import qualified Data.Text as T
import qualified Data.Traversable as TR
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.IORef
import Database.Persist.Sql 

-- getPlaySongR :: SongId -> Handler Html
-- getPlaySongR sid = do
--
-- playSongSequence :: TChan Text -> [(Song, Int, [PlaySongChord])] -> [(String,Int)] -> [(String,Int)] -> IO ()
-- playSongSequence textchan songchords chorddests lightdests = do

playSongSeqWhateverWs :: SongSequenceId -> WebSocketsT Handler ()
playSongSeqWhateverWs ssid = do 
  -- info for new song to play.
  songseqs <- lift $ runDB $ selectList [SongSeqItemSongsequence ==. ssid] [Asc SongSeqItemSeqnum]
  srcs <- lift $ 
    mapM (\(Entity _ sseq) -> do 
            mbsonginfo <- getSongInfo (songSeqItemSong sseq)
            case mbsonginfo of 
              Just (song, chords) -> return $ Just (song, (songSeqItemSong sseq), (songSeqItemReps sseq), chords)
              Nothing -> return $ Nothing)
            -- return $ TR.traverse (\(song, chords) -> (song, (songSeqItemReps sseq), chords)) mbsonginfo)
          songseqs
  let songrepchords = catMaybes srcs
  -- liftIO $ print $ "song thread exists; doing nothing. " ++ show (tid, id)
  -- actually, send song info if here.
  -- let websong = toWebSong 0 song chords
  --    wsjs = toJSON websong
  -- sendTextData (toJsonText wsjs)
  chorddests <- lift $ runDB $ selectList [OSCDestType ==. T.pack "chords"] [] 
  lightdests <- lift $ runDB $ selectList [OSCDestType ==. T.pack "lights"] [] 
  app <- getYesod
  let chordips = map (\(Entity _ dest) -> 
                        (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                      chorddests
      lightips = map (\(Entity _ dest) -> 
                        (T.unpack $ oSCDestIp dest, oSCDestPort dest)) 
                      lightdests
      whateverid = WhatSongSequence ssid
      whateverftn tc = playSongSequence ((currentSong . songControl) app) tc songrepchords chordips lightips
  playWhateverWs whateverftn Nothing whateverid     
--  return ()

getPlaySongSequenceR :: SongSequenceId -> Handler Html
getPlaySongSequenceR ssid = do  
  webSockets $ playSongSeqWhateverWs ssid
  let _ = $(juliusFileReload "templates/playback.julius")
  defaultLayout $ do
    aDomId <- newIdent
    setTitle "Song Playback!"
    $(widgetFile "playback")
 
postPlaySongSequenceR :: SongSequenceId -> Handler Html
postPlaySongSequenceR ssid = do 
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
          -- lift $ print $ show $ fromSqlKey sid 
          (liftIO . atomically) $ 
            writeTChan (songLine app) (toJsonText $ toJSON (WsStop (fromSqlKey ssid)))
          lift $ killThread tid 
          liftIO $ writeIORef (whateverThread $ songControl app) $ Nothing 
      redirect SongSequencesR
