module Handler.PlaySongSequence where

import Import

-- getPlaySongR :: SongId -> Handler Html
-- getPlaySongR sid = do

getPlaySongSequenceR :: SongSequenceId -> Handler Html
getPlaySongSequenceR ssi = error "blah"
{-
  webSockets $ playSongWs sid
  mbsong <- runDB $ get sid
  let _ = $(juliusFileReload "templates/playback.julius")
  case mbsong of 
    (Just song) ->
      defaultLayout $ do
        aDomId <- newIdent
        setTitle "Song Playback!"
        $(widgetFile "playback")
    Nothing -> error "song not found"
-}
 
postPlaySongSequenceR :: SongSequenceId -> Handler Html
postPlaySongSequenceR = error "Not yet implemented: postPlaySongSequenceR"
