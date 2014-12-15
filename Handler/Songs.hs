module Handler.Songs where

import Import

getSongsR :: Handler Html
getSongsR = do
  songs <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Songs
    <ul> 
    $forall Entity sid seq <- songs
       <li> 
          <a href=@{SongR sid}>  #{songName seq}
  <a href=@{AddSongR}>Add new song
  |]

postSongsR :: Handler Html
postSongsR = error "Not yet implemented: postSongsR"
