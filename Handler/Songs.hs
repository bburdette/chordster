module Handler.Songs where

import Import

getSongsR :: Handler Html
getSongsR = do
  songs <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Songs
    <table> 
      <tr>
        <th>Title
        <th>
      $forall Entity sid seq <- songs
        <tr> 
         <td> 
           <a href=@{SongR sid}>  #{songName seq}
         <td> 
           <a href=@{PlaySongR sid}>play
  <a href=@{AddSongR}>Add new song
  |]

postSongsR :: Handler Html
postSongsR = error "Not yet implemented: postSongsR"
