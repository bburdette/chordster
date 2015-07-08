module Handler.SongSequences where

import Import

getSongSequencesR :: Handler Html
getSongSequencesR = do 
  songseqs <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Song Sequences
    <table> 
      <tr>
        <th>Title
        <th>
      $forall Entity ssid seq <- songseqs
        <tr> 
         <td> 
           <a href=@{SongSequenceR ssid}>  #{songSequenceName seq}
         <td> 
           <a href=@{PlaySongSequenceR ssid}> play
  <a href=@{AddSongSequenceR}>Add new song sequence
  |]


postSongSequencesR :: Handler Html
postSongSequencesR = error "Not yet implemented: postSongSequencesR"
