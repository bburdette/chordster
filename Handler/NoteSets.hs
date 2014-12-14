module Handler.NoteSets where

import Import

getNoteSetsR :: Handler Html
getNoteSetsR = do 
  notesets <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Chord Types
    $forall Entity nsid ns <- notesets
      <li> 
        <a href=@{NoteSetR nsid}> #{noteSetName ns}
    <a href=@{AddNoteSetR}>add chord type
    |]


postNoteSetsR :: Handler Html
postNoteSetsR = error "Not yet implemented: postNoteSetsR"
