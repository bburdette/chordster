module Handler.ChordRoots where

import Import

getChordRootsR :: Handler Html
getChordRootsR = do 
  chordroots <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Chord Roots
    $forall Entity crid chordroot <- chordroots
      <li> 
        <a href=@{ChordRootR crid}> #{chordRootName chordroot}
    <a href=@{AddChordRootR}>Add chord root
    |]

postChordRootsR :: Handler Html
postChordRootsR = error "Not yet implemented: postChordRootsR"
