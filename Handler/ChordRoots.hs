module Handler.ChordRoots where

import Import

getChordRootsR :: Handler Html
getChordRootsR = do 
  chordroots <- runDB $ selectList [] [Asc ChordRootName]
  defaultLayout $ [whamlet|
    <h1> Chord Roots
    $forall Entity crid chordroot <- chordroots
      <li> 
        <a href=@{ChordRootR crid}> #{chordRootName chordroot}
        #{ show (chordRootNumer chordroot) }/#{show (chordRootDenom chordroot) }
    <a href=@{AddChordRootR}>Add chord root
    |]

postChordRootsR :: Handler Html
postChordRootsR = error "Not yet implemented: postChordRootsR"
