module Handler.AddChordRoot where

import Import
import Handler.ChordRoot

getAddChordRootR :: Handler Html
getAddChordRootR = do
  (widg,etype) <- generateFormPost $ chordRootForm Nothing
  defaultLayout $ [whamlet|
    <h1> Chord Root
    <form method=post enctype=#{etype}>
      ^{widg}
      <input type=submit value="OK">
    |]

postAddChordRootR :: Handler Html
postAddChordRootR = do 
  ((res,_),_) <- runFormPost $ chordRootForm Nothing
  case res of 
    FormSuccess cr -> do
      runDB $ insert cr
      redirect ChordRootsR
    _ -> do
      redirect ChordRootsR
 
