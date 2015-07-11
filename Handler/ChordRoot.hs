module Handler.ChordRoot where

import Import

chordRootForm :: Maybe ChordRoot -> Form ChordRoot
chordRootForm mbchordRoot = renderDivs $ ChordRoot
   <$> areq textField "Name" (fmap chordRootName mbchordRoot)
   <*> areq intField "Numerator" (fmap chordRootNumer mbchordRoot)
   <*> areq intField "Denominator" (fmap chordRootDenom mbchordRoot)

getChordRootR :: ChordRootId -> Handler Html
getChordRootR crid = do
  mbcr <- runDB $ get crid 
  (widg,etype) <- generateFormPost $ chordRootForm mbcr
  defaultLayout $ [whamlet|
    <h1> Chord Root
    <form method=post enctype=#{etype}>
      ^{widg}
      <input type=submit value="OK">
    <a href=@{ DeleteChordRootR crid }>delete
    |]

postChordRootR :: ChordRootId -> Handler Html
postChordRootR crid = do 
  ((res,_),_) <- runFormPost $ chordRootForm Nothing
  case res of 
    FormSuccess cr -> do
      runDB $ replace crid cr
      redirect ChordRootsR
    _ -> do
      redirect ChordRootsR
 
