module Handler.Chords where

import Import

chordForm :: Maybe Chord -> Form Chord
chordForm mbchord = renderDivs $ Chord
   <$> areq textField "Name" (fmap chordName mbchord)

getChordsR :: Handler Html
getChordsR = do 
  chords <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Chords
    $forall Entity cid chord <- chords
      <li> 
        <a href=@{ChordR cid}> #{chordName chord}
    |]

postChordsR :: Handler Html
postChordsR = error "Not yet implemented: postChordsR"
