module Handler.Chords where

import Import

chordForm :: Maybe Chord -> [(Text, Key NoteSet)] -> Form Chord
chordForm mbchord notesets = renderDivs $ Chord
   <$> areq textField "Name" (fmap chordName mbchord)
   <*> areq intField "Numerator" (fmap chordNumer mbchord)
   <*> areq intField "Denominator" (fmap chordDenom mbchord)
   <*> areq (selectFieldList notesets) "Chord Type" (fmap chordNoteset mbchord)

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
