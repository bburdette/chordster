module Handler.NoteSet where

import Import
import qualified Data.Text as T

data NoteChecks = NoteChecks 
  { notes :: [Bool]
  }

noteArray :: Int -> [(Text,Bool)]
noteArray count = 
  map (\i -> (makename i, False)) [0..count]
  where makename i = T.append (T.pack " ") (T.pack (show i))

noteChecks :: [(Text,Bool)] -> Maybe NoteChecks -> Form NoteChecks
noteChecks blah mbnc = renderDivs $ NoteChecks
  <$> areq (checkboxesFieldList blah) "notes: " (fmap notes mbnc)

noteSetForm :: Maybe NoteSet -> Form NoteSet 
noteSetForm mbnoteSet = renderDivs $ NoteSet
  <$> areq textField "Name" (fmap noteSetName mbnoteSet)

getNoteSetR :: NoteSetId -> Handler Html
getNoteSetR = error "Not yet implemented: postNoteSetR"
  
postNoteSetR :: NoteSetId -> Handler Html
postNoteSetR = error "Not yet implemented: postNoteSetR"
