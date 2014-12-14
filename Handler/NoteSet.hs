module Handler.NoteSet where

import Import

data NoteChecks = NoteChecks 
  { note00 :: Bool,
    note01 :: Bool,
    note02 :: Bool,
    note03 :: Bool,
    note04 :: Bool,
    note05 :: Bool,
    note06 :: Bool,
    note07 :: Bool,
    note08 :: Bool,
    note09 :: Bool,
    note10 :: Bool,
    note11 :: Bool }

noteChecks :: Maybe NoteChecks -> Form NoteChecks
noteChecks mbnc = renderDivs $ NoteChecks
  <$> areq checkBoxField "0" (fmap note00 mbnc) 
  <*> areq checkBoxField "1" (fmap note01 mbnc) 
  <*> areq checkBoxField "2" (fmap note02 mbnc) 
  <*> areq checkBoxField "3" (fmap note03 mbnc) 
  <*> areq checkBoxField "4" (fmap note04 mbnc) 
  <*> areq checkBoxField "5" (fmap note05 mbnc) 
  <*> areq checkBoxField "6" (fmap note06 mbnc) 
  <*> areq checkBoxField "7" (fmap note07 mbnc) 
  <*> areq checkBoxField "8" (fmap note08 mbnc) 
  <*> areq checkBoxField "9" (fmap note09 mbnc) 
  <*> areq checkBoxField "10" (fmap note10 mbnc) 
  <*> areq checkBoxField "11" (fmap note11 mbnc) 

noteSetForm :: Maybe NoteSet -> Form NoteSet 
noteSetForm mbnoteSet = renderDivs $ NoteSet
  <$> areq textField "Name" (fmap noteSetName mbnoteSet)

getNoteSetR :: NoteSetId -> Handler Html
getNoteSetR = error "Not yet implemented: getNoteSetR"

postNoteSetR :: NoteSetId -> Handler Html
postNoteSetR = error "Not yet implemented: postNoteSetR"
