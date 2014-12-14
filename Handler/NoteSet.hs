module Handler.NoteSet where

import Import
import qualified Data.Text as T

data NsData = NsData
  {  name :: Text,
     denom :: Int,
     notes :: [Bool]
  }

noteArray :: Int -> [(Text,Bool)]
noteArray count = 
  map (\i -> (makename i, False)) [1..count]
  where makename i = T.append (T.pack " ") (T.pack (show i))

ndForm :: Maybe NsData -> Form NsData
ndForm mbnd = renderDivs $ NsData
  <$> areq textField "name" (fmap name mbnd)
  <*> areq intField "denominator" (fmap denom mbnd)
  <*> areq (checkboxesFieldList (noteArray (narray mbnd))) "notes: " (fmap notes mbnd)
  where 
    narray mbnd = 
      case mbnd of 
        Nothing -> 12
        Just nd -> denom nd

noteSetForm :: Maybe NoteSet -> Form NoteSet 
noteSetForm mbnoteSet = renderDivs $ NoteSet
  <$> areq textField "Name" (fmap noteSetName mbnoteSet)

getNoteSetR :: NoteSetId -> Handler Html
getNoteSetR = error "Not yet implemented: postNoteSetR"
  
postNoteSetR :: NoteSetId -> Handler Html
postNoteSetR = error "Not yet implemented: postNoteSetR"
