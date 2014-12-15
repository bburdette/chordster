module Handler.NoteSet where

import Import
import qualified Data.Text as T

data NsData = NsData
  {  name :: Text,
     denom :: Int,
     notes :: [Int]
  }

noteArray :: Int -> [(Text,Int)]
noteArray count = 
  map (\i -> (makename i, i)) [1..count]
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
getNoteSetR nsid = do 
  mbns <- runDB $ get nsid
  notes <- runDB $ selectList [NoteNoteset ==. nsid] [Asc NoteNumer]
  let notenums = (map (\(Entity nid note) -> noteNumer note) notes)
      mbnsdata = (case mbns of 
                    Just ns -> Just NsData { name = noteSetName ns,
                                      denom = 12,
                                      notes = notenums }
                    Nothing -> Nothing)
  (ndform,etype) <- generateFormPost $ ndForm mbnsdata
  defaultLayout $ [whamlet|
    <h1>Chord Type
    <form method=post enctype=#{etype}>
      ^{ndform}
      <input type=submit name="save" value="save changes">
      <input type=submit name="delete" value="delete">
    |]
  
postNoteSetR :: NoteSetId -> Handler Html
postNoteSetR nsid = do
  ((res,widg),enctype) <- runFormPost $ ndForm Nothing
  case res of 
    FormSuccess nsdata -> do 
      sav <- lookupPostParam "save"
      del <- lookupPostParam "delete"
      case (sav,del) of 
        (Just _, _) -> do
          _ <- runDB $ replace nsid $ NoteSet (name nsdata)
          runDB $ deleteWhere [NoteNoteset ==. nsid]
          let indices = (notes nsdata)
          _ <- mapM (\i -> do 
            runDB $ insert $ Note i (denom nsdata) nsid)
            indices
          redirect NoteSetsR
        (_, Just _) -> do
          runDB $ deleteWhere [NoteNoteset ==. nsid]
          runDB $ deleteWhere [NoteSetId ==. nsid]
          redirect NoteSetsR
        _ -> do 
          error "error1!"
    FormFailure blah -> do
      defaultLayout $ [whamlet|
        #{show blah} 
        |]
    FormMissing -> do 
      error "missing"
        
      --error "error2!"


