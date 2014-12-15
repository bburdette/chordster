module Handler.AddNoteSet where

import Import
import Handler.NoteSet

getAddNoteSetR :: Handler Html
getAddNoteSetR = do 
  (ndf,etype) <- generateFormPost $ ndForm Nothing
  defaultLayout $ [whamlet|
    <h1> Chord type
    <form method=post enctype=#{etype}>
      ^{ndf}
      <input type=submit value="OK"> 
    |]

postAddNoteSetR :: Handler Html
postAddNoteSetR = do
  ((res,widg),enctype) <- runFormPost $ ndForm Nothing
  case res of 
    FormSuccess nsdata -> do 
      nsid <- runDB $ insert $ NoteSet (name nsdata)
      let indices = (notes nsdata)
      _ <- mapM (\i -> do 
        runDB $ insert $ Note i (denom nsdata) nsid)
        indices
      redirect NoteSetsR
    _ -> error "error!"

           
