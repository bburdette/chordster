module Handler.AddNoteSet where

import Import
import Handler.NoteSet

getAddNoteSetR :: Handler Html
getAddNoteSetR = do 
  (ncks,etype) <- generateFormPost $ noteChecks (noteArray 12) Nothing
  defaultLayout $ [whamlet|
    <h1> Chord type
    <form method=post enctype=#{etype}>
      ^{ncks} 
    |]

postAddNoteSetR :: Handler Html
postAddNoteSetR = error "Not yet implemented: postAddNoteSetR"
