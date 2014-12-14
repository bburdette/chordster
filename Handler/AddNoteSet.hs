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
postAddNoteSetR = error "Not yet implemented: postAddNoteSetR"
