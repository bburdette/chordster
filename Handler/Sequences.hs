module Handler.Sequences where

import Import

getSequencesR :: Handler Html
getSequencesR = do
  sequences <- runDB $ selectList [] []
  defaultLayout $ [whamlet|
    <h1> Sequences
    <ul> 
    $forall Entity sid seq <- sequences
       <li> 
          <a href=@{SequenceR sid}>  #{sequenceName seq}
  |]

postSequencesR :: Handler Html
postSequencesR = error "Not yet implemented: postSequencesR"
