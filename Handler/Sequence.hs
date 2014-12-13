module Handler.Sequence where

import Import

{-
getSequenceR :: SequenceId -> Handler Html
getSequenceR = error "Not yet implemented: getSequenceR"

postSequenceR :: SequenceId -> Handler Html
postSequenceR = error "Not yet implemented: postSequenceR"
-}

sequenceForm :: Maybe Sequence -> Form Sequence 
sequenceForm mbseq = renderDivs $ Sequence
  <$> areq textField "Name" (fmap sequenceName mbseq)

getSequenceR :: SequenceId -> Handler Html
getSequenceR sid = do
  mbseq <- runDB $ get sid
  (widget,enctype) <- generateFormPost $ sequenceForm mbseq
  defaultLayout $ [whamlet|
    <h1> Sequence
    <form method=post enctype=#{enctype}>
      ^{widget}
    |]

postSequenceR :: SequenceId -> Handler Html 
postSequenceR sid = do 
  ((res, widget),enctype) <- runFormPost (sequenceForm Nothing)
  case res of 
    FormSuccess seq -> do
      res2 <- runDB $ replace sid seq 
      redirect SequencesR
    _ -> defaultLayout [whamlet|fale!|]

