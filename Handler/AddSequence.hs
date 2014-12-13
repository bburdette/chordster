module Handler.AddSequence where

import Import

newSequenceForm :: Form Sequence 
newSequenceForm = renderDivs $ Sequence
  <$> areq textField "Name" Nothing

getAddSequenceR :: Handler Html
getAddSequenceR = do 
  (widget,enctype) <- generateFormPost newSequenceForm 
  defaultLayout $ [whamlet|
    <h1> Sequence
    <form method=post enctype=#{enctype}>
      ^{widget}
    |]

postAddSequenceR :: Handler Html
postAddSequenceR = do 
  ((res, projectWidget),enctype) <- runFormPost newSequenceForm
  case res of 
    FormSuccess newseq -> do
      _ <- runDB $ insert newseq 
      redirect SequencesR
    _ -> defaultLayout [whamlet|fale!|]

