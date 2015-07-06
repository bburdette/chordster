module Handler.AddSongSequence where

import Import

newSongSequenceForm :: Form SongSequence
newSongSequenceForm = renderDivs $ SongSequence
  <$> areq textField "Name" Nothing

getAddSongSequenceR :: Handler Html
getAddSongSequenceR = do
  (widget,enctype) <- generateFormPost newSongSequenceForm 
  defaultLayout $ [whamlet|
    <h1> Song Sequence
    <form method=post enctype=#{enctype}>
      ^{widget}
      <input type=submit value="OK">
    |]


postAddSongSequenceR :: Handler Html
postAddSongSequenceR = do
  ((res, projectWidget),enctype) <- runFormPost newSongSequenceForm
  case res of 
    FormSuccess newseq -> do
      ssid <- runDB $ insert newseq 
      redirect $ SongSequenceR ssid
    _ -> defaultLayout [whamlet|fale!|]
