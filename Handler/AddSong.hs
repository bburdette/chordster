module Handler.AddSong where

import Import

newSongForm :: Form Song 
newSongForm = renderDivs $ Song
  <$> areq textField "Name" Nothing

getAddSongR :: Handler Html
getAddSongR = do 
  (widget,enctype) <- generateFormPost newSongForm 
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{widget}
    |]

postAddSongR :: Handler Html
postAddSongR = do 
  ((res, projectWidget),enctype) <- runFormPost newSongForm
  case res of 
    FormSuccess newseq -> do
      _ <- runDB $ insert newseq 
      redirect SongsR
    _ -> defaultLayout [whamlet|fale!|]

