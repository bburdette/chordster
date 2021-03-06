module Handler.AddSong where

import Import

newSongForm :: Form Song 
newSongForm = renderDivs $ Song
  <$> areq textField "Name" Nothing
  <*> areq intField "Tempo" Nothing

getAddSongR :: Handler Html
getAddSongR = do 
  (widget,enctype) <- generateFormPost newSongForm 
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{widget}
      <input type=submit value="OK">
    |]

postAddSongR :: Handler Html
postAddSongR = do 
  ((res, projectWidget),enctype) <- runFormPost newSongForm
  case res of 
    FormSuccess newseq -> do
      sid <- runDB $ insert newseq 
      redirect $ SongR sid
    _ -> defaultLayout [whamlet|fale!|]

