module Handler.Song where

import Import

songForm :: Maybe Song -> Form Song 
songForm mbsong = renderDivs $ Song
  <$> areq textField "Name" (fmap songName mbsong)
  <*> areq intField "Tempo" (fmap songTempo mbsong)

getSongR :: SongId -> Handler Html
getSongR sid = do
  mbsong <- runDB $ get sid
  (widget,enctype) <- generateFormPost $ songForm mbsong
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{widget}
      <input type=submit value="OK">
    |]

postSongR :: SongId -> Handler Html 
postSongR sid = do 
  ((res, widget),enctype) <- runFormPost (songForm Nothing)
  case res of 
    FormSuccess song -> do
      res2 <- runDB $ replace sid song
      redirect SongsR
    _ -> defaultLayout [whamlet|fale!|]

