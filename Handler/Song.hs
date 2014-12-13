module Handler.Song where

import Import

{-
getSongR :: SongId -> Handler Html
getSongR = error "Not yet implemented: getSongR"

postSongR :: SongId -> Handler Html
postSongR = error "Not yet implemented: postSongR"
-}

songForm :: Maybe Song -> Form Song 
songForm mbseq = renderDivs $ Song
  <$> areq textField "Name" (fmap songName mbseq)
  <*> areq intField "Tempo" (fmap songTempo mbseq)

getSongR :: SongId -> Handler Html
getSongR sid = do
  mbseq <- runDB $ get sid
  (widget,enctype) <- generateFormPost $ songForm mbseq
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
    FormSuccess seq -> do
      res2 <- runDB $ replace sid seq 
      redirect SongsR
    _ -> defaultLayout [whamlet|fale!|]

