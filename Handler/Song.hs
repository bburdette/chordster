module Handler.Song where

import Import

songForm :: Maybe Song -> Form Song 
songForm mbsong = renderDivs $ Song
  <$> areq textField "Name" (fmap songName mbsong)
  <*> areq intField "Tempo" (fmap songTempo mbsong)

songChordForm :: Maybe SongChord -> SongId -> Int -> [(Text,Key ChordRoot)] -> [(Text,Key NoteSet)] -> Form SongChord
songChordForm mbsc sid seqnum chordroots notesets = renderDivs $ SongChord 
  <$> pure (maybe sid songChordSong mbsc)
  <*> areq (selectFieldList chordroots) "Root Note" (songChordChordroot <$> mbsc)
  <*> areq (selectFieldList notesets) "Chord Type" (songChordNoteset <$> mbsc)
  <*> pure seqnum
  <*> areq intField "Duration (beats)" (songChordDuration <$> mbsc)

getSongR :: SongId -> Handler Html
getSongR sid = do
  mbsong <- runDB $ get sid
  (swidget,enctype) <- generateFormPost $ songForm mbsong
  chordz <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  chordroots <- runDB $ selectList [] []
  notesets <- runDB $ selectList [] [] 
  let rootz = map (\(Entity crid cr) -> (chordRootName cr, crid)) chordroots
      nsetz = map (\(Entity nsid ns) -> (noteSetName ns, nsid)) notesets 
  (scwidget,scetype) <- 
    generateFormPost $ songChordForm Nothing sid (length chordz) rootz nsetz 
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{swidget}
      <input type=submit name="oksong" value="OK">
    <table class=song>
      <tr>
        <th> Sequence 
        <th> Root
        <th> Chord
        <th> Beats
      $forall Entity scid songchord <- chordz
        <tr>
          <td> #{show (songChordSeqnum songchord)} 
          <td> #{show (songChordChordroot songchord)}
          <td> #{show (songChordNoteset songchord)}
          <td> #{show (songChordDuration songchord)}
    <form method=post enctype=#{enctype}>
      ^{scwidget}
      <input type=submit name="addchord" value="add chord">
   |]

{-
          <td> #{show (songChordSeqnum songchord)} 
          <td> #{show (songChordChordroot songchord)}
          <td> #{show (songChordNoteset songchord)}
          <td> #{show (songChordDuration songchord)}
-} 

postSongR :: SongId -> Handler Html 
postSongR sid = do 
  addc <- lookupPostParam "addchord"
  oksong <- lookupPostParam "oksong"
  case (oksong,addc) of 
    (Just _, _) -> do 
      ((res, widget),enctype) <- runFormPost (songForm Nothing)
      case res of 
        FormSuccess song -> do
          res2 <- runDB $ replace sid song
          redirect SongsR
        _ -> defaultLayout [whamlet|fale!|]
    (_, Just _) -> do 
      chordz <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
      chordroots <- runDB $ selectList [] []
      notesets <- runDB $ selectList [] [] 
      let rootz = map (\(Entity crid cr) -> (chordRootName cr, crid)) chordroots
          nsetz = map (\(Entity nsid ns) -> (noteSetName ns, nsid)) notesets 
      ((res, widget),enctype) <- runFormPost $ songChordForm Nothing sid (length chordz) rootz nsetz 
      case res of 
        FormSuccess songchord -> do
          res2 <- runDB $ insert songchord
          redirect (SongR sid)
        _ -> defaultLayout [whamlet|meh!|]
    _ -> do
      redirect SongsR

