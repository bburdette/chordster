module Handler.Song where

import Import
import Control.Monad

songForm :: Maybe Song -> Form Song 
songForm mbsong = renderTable $ Song
  <$> areq textField "Name" (fmap songName mbsong)
  <*> areq intField "Tempo" (fmap songTempo mbsong)

data Scf = Scf 
  {
    song :: SongId,
    chordroot :: ChordRootId,
    noteset :: NoteSetId,
    seqnum :: Int,
    duration :: Int,
    scid :: Maybe SongChordId
  } 

toScf sc scid = 
  Scf {
    song = songChordSong sc, 
    chordroot = songChordChordroot sc, 
    noteset = songChordNoteset sc, 
    seqnum = songChordSeqnum sc, 
    duration = songChordDuration sc, 
    scid = Just scid 
    }

fromScf scf = 
  ((scid scf),
   SongChord {
    songChordSong = song scf, 
    songChordChordroot = chordroot scf, 
    songChordNoteset = noteset scf, 
    songChordSeqnum = seqnum scf, 
    songChordDuration = duration scf
   })
  
scfFrm :: Maybe Scf -> SongId -> Int -> [(Text,Key ChordRoot)] -> [(Text,Key NoteSet)] -> Form Scf
scfFrm mbscf sid seqnum chordroots notesets = renderTable $ Scf 
  <$> pure (maybe sid song mbscf)
  <*> areq (selectFieldList chordroots) "Root Note" (chordroot <$> mbscf)
  <*> areq (selectFieldList notesets) "Chord Type" (noteset <$> mbscf)
  <*> pure seqnum
  <*> areq intField "Duration (beats)" (duration <$> mbscf)
  <*> pure (maybe Nothing scid mbscf)

songChordForm :: Maybe SongChord -> SongId -> Int -> [(Text,Key ChordRoot)] -> [(Text,Key NoteSet)] -> Form SongChord
songChordForm mbsc sid seqnum chordroots notesets = renderTable $ SongChord 
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
  chordforms <- mapM (\(Entity cid ch) ->
                    liftM (\fp -> (cid, fp))
                      (generateFormPost $ 
                        songChordForm (Just ch) sid (length chordz) rootz nsetz))
                  chordz
  (scwidget,scetype) <- 
    generateFormPost $ songChordForm Nothing sid (length chordz) rootz nsetz 
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{swidget}
      <input type=submit name="oksong" value="OK">
    $forall (scid, (widget,etype)) <- chordforms
      <form method=post enctype=#{etype}>
        ^{widget}
        <input type=submit name="updated" value="ed chord">
    <form method=post enctype=#{enctype}>
      ^{scwidget}
      <input type=submit name="addchord" value="add chord">
   |]

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
      ((res, widget),enctype) <- 
        runFormPost $ songChordForm Nothing sid (length chordz) rootz nsetz 
      case res of 
        FormSuccess songchord -> do
          res2 <- runDB $ insert songchord
          redirect (SongR sid)
        _ -> defaultLayout [whamlet|meh!|]
    _ -> do
      redirect SongsR

