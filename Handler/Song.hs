module Handler.Song where

import Import
import Control.Monad

songForm :: Maybe Song -> Form Song 
songForm mbsong = renderTable $ Song
  <$> areq textField "Name" (fmap songName mbsong)
  <*> areq intField "Tempo" (fmap songTempo mbsong)

-- Scf == "Song Chord Form"
-- songchord + id.  
data Scf = Scf 
  {
    song :: SongId,
    chordroot :: ChordRootId,
    noteset :: NoteSetId,
    seqnum :: Int,
    duration :: Int,
    scid :: SongChordId
  } 
 deriving Show

toScf :: SongChordId -> SongChord -> Scf
toScf sc_id sc = 
  Scf {
    song = songChordSong sc, 
    chordroot = songChordChordroot sc, 
    noteset = songChordNoteset sc, 
    seqnum = songChordSeqnum sc, 
    duration = songChordDuration sc, 
    scid = sc_id 
    }

fromScf :: Scf -> (SongChordId, SongChord)
fromScf scf = 
  (scid scf,
   SongChord {
    songChordSong = song scf, 
    songChordChordroot = chordroot scf, 
    songChordNoteset = noteset scf, 
    songChordSeqnum = seqnum scf, 
    songChordDuration = duration scf
   })
  
scfForm :: Maybe Scf -> [(Text,Key ChordRoot)] -> [(Text,Key NoteSet)] -> Form Scf
scfForm mbscf chordroots notesets = renderTable $ Scf 
  <$> areq hiddenField "" (song <$> mbscf)
  <*> areq (selectFieldList chordroots) "Root Note" (chordroot <$> mbscf)
  <*> areq (selectFieldList notesets) "Chord Type" (noteset <$> mbscf)
  <*> areq hiddenField "" (seqnum <$> mbscf)
  <*> areq intField "Duration (beats)" (duration <$> mbscf)
  <*> areq hiddenField "" (scid <$> mbscf)

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
  chordroots <- runDB $ selectList [] [Asc ChordRootName]
  notesets <- runDB $ selectList [] [] 
  let rootz = map (\(Entity crid cr) -> (chordRootName cr, crid)) chordroots
      nsetz = map (\(Entity nsid ns) -> (noteSetName ns, nsid)) notesets 
  chordforms <- mapM (\(Entity scid sc) ->
                    liftM (\fp -> (scid, fp))
                      (generateFormPost $ 
                        scfForm (Just $ toScf scid sc) rootz nsetz))
                  chordz
  (scwidget,scetype) <- 
    generateFormPost $ songChordForm Nothing sid (length chordz) rootz nsetz 
  defaultLayout $ [whamlet|
    <h1> Song
    <form method=post enctype=#{enctype}>
      ^{swidget}
      <input type=submit name="oksong" value="OK">
      <input type=submit name="deletesong" value="delete song">
      <a href=@{CopySongR sid}>copy
    $forall (scid, (widget,etype)) <- chordforms
      <form method=post enctype=#{etype}>
        ^{widget}
        <input type=submit name="updated" value="update">
        <a href=@{DeleteChordR sid scid}>delete
        <a href=@{CopyChordR scid}>copy
    <form method=post enctype=#{enctype}>
      ^{scwidget}
      <input type=submit name="addchord" value="add chord">
   |]

postSongR :: SongId -> Handler Html 
postSongR sid = do 
  addc <- lookupPostParam "addchord"
  updatec <- lookupPostParam "updated"
  oksong <- lookupPostParam "oksong"
  deletesong <- lookupPostParam "deletesong"
  case (oksong,deletesong,addc,updatec) of 
    (Just _, _, _, _) -> do 
      ((res, widget),enctype) <- runFormPost (songForm Nothing)
      case res of 
        FormSuccess song -> do
          res2 <- runDB $ replace sid song
          redirect $ SongR sid
        _ -> defaultLayout [whamlet|fale!|]
    (_, Just _, _, _) -> do 
      runDB $ deleteWhere [SongChordSong ==. sid]
      runDB $ delete sid
      redirect SongsR
    (_, _, Just _, _) -> do 
      chordz <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
      chordroots <- runDB $ selectList [] [Asc ChordRootName]
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
    (_, _, _, Just _) -> do 
      chordz <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
      chordroots <- runDB $ selectList [] [Asc ChordRootName]
      notesets <- runDB $ selectList [] [] 
      let rootz = map (\(Entity crid cr) -> (chordRootName cr, crid)) chordroots
          nsetz = map (\(Entity nsid ns) -> (noteSetName ns, nsid)) notesets 
      ((res, widget),enctype) <- 
        runFormPost $ scfForm Nothing rootz nsetz 
      case res of 
        FormSuccess scf -> do
          let (sc_id, sc) = fromScf scf
          runDB $ replace sc_id sc 
          redirect $ SongR sid
        _ -> defaultLayout [whamlet|meh!|]
    _ -> do
      redirect SongsR


