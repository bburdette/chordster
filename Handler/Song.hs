module Handler.Song where

import Import
import Control.Monad

songForm :: Maybe Song -> Form Song 
songForm mbsong = renderTable $ Song
  <$> areq textField "Name" (fmap songName mbsong)
  <*> areq intField "Tempo" (fmap songTempo mbsong)

-- Scf == Song Chord Form
-- songchord record + id.  
data Scf = Scf 
  {
    song :: SongId,
    chordroot :: ChordRootId,
    noteset :: NoteSetId,
    seqnum :: Int,
    duration :: Int,
    scid :: Key SongChord
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
  ((scid scf),
   SongChord {
    songChordSong = song scf, 
    songChordChordroot = chordroot scf, 
    songChordNoteset = noteset scf, 
    songChordSeqnum = seqnum scf, 
    songChordDuration = duration scf
   })
  
scfForm :: Scf -> [(Text,Key ChordRoot)] -> [(Text,Key NoteSet)] -> Form Scf
scfForm scf chordroots notesets = renderTable $ Scf 
  <$> pure (song scf)
  <*> areq (selectFieldList chordroots) "Root Note" (Just (chordroot scf))
  <*> areq (selectFieldList notesets) "Chord Type" (Just (noteset scf))
  <*> pure (seqnum scf)
  <*> areq intField "Duration (beats)" (Just (duration scf))
  <*> pure (scid scf)

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
  chordforms <- mapM (\(Entity scid sc) ->
                    liftM (\fp -> (scid, fp))
                      (generateFormPost $ 
                        scfForm (toScf scid sc) rootz nsetz))
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
        <br> scid: #{show scid}
        <input type=submit name="updated" value="ed chord">
    <form method=post enctype=#{enctype}>
      ^{scwidget}
      <input type=submit name="addchord" value="add chord">
   |]

postSongR :: SongId -> Handler Html 
postSongR sid = do 
  addc <- lookupPostParam "addchord"
  update <- lookupPostParam "updated"
  oksong <- lookupPostParam "oksong"
  case (oksong,addc,update) of 
    (Just _, _, _) -> do 
      ((res, widget),enctype) <- runFormPost (songForm Nothing)
      case res of 
        FormSuccess song -> do
          res2 <- runDB $ replace sid song
          redirect SongsR
        _ -> defaultLayout [whamlet|fale!|]
    (_, Just _, _) -> do 
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
    (_, _, Just _) -> do 
      schord <- runDB $ selectFirst [SongChordSong ==. sid] [Asc SongChordSeqnum]
      chordroots <- runDB $ selectList [] []
      notesets <- runDB $ selectList [] [] 
      let rootz = map (\(Entity crid cr) -> (chordRootName cr, crid)) chordroots
          nsetz = map (\(Entity nsid ns) -> (noteSetName ns, nsid)) notesets 
      case schord of 
        Just (Entity scid sc) -> do
          ((res, widget),enctype) <- 
            runFormPost $ scfForm (toScf scid sc) rootz nsetz 
          case res of 
            FormSuccess scf -> do
              let (scid, sc) = fromScf scf
              runDB $ replace scid sc
              defaultLayout $ [whamlet|
                <h1> #{show scid}
                <br> #{show scf}
                |]
              -- redirect (SongR sid)
            _ -> defaultLayout [whamlet|meh!|]
        _ -> defaultLayout [whamlet|wot!|]
    _ -> do
      redirect SongsR

