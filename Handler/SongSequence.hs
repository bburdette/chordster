module Handler.SongSequence where

import Import
import Control.Monad

songSequenceForm :: Maybe SongSequence -> Form SongSequence 
songSequenceForm mbsongSequence = renderTable $ SongSequence
  <$> areq textField "Name" (fmap songSequenceName mbsongSequence)

data Ssif = Ssif 
  {
    songsequence :: SongSequenceId,
    song :: SongId,
    seqnum :: Int,
    reps :: Int,
    ssiid :: SongSeqItemId
  } 
 deriving Show

{-
SongSeqItem
  songsequence SongSequenceId
  song SongId
  reps Int
  seqnum Int
  deriving Show
-}

toSsif :: SongSeqItemId -> SongSeqItem -> Ssif
toSsif ssi_id ssi = 
  Ssif {
    songsequence = songSeqItemSongsequence ssi, 
    song = songSeqItemSong ssi, 
    seqnum = songSeqItemSeqnum ssi, 
    reps = songSeqItemReps ssi, 
    ssiid = ssi_id 
    }

fromSsif :: Ssif -> (SongSeqItemId, SongSeqItem)
fromSsif ssif = 
  (ssiid ssif,
   SongSeqItem {
    songSeqItemSongsequence = songsequence ssif, 
    songSeqItemSong = song ssif, 
    songSeqItemSeqnum = seqnum ssif, 
    songSeqItemReps = reps ssif
   })
  
ssifForm :: Maybe Ssif -> [(Text,Key Song)] -> Form Ssif
ssifForm mbssif songs = renderTable $ Ssif 
  <$> areq hiddenField "" (songsequence <$> mbssif)
  <*> areq (selectFieldList songs) "Song" (song <$> mbssif)
  <*> areq hiddenField "" (seqnum <$> mbssif)
  <*> areq intField "Reps" (reps <$> mbssif)
  <*> areq hiddenField "" (ssiid <$> mbssif)

newSsiForm :: SongSequenceId -> Int -> [(Text,Key Song)] -> Form SongSeqItem
newSsiForm ssid seqnum songs = renderTable $ SongSeqItem 
  <$> areq hiddenField "" (Just ssid)
  <*> areq (selectFieldList songs) "Song" Nothing 
  <*> areq intField "Reps" Nothing
  <*> areq hiddenField "" (Just seqnum)

getSongSequenceR :: SongSequenceId -> Handler Html
getSongSequenceR ssid = do
  mbsongseq <- runDB $ get ssid
  (swidget,enctype) <- generateFormPost $ songSequenceForm mbsongseq
  seqitems <- runDB $ selectList [SongSeqItemSongsequence ==. ssid] [Asc SongSeqItemSeqnum]
  songs <- runDB $ selectList [] []
  let songz = map (\(Entity sid song) -> (songName song, sid)) songs
  seqitemforms <- mapM (\(Entity ssiid seqitem) ->
                    liftM (\fp -> (ssiid, fp))
                      (generateFormPost $ 
                        ssifForm (Just $ toSsif ssiid seqitem) songz))
                  seqitems
  (scwidget,scetype) <- 
    generateFormPost $ newSsiForm ssid (length seqitems) songz 
  defaultLayout $ [whamlet|
    <h1> Song Sequence
    <form method=post enctype=#{enctype}>
      ^{swidget}
      <input type=submit name="oksongseq" value="OK">
      <input type=submit name="deletesongseq" value="delete song sequence">
    $forall (scid, (widget,etype)) <- seqitemforms
      <form method=post enctype=#{etype}>
        ^{widget}
        <input type=submit name="updated" value="update">
    <form method=post enctype=#{enctype}>
      ^{scwidget}
      <input type=submit name="addseqitem" value="add song">
   |]


postSongSequenceR :: SongSequenceId -> Handler Html
postSongSequenceR ssid = do 
  addsi <- lookupPostParam "addseqitem"
  update <- lookupPostParam "updated"
  oksongseq <- lookupPostParam "oksongseq"
  deletesongseq <- lookupPostParam "deletesongseq"
  case (oksongseq,deletesongseq,addsi,update) of 
    (Just _, _, _, _) -> do 
      ((res, widget),enctype) <- runFormPost (songSequenceForm Nothing)
      case res of 
        FormSuccess songseq -> do
          res2 <- runDB $ replace ssid songseq
          redirect $ SongSequenceR ssid
        _ -> defaultLayout [whamlet|fale!|]
    (_, Just _, _, _) -> do 
      runDB $ deleteWhere [SongSeqItemSongsequence ==. ssid]
      runDB $ delete ssid
      redirect SongSequencesR
    (_, _, Just _, _) -> do 
      ssiz <- runDB $ selectList [SongSeqItemSongsequence ==. ssid] [Asc SongSeqItemSeqnum]
      songs <- runDB $ selectList [] []
      let songz = map (\(Entity sid song) -> (songName song, sid)) songs
      ((res, widget),enctype) <- 
        runFormPost $ newSsiForm ssid (length ssiz) songz 
      case res of 
        FormSuccess songsequence -> do
          res2 <- runDB $ insert songsequence 
          redirect (SongSequenceR ssid)
        _ -> defaultLayout [whamlet|meh!|]
    (_, _, _, Just _) -> do 
      ssiz <- runDB $ selectList [SongSeqItemSongsequence ==. ssid] [Asc SongSeqItemSeqnum]
      songs <- runDB $ selectList [] []
      let songz = map (\(Entity sid song) -> (songName song, sid)) songs
      ((res, widget),enctype) <- 
        runFormPost $ ssifForm Nothing songz 
      case res of 
        FormSuccess ssif -> do
          let (ssi_id, ssi) = fromSsif ssif
          runDB $ replace ssi_id ssi 
          redirect $ SongSequenceR ssid
        _ -> defaultLayout [whamlet|meh!|]
    _ -> do
      redirect SongSequencesR

