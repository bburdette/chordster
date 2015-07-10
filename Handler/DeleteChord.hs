module Handler.DeleteChord where

import Import

getDeleteChordR :: SongId -> SongChordId -> Handler Html
getDeleteChordR sid scid = do 
  runDB $ deleteWhere [SongChordId ==. scid]
  renumberChords sid
  redirect $ SongR sid

renumberChords :: SongId -> Handler ()
renumberChords sid = do 
  chordz <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  let rnchords = reverse $ foldl addtalist [] chordz
      addtalist :: [(Entity SongChord)] -> Entity SongChord -> [(Entity SongChord)]
      addtalist (c:cs) ch = (Entity (entityKey ch) 
                                    ((entityVal ch) { songChordSeqnum = (songChordSeqnum (entityVal c)) + 1 })) 
        : c : cs
      addtalist [] ch = [(Entity (entityKey ch) 
                               ((entityVal ch) { songChordSeqnum = 0 }))]
  runDB $ 
    mapM (\ent -> replace (entityKey ent) (entityVal ent)) rnchords
  return ()


