module Handler.CopyChord where

import Import

getCopyChordR :: SongChordId -> Handler Html
getCopyChordR scid = do 
  mbsid <- runDB $ do 
    mbchord <- get scid
    case mbchord of 
      Just chord -> do 
        let sid = songChordSong chord
            sn = songChordSeqnum chord
        chords <- selectList [SongChordSong ==. sid, SongChordSeqnum >. sn] []
        let updchords = map (\ech -> (Entity (entityKey ech) 
                              ((entityVal ech) { songChordSeqnum = (songChordSeqnum (entityVal ech)) + 1 }))) 
                            chords 
        liftIO $ print $ "upchords " ++ show (map (\(Entity _ ch) -> (songChordSeqnum ch)) updchords)
        _ <- mapM (\meh -> replace (entityKey meh) (entityVal meh)) 
             updchords
        _ <- insert $ chord { songChordSeqnum = (songChordSeqnum chord) + 1 }
        return $ Just sid
      Nothing -> return Nothing
  case mbsid of 
    Just sid -> 
      redirect $ SongR sid
    Nothing -> 
      error "song chord id not found, probably"
