module SongControl where

import Import
import Control.Concurrent
import Sound.OSC.FD
import Data.Ratio
import Data.List

data SongControl = SongControl 
  {
    tempo :: Int,
    stahp :: Bool,
    running :: Bool,
    playsong :: Maybe PlaySongChord
  }

data PlaySongChord = PlaySongChord
  { songChord :: SongChord
  , chordRoot :: ChordRoot
  , name :: Text
  , notes :: [Rational]
  }

makePscs :: [SongChord] -> Handler [Maybe PlaySongChord]
makePscs scs = do
  mapM scPsc scs

scPsc :: SongChord -> Handler (Maybe PlaySongChord)
scPsc sc = do
    mbchroot <- runDB $ get (songChordChordroot sc) 
    mbntst <- runDB $ get (songChordNoteset sc) 
    notes <- runDB $ selectList [NoteNoteset ==. songChordNoteset sc] []
    case (mbchroot, mbntst) of
      (Nothing,_) -> return Nothing
      (_,Nothing) -> return Nothing
      (Just chr, Just ntst) -> 
        return $ Just PlaySongChord {
           songChord = sc, 
           chordRoot = chr, 
           name = (noteSetName ntst),
           notes = map (\(Entity nid note) -> fromIntegral (noteNumer note) % fromIntegral (noteDenom note)) notes
           }

tempoToBeattime :: Int -> Int
tempoToBeattime tempo =  
  -- tempo is Beats per minute.
  -- one minute in microseconds is...
  let minute = 1000000 * 60 in 
    div minute tempo

playSong :: Song -> [PlaySongChord] -> [(String,Int)] -> IO ()
playSong song chords dests = do
  cons <- mapM (\(ip,port) -> openUDP ip port) dests
  playit cons ((tempoToBeattime . songTempo) song) chords

chordnotes :: [Rational] -> [Int]
chordnotes [] = []
chordnotes rats = 
  let denom = denominator $ head rats
      notes = map numerator rats
    in
  map fromInteger (denom : notes)
  
playit :: [UDP] -> Int -> [PlaySongChord] -> IO ()
playit cons beattime [] = return ()
playit cons beattime (psc:pscs) = 
  -- set the root
  let rootmsg = Message "root" (map d_put [(chordRootNumer (chordRoot psc)), 
                                (chordRootDenom (chordRoot psc))])
      chordmsg = Message "scale" $ map d_put $ chordnotes $ notes psc
    in do
  _ <- mapM (\conn -> do 
          sendOSC conn rootmsg
          sendOSC conn chordmsg)
      cons 
  threadDelay (beattime * songChordDuration (songChord psc))
  playit cons beattime pscs

