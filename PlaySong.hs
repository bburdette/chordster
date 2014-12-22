module PlaySong where

import Import
import Control.Concurrent
import Sound.OSC.FD
import Data.Ratio
import Data.List
import Control.Monad.Loops
import SongControl

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

playSong :: Song -> [PlaySongChord] -> [(String,Int)] -> [(String,Int)] -> IO ()
playSong song chords chorddests lightdests = do
  chordcons <- mapM (\(ip,port) -> openUDP ip port) chorddests
  lightcons <- mapM (\(ip,port) -> openUDP ip port) lightdests
  iterateWhile (\_ -> True) 
    (playit chordcons lightcons ((tempoToBeattime . songTempo) song) chords)

chordnotes :: [Rational] -> [Int]
chordnotes [] = []
chordnotes rats = 
  let denom = denominator $ head rats
      notes = map numerator rats
    in
  map fromInteger (denom : notes)
  
playit :: [UDP] -> [UDP] -> Int -> [PlaySongChord] -> IO ()
playit ccons lcons beattime [] = return ()
playit ccons lcons beattime (psc:pscs) = 
  -- on chord change, set the root and the scale.
  let rootmsg = Message "root" (map d_put [(chordRootNumer (chordRoot psc)), 
                                (chordRootDenom (chordRoot psc))])
      chordmsg = Message "scale" $ map d_put $ chordnotes $ notes psc
    in do
  -- send root and scale msgs to all destinations.
  _ <- mapM (\conn -> do 
          sendOSC conn rootmsg
          sendOSC conn chordmsg)
      ccons 
  -- delay N times for N beats, flashing the lights each time.  
  let flashmsg1 = Message "fadeto" (map d_put [1::Int,100])
      flashmsg2 = Message "fadeto" (map d_put [0::Int,100])
  _ <- mapM (\_ -> do 
    -- send light flash msgs
    _ <- mapM (\conn -> do 
            sendOSC conn flashmsg1 
            sendOSC conn flashmsg2)
            lcons
    -- delay for a beat.
    threadDelay beattime)
    (take (songChordDuration (songChord psc)) [0..])
  playit ccons lcons beattime pscs

