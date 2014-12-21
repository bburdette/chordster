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

playSong :: SongControl -> Song -> [PlaySongChord] -> String -> Int -> IO ()
playSong sc song chords ip port = do
  t <- openUDP ip port
  playit t (songTempo song) chords

chordnotes :: [Rational] -> [Int]
chordnotes [] = []
chordnotes rats = 
  let denom = denominator $ head rats
      notes = map numerator rats
    in
  map fromInteger (denom : notes)
  
playit :: UDP -> Int -> [PlaySongChord] -> IO ()
playit conn tempo [] = return ()
playit conn tempo (psc:pscs) = 
  -- set the root
  let rootmsg = Message "root" (map d_put [(chordRootNumer (chordRoot psc)), 
                                (chordRootDenom (chordRoot psc))])
      chordmsg = Message "scale" $ map d_put $ chordnotes $ notes psc
    in do
  sendOSC conn rootmsg
  sendOSC conn chordmsg 
  threadDelay (tempo * songChordDuration (songChord psc))
  playit conn tempo pscs

