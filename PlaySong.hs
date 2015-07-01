{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PlaySong where

import Import
import Control.Concurrent
import Sound.OSC.FD
import Control.Monad.Loops
import SongControl
import Data.Ratio
import Data.List
import Data.Int
import Data.Maybe
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import GHC.Generics

data PlaySongChord = PlaySongChord
  { songChord :: SongChord
  , chordRoot :: ChordRoot
  , name :: Text
  , notes :: [Rational]
  }
  deriving Show

data TextSong = TextSong 
  { song :: Song
  , chords :: [PlaySongChord]
  }
  deriving Show

data WebSong = WebSong
  { wsId :: Int64
  , wsName :: Text
  , wsChords :: [WebChord]
  , wsTempo :: Int
  }
  deriving (Show, Generic)
instance ToJSON WebSong 

data WebChord = WebChord
  { wcName :: Text
  , wcDuration :: Int
  }
  deriving (Show, Generic)
instance ToJSON WebChord

data WsIndex = WsIndex { wiIndex :: Int }
  deriving (Show, Generic)

instance ToJSON WsIndex 

data WsStop = WsStop
  { wssId :: Int64
  }
  deriving Generic

instance ToJSON WsStop 

{-
tsToWebSong :: TextSong -> WebSong
tsToWebSong ts = WebSong { 
    wsName = songName (song ts)
  , wsChords = (\psc -> name psc) <$> (chords ts)
  }
-}

toWebSong :: Int64 -> Song -> [PlaySongChord] -> WebSong
toWebSong sid song chords = WebSong { 
    wsId = sid 
  , wsName = songName song 
  , wsChords = 
      (\psc -> WebChord
        { wcName = (chordRootName (chordRoot psc) <> name psc)
        , wcDuration = songChordDuration (songChord psc) } )
      <$> chords 
  , wsTempo = songTempo song 
  }

loadTextSong :: SongId -> Handler (Maybe TextSong)
loadTextSong sid = do
  mbsong <- runDB $ get sid
  chords <- runDB $ selectList [SongChordSong ==. sid] [Asc SongChordSeqnum]
  pscs <- makePscs (map entityVal chords)
  case mbsong of 
    Nothing -> return Nothing
    Just sng -> return $ Just $ TextSong sng (catMaybes pscs)

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

setupLights :: [UDP] -> IO ()
setupLights lightcons = do 
  -- send light flash msgs
  _ <- mapM (\conn -> do 
          setArrayColor conn 1 0 
          setArrayColor conn 2 16711680)
          lightcons
  return ()

setArrayColor :: UDP -> Int -> Int -> IO ()
setArrayColor conn arrayindex color = do
  let tst = (Message "updatearray" [(d_put arrayindex)])
  sendOSC conn tst 
  _ <- mapM (\elt -> do 
        sendOSC conn (Message "setpixel" [(d_put elt), (d_put color)]))
        [0..23::Int]
  return ()

playSong :: TChan Text -> Song -> [PlaySongChord] -> [(String,Int)] -> [(String,Int)] -> IO ()
playSong textchan song chords chorddests lightdests = do
  chordcons <- mapM (\(ip,port) -> openUDP ip port) chorddests
  lightcons <- mapM (\(ip,port) -> openUDP ip port) lightdests
  setupLights lightcons
  let websong = toWebSong 0 song chords
      wsjs = toJSON websong
  (liftIO . atomically) $ writeTChan textchan (toJsonText wsjs)
  iterateWhile (\_ -> True) 
    (playit textchan chordcons lightcons ((tempoToBeattime . songTempo) song) chords 0)

chordnotes :: Int -> [Rational] -> [Int]
chordnotes _ [] = []
chordnotes den rats = 
  let 
      notes = map (\rat -> (fromIntegral (numerator rat)) * (quot den (fromIntegral (denominator rat)))) rats
    in
  (den : notes)
  
playit :: TChan Text -> [UDP] -> [UDP] -> Int -> [PlaySongChord] -> Int -> IO ()
playit textchan ccons lcons beattime [] count = return ()
playit textchan ccons lcons beattime (psc:pscs) count = 
  -- on chord change, set the root and the scale.
  let rootmsg = Message "root" (map d_put [(chordRootNumer (chordRoot psc)), 
                                (chordRootDenom (chordRoot psc))])
      chordmsg = Message "scale" $ map d_put $ chordnotes 12 $ notes psc
      -- chordtext = (chordRootName (chordRoot psc)) <> (name psc)
      wsi = WsIndex { wiIndex = count } 
      wsijs = toJSON wsi
    in do
  -- send root and scale msgs to all destinations.
  _ <- mapM (\conn -> do 
          sendOSC conn rootmsg
          sendOSC conn chordmsg)
      ccons 
  (liftIO . atomically) $ writeTChan textchan (toJsonText wsijs)
  -- delay N times for N beats, flashing the lights each time.  
  let flashmsg1 = Message "fadeto" (map d_put [0::Int,20])
      flashmsg2 = Message "fadeto" (map d_put [1::Int,20])
  _ <- mapM (\_ -> do 
    -- send light flash msgs
    _ <- mapM (\conn -> do 
            sendOSC conn flashmsg1 
            sendOSC conn flashmsg2)
            lcons
    -- delay for a beat.
    threadDelay beattime)
    (take (songChordDuration (songChord psc)) [0..])
  playit textchan ccons lcons beattime pscs (count + 1)

