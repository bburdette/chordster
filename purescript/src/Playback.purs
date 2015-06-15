module Main where

import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.String
import Data.Either
import Data.Tuple
import qualified Data.Array as A
import Debug.Trace
import Control.Alt
import DOM
import qualified WebSocket as WS
import Control.Monad.Eff.Ref
import Data.DOM.Simple.Window
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Dims
import Data.Date 
import Data.Foldable
import Data.Time
import Data.Traversable

--  data structures we're receiving from haskell ----
-- main data structure is WebMessage, which could be a websong or a wsindex.

data WebChord = WebChord
  { wcName :: String
  , wcDuration :: Number
  }
 
data WebSong = WebSong
  { wsName :: String
  , wsChords :: [WebChord]
  , wsTempo :: Number
  }

data WsIndex = WsIndex
  { wiIndex :: Number
  }

instance webChordIsForeign :: IsForeign WebChord where
  read value = do 
    wn <- readProp "wcName" value
    dr <- readProp "wcDuration" value
    return $ WebChord 
      { wcName: wn 
      , wcDuration: dr
      }

instance webSongIsForeign :: IsForeign WebSong where
  read value = do 
    wn <- readProp "wsName" value
    ch <- readProp "wsChords" value
    tm <- readProp "wsTempo" value
    return $ WebSong  
      { wsName: wn
      , wsChords: ch 
      , wsTempo: tm 
      }

instance wsIndexIsForeign :: IsForeign WsIndex where
  read value = do 
    wi <- readProp "wiIndex" value :: F Number
    return $ WsIndex { wiIndex: wi }

data WebMessage = WmIndex WsIndex
                | WmSong WebSong

-- just returns the first 'read' that succeeds.  
instance webMessageIsForeign :: IsForeign WebMessage where
  read value = 
    (WmSong <$> (read value :: F WebSong))
    <|> (WmIndex <$> (read value :: F WsIndex))

-- in this callback function we process a message from the websocket.
enmessage :: forall e. RefVal WebSong -> CanvasElement -> WS.Message -> Eff (ref :: Ref, 
      canvas :: Canvas, 
      ws :: WS.WebSocket,
      now :: Data.Date.Now,
      dom :: DOM,
      trace :: Trace | e) Unit
enmessage songref canelt msg = do 
  -- trace msg
  con2d <- getContext2D canelt
  candims <- getCanvasDimensions canelt
  let wholerect = { h: candims.height
                  , w: candims.width
                  , x: 0
                  , y: 0 }
  trace "received msg: "
  trace msg
  let wm = readJSON msg :: F WebMessage
  case wm of 
    Right wm -> case wm of 
      WmSong (WebSong ws) -> do
        -- trace "song"
        writeRef songref (WebSong ws)
        drawsong (WebSong ws) 0 canelt
        -- trace (ws.wsName ++ show ws.wsChords) 
        startAnimation canelt (WebSong ws)
        return unit
        {-
        clearRect con2d wholerect 
        strokeText con2d msg 50 100
        -- save con2d   -- doesn't work across multiple calls to enmessage?
        trace (ws.wsName ++ show ws.wsChords) 
        return unit
        -}
      WmIndex (WsIndex wi) -> do
        (WebSong song) <- readRef songref
        -- drawsong (WebSong song) wi.wiIndex canelt
        return unit
        {-
        -- trace "index"
        let tc = { x: 50, y: 200 }
        clearRect con2d { x: tc.x - 20 , y: tc.y - 20, w: 200, h: 50 } 
        strokeText con2d (show wi.wiIndex) tc.x tc.y
        trace (show wi.wiIndex)
        -}
      default -> do 
        trace "message pattern match failed"
    Left _ -> do 
        trace "message read failed"

drawsong :: forall e. WebSong -> Number -> CanvasElement
  -> Eff (canvas :: Canvas, trace :: Trace, dom :: DOM | e) Unit
drawsong (WebSong song) index canelt = do 
  con2d <- getContext2D canelt
  globw <- innerWidth globalWindow
  globh <- innerHeight globalWindow
  doc <- document globalWindow
  bod <- body doc
  mbmain <- getElementById "main" doc 
  case mbmain of 
    Just main -> do 
      mainw <- getClientWidth main 
      bodh <- getOffsetHeight bod 
      odims <- getCanvasDimensions canelt
      let canh = odims.height + globh - bodh 
          canw = mainw
          -- canw = bodw - (globw - bodw) 
      setCanvasDimensions {height: canh, 
                           width: canw } canelt  
      -- get the dims again, just in case they didn't take.
      candims <- getCanvasDimensions canelt
      let wholerect = { h: candims.height
                      , w: candims.width
                      , x: 0
                      , y: 0 }
      clearRect con2d wholerect 
      strokeText con2d (song.wsName) (candims.width * 0.5) 25
      let count = A.length song.wsChords
      case count of 
        0 -> return unit
        _ -> do
          let prev = song.wsChords A.!! (mod (index - 1) count)
              cur = song.wsChords A.!! (mod index count)
              next = song.wsChords A.!! (mod (index + 1) count)
              mod x y = let z = x % y in
                if z < 0 then z + y else z
              y = 100
              toop = Tuple prev (Tuple cur next)
          case toop of 
            (Tuple (Just prev) (Tuple (Just (WebChord cur)) (Just (WebChord next)))) -> do 
              -- strokeText con2d prev (candims.width * 0.25) y
              -- trace $ show $ cur.wcName
              strokeText con2d cur.wcName (candims.width * 0.5) y
              strokeText con2d next.wcName (candims.width * 0.75) y
              return unit 
            _ -> do 
              trace "prev/cur/next failed:"
              -- trace $ "prev: " ++ show prev
              -- trace $ "cur: " ++ show cur
              -- trace $ "next: " ++ show next
              return unit 

-- compute milliseconds per beat
tempoToBeatMs :: Number -> Milliseconds
tempoToBeatMs tempo = 
  if (tempo == 0) 
    then Milliseconds 0
    else  
      -- tempo is Beats per minute.
      -- one minute in milliseconds is...
      let minute = 1000 * 60 in 
        Milliseconds (minute / tempo)

data AniChord = AniChord
  { name :: String
  , time :: Milliseconds 
  }

makeAniChords :: WebSong -> [AniChord]
makeAniChords (WebSong ws) = 
  snd $ foldl accum (Tuple (Milliseconds 0) []) ws.wsChords
  where
    beatms = tempoToBeatMs ws.wsTempo 
    accum :: (Tuple Milliseconds [AniChord]) -> WebChord -> (Tuple Milliseconds [AniChord])
    accum (Tuple time acs) (WebChord wc) = 
      (Tuple (time + (Milliseconds wc.wcDuration) * beatms) 
            (A.snoc acs 
                    (AniChord { name: wc.wcName, time: time })))

startAnimation canelt (WebSong ws) = do 
  begin <- nowEpochMilliseconds
  let anichords = makeAniChords (WebSong ws)
      beatms = tempoToBeatMs ws.wsTempo
      songduration :: Milliseconds
      songduration = foldl 
          (\sum (WebChord wc) -> 
            sum + (Milliseconds wc.wcDuration) * beatms) 
          (Milliseconds 0) 
          ws.wsChords 
  setInterval globalWindow 40 
    (animate canelt songduration begin beatms (Milliseconds 5000) anichords)

msMod :: Milliseconds -> Milliseconds -> Milliseconds 
msMod (Milliseconds l) (Milliseconds r) = 
  Milliseconds (l % r) 

animate :: forall eff. CanvasElement -> Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
animate canelt songduration begin beatms windowms acs = do 
  -- yeah, animate!
  -- get current time.
  con2d <- getContext2D canelt
  globw <- innerWidth globalWindow
  globh <- innerHeight globalWindow
  now <- nowEpochMilliseconds
  let modnow = msMod (now - begin) songduration 
      acnows = (\(AniChord ac) -> Tuple (timeToChord modnow ac.time songduration) (AniChord ac)) <$> acs 
      drawAcs = A.filter (\(Tuple ms ac) -> ms < windowms) acnows
  -- draw the chords that remain after filtering.
  -- trace $ show $ (\(Tuple ttc (AniChord ac)) -> (show ttc) ++ " " ++ show ac.time) <$> acnows
  drawAniChords con2d 25 100 500 60 modnow windowms drawAcs 
  return unit

drawAniChords :: forall eff. Context2D -> Number -> Number -> Number -> Number -> Milliseconds -> Milliseconds -> [(Tuple Milliseconds AniChord)] -> Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
drawAniChords con2d x y xw yw now window acs = do
  let xes :: [Number]
      xes = (\(Tuple ms _) -> x + (xw * ((toNumber ms) / nwindow))) <$> acs
      toNumber = \(Milliseconds ms) -> ms 
      nwindow = toNumber window
      wholerect = { h: yw
                  , w: xw
                  , x: x
                  , y: y - yw }
  save con2d
  rect con2d wholerect
  clip con2d 
  -- trace $ "xes: " ++ (show xes)
  setFillStyle "#8080FF" con2d
  fillPath con2d $ rect con2d wholerect
  -- clearRect con2d wholerect 
  traverse (\(Tuple x (Tuple _ (AniChord ac))) ->  
    strokeText con2d (ac.name) x (y - 10)) (zip xes acs)
  -- unclip
  restore con2d
  return unit

-- how long until we reach the chord?
timeToChord :: Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds
timeToChord modnow chordtime duration = 
  let ct = chordtime - modnow in
  if ct < (Milliseconds 0)
    then ct + duration
    else ct

-- this function is called on page load.
-- registers the onMessage callback.
-- enlode :: forall e. Eff (ref :: Ref, canvas :: Canvas, ws :: WS.WebSocket, trace :: Trace | e) Unit
enlode = do
  trace "enlode"
  doc <- document globalWindow
  myurl <- documentUrl
  let wsurl = replace "https:" "wss:" (replace "http:" "ws:" myurl)
  ws <- WS.mkWebSocket wsurl
  Just canvas <- getCanvasElementById "canvas"
  --songref <- newRef $ WebSong { wsName: "", wsChords: [] }
  songref <- newRef $ WebSong { wsName: "", wsChords: [], wsTempo: 0 }
  WS.onMessage ws (enmessage songref canvas)
  trace "enlode end"

-- boilerplate to get url for websockets.
foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)


