module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
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
import qualified Data.DOM.Simple.Events as E
import Data.DOM.Simple.Types
import Dims
import Math
import Data.Date 
import Data.Foldable
import Data.Time
import Data.Traversable

--  data structures we're receiving from haskell ----
-- main data structure is WebMessage, which could be a websong or a wsindex.

twoPi = 2 * pi

data WebChord = WebChord
  { wcName :: String
  , wcDuration :: Number
  }
 
data WebSong = WebSong
  { wsId :: Number
  , wsName :: String
  , wsChords :: [WebChord]
  , wsTempo :: Number
  }

data WsIndex = WsIndex
  { wiIndex :: Number
  }

data WsStop = WsStop
  { wssId :: Number } 

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
    wi <- readProp "wsId" value
    wn <- readProp "wsName" value
    ch <- readProp "wsChords" value
    tm <- readProp "wsTempo" value
    return $ WebSong  
      { wsId: wi
      , wsName: wn
      , wsChords: ch 
      , wsTempo: tm 
      }

instance wsIndexIsForeign :: IsForeign WsIndex where
  read value = do 
    wi <- readProp "wiIndex" value :: F Number
    return $ WsIndex { wiIndex: wi }

instance wsStopIsForeign :: IsForeign WsStop where
  read value = do 
    wsid <- readProp "wssId" value :: F Number
    return $ WsStop { wssId: wsid }

data WebMessage = WmSong WebSong
                | WmIndex WsIndex
                | WmStop WsStop

-- just returns the first 'read' that succeeds.  
instance webMessageIsForeign :: IsForeign WebMessage where
  read value = 
    (WmSong <$> (read value :: F WebSong))
    <|> (WmIndex <$> (read value :: F WsIndex))
    <|> (WmStop <$> (read value :: F WsStop))

data AniChord = AniChord
  { name :: String
  , time :: Milliseconds 
  , beats :: Number       -- duration of the chord in beats. 
  , onbeat :: Number      -- how many beats until now in the song.
  }

data AniSong = AniSong
  { name :: String
  , tempo :: Number
  , beatms :: Milliseconds
  , anichords :: [AniChord]
  , duration :: Milliseconds
  }

makeAniChords :: WebSong -> [AniChord]
makeAniChords (WebSong ws) = 
  snd $ foldl accum (Tuple 0 []) ws.wsChords
  where
    beatms = tempoToBeatMs ws.wsTempo 
    accum :: (Tuple Number [AniChord]) -> WebChord -> (Tuple Number [AniChord])
    accum (Tuple totebeats acs) (WebChord wc) = 
      (Tuple (totebeats + wc.wcDuration)
            (A.snoc acs 
                    (AniChord { name: wc.wcName, 
                        time: Milliseconds totebeats * beatms, 
                        beats: wc.wcDuration,
                        onbeat: totebeats })))

makeAniSong :: WebSong -> AniSong
makeAniSong (WebSong ws) = 
   let bms = tempoToBeatMs ws.wsTempo in
     AniSong 
     { name: ws.wsName
     , tempo: ws.wsTempo
     , beatms: bms 
     , anichords: makeAniChords (WebSong ws)
     , duration: songduration ws.wsChords bms
     }

songduration :: [WebChord] -> Milliseconds-> Milliseconds
songduration wc beatms = foldl 
    (\sum (WebChord wc) -> 
      sum + (Milliseconds wc.wcDuration) * beatms) 
    (Milliseconds 0) 
    wc
 
-- in this callback function we process a message from the websocket.
enmessage :: forall e. 
  RefVal AniSong -> 
  RefVal (Maybe (Tuple Timeout Timeout)) -> 
  RefVal (Number -> (Tuple Number Number)) -> 
  CanvasElement -> 
  WS.Message -> Eff (ref :: Ref, 
    canvas :: Canvas, 
    ws :: WS.WebSocket,
    now :: Data.Date.Now,
    dom :: DOM,
    trace :: Trace | e) Unit
enmessage songref timeoutref beatlocref canelt msg = do 
  -- trace "received msg: "
  -- trace msg
  let wm = readJSON msg :: F WebMessage
  case wm of 
    Right wm -> case wm of 
      WmSong (WebSong ws) -> do
        trace $ "song loaded: " ++ ws.wsName 
        let as = makeAniSong (WebSong ws)
        writeRef songref as
        doc <- document globalWindow
        wat <- getElementById "songname" doc
        traverse (setTextContent ws.wsName) wat 
        mbt <- readRef timeoutref
        traverse (\(Tuple tmo1 tmo2) -> do 
          clearTimeout globalWindow tmo1 
          clearTimeout globalWindow tmo2)
          mbt
        timeouts <- startAnimation canelt as beatlocref 0
        writeRef timeoutref $ Just timeouts 
        return unit
      WmIndex (WsIndex wi) -> do
        -- trace "index"
        -- trace (show wi.wiIndex)
        (AniSong as) <- readRef songref
        mbt <- readRef timeoutref
        traverse (\(Tuple tmo1 tmo2) -> do 
          clearTimeout globalWindow tmo1 
          clearTimeout globalWindow tmo2)
          mbt
        timeouts <- startAnimation canelt (AniSong as) beatlocref wi.wiIndex 
        writeRef timeoutref $ Just timeouts
        onChordDraw canelt wi.wiIndex (AniSong as)
        return unit
      WmStop (WsStop ws) -> do 
        -- if there's an animation running, stop it.
        mbt <- readRef timeoutref
        traverse (\(Tuple tmo1 tmo2) -> do 
          clearTimeout globalWindow tmo1 
          clearTimeout globalWindow tmo2)
          mbt
        return unit
      default -> do 
        trace "message pattern match failed"
    Left _ -> do 
        trace "message read failed"

sizeCanvasEvt :: forall e. RefVal AniSong -> RefVal (Number -> Tuple Number Number) -> CanvasElement -> DOMEvent ->
  Eff (ref :: Ref, canvas :: Canvas, trace :: Trace, dom :: DOM | e) Unit
sizeCanvasEvt asref beatlocref canelt _ = do 
  -- resize the canvas
  cdims <- sizeCanvas canelt 
  -- make a new beatloc with the new canvas dims.
  (AniSong as) <- readRef asref
  con2d <- getContext2D canelt
  setFont "20px sans-serif" con2d 
  mcw <- maxChordWidth con2d as.anichords
  trace $ "sce: " ++ show cdims.height ++ " " ++ show cdims.width ++ " " ++ show mcw
  let bl = makeBeatLoc (AniSong as) cdims mcw
  writeRef beatlocref bl
  return unit

sizeCanvas :: forall e. CanvasElement -> 
  Eff (canvas :: Canvas, trace :: Trace, dom :: DOM | e) Dimensions
sizeCanvas canelt = do 
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
          cdims = {height: canh, 
                    width: canw } 
      setCanvasDimensions cdims canelt  
      return cdims
    _ -> return { height: 0, width: 0 }

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

makeBeatLoc :: AniSong -> Dimensions -> Number -> (Number -> (Tuple Number Number))
makeBeatLoc (AniSong as) cdims maxchordwidth = 
  let dr = { x: 0, y: 100, w: cdims.width, h: 60 }
      chrect = { x: 0, y: 160, w: cdims.width, h: cdims.height - 160 }
      rowheight = 40
   in
    gridBeatLoc chrect rowheight (maxchordwidth + 15) as.anichords 

startAnimation canelt (AniSong as) beatlocref chordindex = do 
  begin <- nowEpochMilliseconds
  cdims <- getCanvasDimensions canelt
  con2d <- getContext2D canelt
  let curchordtime = maybe (Milliseconds 0) (\(AniChord x) -> x.time) (as.anichords A.!! chordindex) 
      cdh = (cdims.height * 0.5)
      dr = { x: 0, y: 100, w: cdims.width, h: 60 }
      chrect = { x: 0, y: 160, w: cdims.width, h: cdims.height - 160 }
      rowheight = 40
  setFont "20px sans-serif" con2d 
  mcw <- maxChordWidth con2d as.anichords
  writeRef beatlocref $ makeBeatLoc (AniSong as) cdims mcw
  let bms = (\(Milliseconds ms) -> ms) as.beatms
  meh1 <- setInterval globalWindow 40 
    (animate canelt dr as.duration (begin - curchordtime) as.beatms (Milliseconds 5000) as.anichords)
  meh2 <- setInterval globalWindow bms
    (anibeat canelt as.duration (begin - curchordtime) as.beatms beatlocref)
  return $ Tuple meh1 meh2

anibeat :: forall eff. CanvasElement -> Milliseconds -> Milliseconds -> Milliseconds -> RefVal (Number -> (Tuple Number Number)) -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace, ref :: Ref | eff) Unit
anibeat canelt songduration begin beatms beatlocref = do 
  con2d <- getContext2D canelt
  now <- nowEpochMilliseconds
  beatloc <- readRef beatlocref
  -- compute beat number.
  let tobeat :: Milliseconds -> Number
      tobeat nowms = 
        let modnow = msMod (now - begin) songduration 
          in
            floor $ 0.5 + ((\(Milliseconds nowms) (Milliseconds bms) -> nowms / bms) modnow beatms) 
      nowbeat = tobeat now
  -- black dot on prev beat, red dot on nowbeat.
  trace $ "nowbeatloc: " ++ show (beatloc nowbeat)
  setFillStyle "#000000" con2d
  drawbeat beatloc con2d (nowbeat - 1)
  setFillStyle "#FF0000" con2d
  drawbeat beatloc con2d nowbeat
  return unit

drawbeat :: forall eff. (Number -> (Tuple Number Number)) -> Context2D -> Number -> Eff (canvas :: Canvas | eff) Unit   
drawbeat beatloc con2d beat = do 
  let toop = beatloc beat
      a = { x: (fst toop) + 5, y: (snd toop), r: 5, start: 0, end: twoPi }
  beginPath con2d
  arc con2d a
  fill con2d
  return unit
 
msMod :: Milliseconds -> Milliseconds -> Milliseconds 
msMod (Milliseconds l) (Milliseconds r) = 
  Milliseconds (l % r) 

animate :: forall eff. CanvasElement -> Rectangle -> Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
animate canelt drawrect songduration begin beatms windowms acs = do 
  -- get current time.
  con2d <- getContext2D canelt
  now <- nowEpochMilliseconds
  let modnow = msMod (now - begin) songduration 
      acnows = (\(AniChord ac) -> Tuple (timeToChord modnow ac.time songduration) (AniChord ac)) <$> acs 
      drawAcs = A.filter (\(Tuple ms ac) -> ms < windowms) acnows
  -- draw the chords that remain after filtering.
  setFont "20px sans-serif" con2d 
  cdims <- getCanvasDimensions canelt
  drawAniChords con2d drawrect modnow windowms drawAcs 
  drawAniDots con2d drawrect begin beatms windowms now
  return unit

drawAniChords :: forall eff. Context2D -> Rectangle -> Milliseconds -> Milliseconds -> [(Tuple Milliseconds AniChord)] -> Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
drawAniChords con2d { x: x, y: y, w: xw, h: yw } now window acs = do
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
  setFillStyle "#8080FF" con2d
  fillPath con2d $ rect con2d wholerect
  setFillStyle "#000000" con2d
  traverse (\(Tuple x (Tuple _ (AniChord ac))) -> do
    fillText con2d (ac.name) x (y - 10)
    let a = { x: x, y: y - 35, r: 10, start: 0, end: twoPi }
    beginPath con2d
    arc con2d a
    fill con2d
    )
    (zip xes acs)
  -- unclip
  restore con2d
  return unit

drawAniDots :: forall eff. Context2D -> Rectangle 
-> Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds 
-> Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
drawAniDots con2d { x: x, y: y, w: xw, h: yw } (Milliseconds begin) (Milliseconds beatms) (Milliseconds windowms) (Milliseconds now) = do
  let beatsofar = (now - begin) / beatms
      firstbeat = (ceil beatsofar - beatsofar) * beatms
      numbeats = floor ((windowms - firstbeat) / beatms)
      b2x = xw / windowms
      dots = A.range 0 numbeats
      dotsms = (\i -> (i * beatms + firstbeat) * b2x) <$> dots
  setFillStyle "#000000" con2d
  traverse (\dtx -> do 
      let a = { x: dtx, y: y - 10, r: 5, start: 0, end: twoPi }
      beginPath con2d
      arc con2d a
      fill con2d
      )
    dotsms
  return unit
  
onChordDraw :: forall eff. CanvasElement -> Number -> AniSong -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
onChordDraw canelt curchordidx (AniSong as) = do 
  con2d <- getContext2D canelt
  cdims <- getCanvasDimensions canelt
  setFont "20px sans-serif" con2d 
  mcw <- maxChordWidth con2d as.anichords
  let mbcurchord = as.anichords A.!! curchordidx
  -- draw current chord.
  traverse (\(AniChord ac) -> do 
    tm <- measureText con2d ac.name
    clearRect con2d { x: 5, y: 5, w: mcw, h: 25 }
    fillText con2d (ac.name) 5 25) mbcurchord 
  trace $ "onchorddraw: " ++ show cdims.height ++ " " ++ show cdims.width ++ " " ++ show mcw
  let chrect = { x: 0, y: 160, w: cdims.width, h: cdims.height - 160 }
      beattot = foldr (\(AniChord ac) sum -> sum + ac.beats) 0 as.anichords
      beatloc = makeBeatLoc (AniSong as) cdims mcw
  clearRect con2d chrect
  drawMsChordGrid con2d beatloc beattot curchordidx as.anichords
  setFillStyle "#FF0000" con2d
  traverse (\(AniChord ac) -> drawbeat beatloc con2d ac.onbeat) mbcurchord
  return unit
  
maxChordWidth :: forall eff. Context2D -> [AniChord] -> Eff (canvas :: Canvas, trace :: Trace | eff) Number
maxChordWidth con2d chords = do 
  widths <- traverse (\(AniChord ac) -> do
    tm <- measureText con2d ac.name
    return tm.width) 
    chords
  return $ foldr (\a b -> if a > b then a else b) 0 widths 

posmod x a =  
  let b = x % a in 
  if (b < 0)
   then
     b + a
   else
     b

gridBeatLoc :: Rectangle -> Number -> Number -> [AniChord] -> (Number -> (Tuple Number Number))
gridBeatLoc { x: x, y: y, w: xw, h: yw } rowheight maxchordwidth acs = 
  -- how many beats are we talking?
  let beattot = foldr (\(AniChord ac) sum -> sum + ac.beats) 0 acs
      minbeats = foldr (\(AniChord ac) minimum -> min minimum ac.beats) beattot acs
      beatwidth = maxchordwidth / minbeats
      beatrowwidth = xw
      numperrow = floor (beatrowwidth / beatwidth)
      rows = ceil (beattot / numperrow)
      drawloc numperrow rowheight hspace index = 
        let inum = (posmod index beattot) / numperrow
            row = floor inum
            col = (inum - row) * numperrow
         in 
          Tuple (x + col * hspace) (y + 20 + row * rowheight) in 
  drawloc numperrow rowheight beatwidth

drawMsChordGrid :: forall eff. Context2D -> (Number -> (Tuple Number Number)) -> Number -> Number -> [AniChord] -> Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
drawMsChordGrid con2d beatloc beattot curchordidx acs = do
  -- how many beats are we talking?
  let dexes = A.range 0 (A.length acs - 1)
      dexedacs = zip dexes acs
  setFillStyle "#000000" con2d
  -- draw the chords.
  traverse (\(Tuple idx (AniChord ac)) -> do
    let toop = beatloc ac.onbeat
    setLineWidth 2 con2d
    beginPath con2d 
    moveTo con2d (fst toop) (snd toop)
    lineTo con2d (fst toop) ((snd toop) - 25)
    stroke con2d
    if idx == curchordidx
      then do 
        setFillStyle "#FF0000" con2d
        fillText con2d (ac.name) (fst toop) ((snd toop) - 10)
        setFillStyle "#000000" con2d
      else do 
        fillText con2d (ac.name) (fst toop) ((snd toop) - 10)
    )
    dexedacs
  -- draw the beats
  traverse (\xidx -> do 
    let toop = beatloc xidx
    let a = { x: (fst toop) + 5, y: (snd toop), r: 5, start: 0, end: twoPi }
    beginPath con2d
    arc con2d a
    fill con2d)
    (A.range 0 (beattot - 1))
  return unit 

--- how long until we reach the chord?
timeToChord :: Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds
timeToChord modnow chordtime duration = 
  let ct = chordtime - modnow in
  if ct < (Milliseconds 0)
    then ct + duration
    else ct

-- this function is called on page load.
-- registers the onMessage callback.
enlode :: forall e. Eff (now :: Data.Date.Now, dom :: DOM, ref :: Ref, canvas :: Canvas, ws :: WS.WebSocket, trace :: Trace | e) Unit
--  -> Eff (canvas :: Canvas, trace :: Trace, dom :: DOM | e) Unit
enlode = do
  doc <- document globalWindow
  myurl <- documentUrl
  Just canelt <- getCanvasElementById "canvas"
  cdims <- getCanvasDimensions canelt
  con2d <- getContext2D canelt
  let wsurl = replace "https:" "wss:" (replace "http:" "ws:" myurl)
      anisawng = makeAniSong $ WebSong { wsId: 0, wsName: "", wsChords: [], wsTempo: 0 }
      as = (\(AniSong a) -> a) anisawng
  ws <- WS.mkWebSocket wsurl
  mcw <- maxChordWidth con2d as.anichords
  --songref <- newRef $ WebSong { wsName: "", wsChords: [] }
  songref <- newRef $ anisawng 
  ref <- newRef $ Nothing
  beatlocref <- newRef $ makeBeatLoc (AniSong as) cdims mcw
  WS.onMessage ws (enmessage songref ref beatlocref canelt)
  sizeCanvas canelt
  E.addUIEventListener E.ResizeEvent (sizeCanvasEvt songref beatlocref canelt) globalWindow
  return unit

-- boilerplate to get url for websockets.
foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)


