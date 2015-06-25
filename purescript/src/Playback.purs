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

-- in this callback function we process a message from the websocket.
enmessage :: forall e. RefVal WebSong -> RefVal (Maybe (Tuple Timeout Timeout)) -> CanvasElement -> WS.Message -> Eff (ref :: Ref, 
      canvas :: Canvas, 
      ws :: WS.WebSocket,
      now :: Data.Date.Now,
      dom :: DOM,
      trace :: Trace | e) Unit
enmessage songref timeoutref canelt msg = do 
  -- trace "received msg: "
  -- trace msg
  con2d <- getContext2D canelt
  candims <- getCanvasDimensions canelt
  let wholerect = { h: candims.height
                  , w: candims.width
                  , x: 0
                  , y: 0 }
  let wm = readJSON msg :: F WebMessage
  case wm of 
    Right wm -> case wm of 
      WmSong (WebSong ws) -> do
        trace "song"
        trace ws.wsName 
        writeRef songref (WebSong ws)
        doc <- document globalWindow
        wat <- getElementById "songname" doc
        traverse (setTextContent ws.wsName) wat 
        mbt <- readRef timeoutref
        traverse (\(Tuple tmo1 tmo2) -> do 
          clearTimeout globalWindow tmo1 
          clearTimeout globalWindow tmo2)
          mbt
        timeouts <- startAnimation canelt (WebSong ws) 0
        writeRef timeoutref $ Just timeouts 
        return unit
      WmIndex (WsIndex wi) -> do
        -- trace "index"
        -- trace (show wi.wiIndex)
        (WebSong ws) <- readRef songref
        mbt <- readRef timeoutref
        traverse (\(Tuple tmo1 tmo2) -> do 
          clearTimeout globalWindow tmo1 
          clearTimeout globalWindow tmo2)
          mbt
        timeouts <- startAnimation canelt (WebSong ws) wi.wiIndex 
        writeRef timeoutref $ Just timeouts
        let anichords = makeAniChords (WebSong ws)
        onChordDraw canelt wi.wiIndex anichords
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

sizeCanvasEvt :: forall e. CanvasElement -> DOMEvent ->
  Eff (canvas :: Canvas, trace :: Trace, dom :: DOM | e) Unit
sizeCanvasEvt canelt _ = sizeCanvas canelt 

sizeCanvas :: forall e. CanvasElement -> 
  Eff (canvas :: Canvas, trace :: Trace, dom :: DOM | e) Unit
sizeCanvas canelt = do 
  -- con2d <- getContext2D canelt
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
      return unit
    _ -> return unit

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

startAnimation canelt (WebSong ws) chordindex = do 
  begin <- nowEpochMilliseconds
  cdims <- getCanvasDimensions canelt
  con2d <- getContext2D canelt
  let anichords = makeAniChords (WebSong ws)
      beatms = tempoToBeatMs ws.wsTempo
      songduration :: Milliseconds
      songduration = foldl 
          (\sum (WebChord wc) -> 
            sum + (Milliseconds wc.wcDuration) * beatms) 
          (Milliseconds 0) 
          ws.wsChords
      curchordtime = maybe (Milliseconds 0) (\(AniChord x) -> x.time) (anichords A.!! chordindex) 
      cdh = (cdims.height * 0.5)
      chrect = { x: 0, y: cdh, w: cdims.width, h: cdh }
      rowheight = 30
  mcw <- maxChordWidth con2d anichords
  let beatloc = gridBeatLoc chrect rowheight mcw anichords 
      bms = (\(Milliseconds ms) -> ms) beatms
  meh1 <- setInterval globalWindow 40 
    (animate canelt songduration (begin - curchordtime) beatms (Milliseconds 5000) anichords)
  meh2 <- setInterval globalWindow bms
    (anibeat canelt songduration (begin - curchordtime) beatms beatloc)
  return $ Tuple meh1 meh2

anibeat :: forall eff. CanvasElement -> Milliseconds -> Milliseconds -> Milliseconds -> (Number -> (Tuple Number Number)) -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
anibeat canelt songduration begin beatms beatloc = do 
  con2d <- getContext2D canelt
  now <- nowEpochMilliseconds
  -- compute current and previous beat number.
  let tobeat :: Milliseconds -> Number
      tobeat nowms = 
        let modnow = msMod (now - begin) songduration 
          in
            floor $ 0.5 + ((\(Milliseconds nowms) (Milliseconds bms) -> nowms / bms) modnow beatms) 
      nowbeat = tobeat now
      prevbeat = tobeat (now - beatms)
  -- black dot on prev beat, red dot on nowbeat.
  setFillStyle "#00FF00" con2d
  drawbeat beatloc con2d prevbeat
  setFillStyle "#FF0000" con2d
  drawbeat beatloc con2d nowbeat
  return unit

drawbeat :: forall eff. (Number -> (Tuple Number Number)) -> Context2D -> Number -> Eff (canvas :: Canvas | eff) Unit   
drawbeat beatloc con2d beat = do 
  let toop = beatloc beat
      a = { x: (fst toop), y: (snd toop) - 35, r: 5, start: 0, end: twoPi }
  beginPath con2d
  arc con2d a
  fill con2d
  return unit
 
msMod :: Milliseconds -> Milliseconds -> Milliseconds 
msMod (Milliseconds l) (Milliseconds r) = 
  Milliseconds (l % r) 

animate :: forall eff. CanvasElement -> Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
animate canelt songduration begin beatms windowms acs = do 
  -- get current time.
  con2d <- getContext2D canelt
  now <- nowEpochMilliseconds
  let modnow = msMod (now - begin) songduration 
      acnows = (\(AniChord ac) -> Tuple (timeToChord modnow ac.time songduration) (AniChord ac)) <$> acs 
      drawAcs = A.filter (\(Tuple ms ac) -> ms < windowms) acnows
  -- draw the chords that remain after filtering.
  -- trace $ show $ (\(Tuple ttc (AniChord ac)) -> (show ttc) ++ " " ++ show ac.time) <$> acnows
  -- zefont <- font con2d
  -- trace $ "font: " ++ show zefont
  setFont "20px sans-serif" con2d 
  cdims <- getCanvasDimensions canelt
  let dr = { x: 0, y: 100, w: cdims.width, h: 60 }
  drawAniChords con2d dr modnow windowms drawAcs 
  -- drawAniChords con2d 0 100 cdims.width 60 modnow windowms drawAcs 
  drawAniDots con2d dr begin beatms windowms now
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
  -- trace $ "xes: " ++ (show xes)
  setFillStyle "#8080FF" con2d
  fillPath con2d $ rect con2d wholerect
  -- clearRect con2d wholerect 
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
  

onChordDraw :: forall eff. CanvasElement -> Number -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
onChordDraw canelt curchordidx acs = do 
  con2d <- getContext2D canelt
  cdims <- getCanvasDimensions canelt
  setFont "20px sans-serif" con2d 
  let mbcurchord = acs A.!! curchordidx
  mcw <- maxChordWidth con2d acs
  -- draw current chord.
  traverse (\(AniChord ac) -> do 
    tm <- measureText con2d ac.name
    clearRect con2d { x: 25, y: 5, w: mcw, h: 25 }
    fillText con2d (ac.name) 25 25) mbcurchord 
  let cdh = (cdims.height * 0.5)
      chrect = { x: 0, y: cdh, w: cdims.width, h: cdh }
      rowheight = 30
      beattot = foldr (\(AniChord ac) sum -> sum + ac.beats) 0 acs
      beatloc = gridBeatLoc chrect rowheight mcw acs 
  clearRect con2d chrect
  drawMsChordGrid con2d beatloc beattot curchordidx acs
  traverse (\(AniChord ac) -> drawbeat beatloc con2d ac.onbeat) mbcurchord
  return unit
  

maxChordWidth :: forall eff. Context2D -> [AniChord] -> Eff (canvas :: Canvas, trace :: Trace | eff) Number
maxChordWidth con2d chords = do 
  widths <- traverse (\(AniChord ac) -> do
    tm <- measureText con2d ac.name
    return tm.width) 
    chords
  return $ foldr (\a b -> if a > b then a else b) 0 widths 

gridBeatLoc :: Rectangle -> Number -> Number -> [AniChord] -> (Number -> (Tuple Number Number))
gridBeatLoc { x: x, y: y, w: xw, h: yw } rowheight maxchordwidth acs = 
  -- how many beats are we talking?
  let beattot = foldr (\(AniChord ac) sum -> sum + ac.beats) 0 acs
      minbeats = foldr (\(AniChord ac) minimum -> min minimum ac.beats) beattot acs
      beatwidth = maxchordwidth / minbeats
      beatrowwidth = xw - maxchordwidth
      numperrow = floor (beatrowwidth / beatwidth)
      rows = ceil (beattot / numperrow)
      -- rowheight = yw / rows
      -- hspace = xw / numperrow
      drawloc numperrow rowheight hspace index = 
        let inum = index / numperrow
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
    if idx == curchordidx
      then do 
        setFillStyle "#FF0000" con2d
        fillText con2d (ac.name) (fst toop) (snd toop)
        setFillStyle "#000000" con2d
      else do 
        fillText con2d (ac.name) (fst toop) (snd toop)
    )
    dexedacs
  -- draw the beats
  traverse (\xidx -> do 
    let toop = beatloc xidx
    let a = { x: (fst toop), y: (snd toop), r: 5, start: 0, end: twoPi }
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
  trace "enlode"
  doc <- document globalWindow
  myurl <- documentUrl
  let wsurl = replace "https:" "wss:" (replace "http:" "ws:" myurl)
  ws <- WS.mkWebSocket wsurl
  Just canvas <- getCanvasElementById "canvas"
  --songref <- newRef $ WebSong { wsName: "", wsChords: [] }
  songref <- newRef $ WebSong { wsId: 0, wsName: "", wsChords: [], wsTempo: 0 }
  ref <- newRef $ Nothing
  WS.onMessage ws (enmessage songref ref canvas)
  sizeCanvas canvas
  E.addUIEventListener E.ResizeEvent (sizeCanvasEvt canvas) globalWindow
  trace "enlode end"
  return unit

-- boilerplate to get url for websockets.
foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)


