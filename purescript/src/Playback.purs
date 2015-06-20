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

-- in this callback function we process a message from the websocket.
enmessage :: forall e. RefVal WebSong -> RefVal (Maybe Timeout) -> CanvasElement -> WS.Message -> Eff (ref :: Ref, 
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
        traverse (clearTimeout globalWindow) mbt
        timeout <- startAnimation canelt (WebSong ws) 0
        writeRef timeoutref $ Just timeout 
        return unit
      WmIndex (WsIndex wi) -> do
        -- trace "index"
        -- trace (show wi.wiIndex)
        (WebSong ws) <- readRef songref
        mbt <- readRef timeoutref
        traverse (clearTimeout globalWindow) mbt
        timeout <- startAnimation canelt (WebSong ws) wi.wiIndex 
        writeRef timeoutref $ Just timeout 
        return unit
      WmStop (WsStop ws) -> do 
        -- if there's an animation running, stop it.
        mbt <- readRef timeoutref
        traverse (clearTimeout globalWindow) mbt
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

startAnimation canelt (WebSong ws) chordindex = do 
  begin <- nowEpochMilliseconds
  let anichords = makeAniChords (WebSong ws)
      beatms = tempoToBeatMs ws.wsTempo
      songduration :: Milliseconds
      songduration = foldl 
          (\sum (WebChord wc) -> 
            sum + (Milliseconds wc.wcDuration) * beatms) 
          (Milliseconds 0) 
          ws.wsChords
      curchordtime = maybe (Milliseconds 0) (\(AniChord x) -> x.time) (anichords A.!! chordindex) 
  setInterval globalWindow 40 
    (animate canelt songduration (begin - curchordtime) beatms (Milliseconds 5000) anichords)

msMod :: Milliseconds -> Milliseconds -> Milliseconds 
msMod (Milliseconds l) (Milliseconds r) = 
  Milliseconds (l % r) 

animate :: forall eff. CanvasElement -> Milliseconds -> Milliseconds -> Milliseconds -> Milliseconds -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
animate canelt songduration begin beatms windowms acs = do 
  -- yeah, animate!
  -- get current time.
  con2d <- getContext2D canelt
  now <- nowEpochMilliseconds
  let modnow = msMod (now - begin) songduration 
      acnows = (\(AniChord ac) -> Tuple (timeToChord modnow ac.time songduration) (AniChord ac)) <$> acs 
      drawAcs = A.filter (\(Tuple ms ac) -> ms < windowms) acnows
      mbcurchord = (\x -> foldr tupcomp x acnows) <$> A.head acnows
      tupcomp (Tuple msl acl) (Tuple msr acr) = 
        if (msl > msr) then (Tuple msl acl) else (Tuple msr acr)
  -- draw the chords that remain after filtering.
  -- trace $ show $ (\(Tuple ttc (AniChord ac)) -> (show ttc) ++ " " ++ show ac.time) <$> acnows
  -- zefont <- font con2d
  -- trace $ "font: " ++ show zefont
  setFont "20px sans-serif" con2d 
  cdims <- getCanvasDimensions canelt
  clearRect con2d { x: 0, y: 0, w: cdims.width, h: cdims.height }
  traverse (\(Tuple ms (AniChord ac)) -> do 
    fillText con2d (ac.name) 25 25) mbcurchord 
  drawAniChords con2d 0 100 cdims.width 60 modnow windowms drawAcs 
  drawChordGrid con2d 0 (cdims.height * 0.5) cdims.width (cdims.height * 0.5) (drawAcs A.!! 0) drawAcs 
  return unit

{-
onChordDraw :: forall eff. CanvasElement -> Number -> [AniChord] -> 
  Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
onChordDraw canelt curchordidx acs = do 
  con2d <- getContext2D canelt
  cdims <- getCanvasDimensions canelt
  let mbcurchord = acs A.!! curchordidx
  traverse (\(Tuple ms (AniChord ac)) -> do 
    fillText con2d (ac.name) 25 25) mbcurchord 
  drawChordGrid con2d 0 (cdims.height * 0.5) cdims.width (cdims.height * 0.5) mbcurchord acs
-}

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
  setFillStyle "#000000" con2d
  traverse (\(Tuple x (Tuple _ (AniChord ac))) ->  
    fillText con2d (ac.name) x (y - 10)) (zip xes acs)
  -- unclip
  restore con2d
  return unit

drawChordGrid :: forall eff. Context2D -> Number -> Number -> Number -> Number -> Maybe (Tuple Milliseconds AniChord) -> [(Tuple Milliseconds AniChord)] -> Eff (now :: Data.Date.Now, dom :: DOM, canvas :: Canvas, trace :: Trace | eff) Unit
drawChordGrid con2d x y xw yw (Just (Tuple ms (AniChord curchord))) acs = do
  -- how many chords are we talking?
  let count = A.length acs
  -- what's the size of t 
  tm <- measureText con2d curchord.name
  --  
  let numperrow = floor (xw / tm.width)
      rows = ceil (count / numperrow)
      rowheight = yw / rows
      hspace = xw / numperrow
      drawloc hspace rowheight index = 
        let inum = index / numperrow
            row = floor inum
            col = (inum - row) * numperrow
         in 
          Tuple (x + col * hspace) (y + row * rowheight)
      dexes = A.range 0 (A.length acs - 1)
      dexedacs = zip dexes acs
  -- trace $ "numperrow: " ++ show numperrow
  -- trace $ "rows: " ++ show rows
  -- trace $ "rowheight: " ++ show rowheight
  -- trace $ "hspace: " ++ show hspace
  -- setFont (show rowheight ++ "px sans-serif") con2d 
  setFillStyle "#000000" con2d
  traverse (\(Tuple xidx (Tuple _ (AniChord ac))) -> do
    let toop = drawloc hspace rowheight xidx
    fillText con2d (ac.name) (fst toop) (snd toop)
    -- trace $ "blah x " ++ (show (fst toop)) ++ 
    --   " y " ++ (show (snd toop)))
    )
    dexedacs
  return unit 
drawChordGrid con2d x y xw yw Nothing acs = return unit

-- how long until we reach the chord?
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


