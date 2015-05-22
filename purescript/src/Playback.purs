module Main where

import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.String
import Data.Either
import Debug.Trace
import Control.Alt
import DOM
import qualified WebSocket as WS

import Data.DOM.Simple.Window

--  data structures we're receiving from haskell ----
-- main data structure is WebMessage, which could be a websong or a wsindex.

newtype WebSong = WebSong
  { wsName :: String
  , wsChords :: [String] 
  }

instance webSongIsForeign :: IsForeign WebSong where
  read value = do 
    wn <- readProp "wsName" value
    ch <- readProp "wsChords" value
    return $ WebSong  
      { wsName: wn
      , wsChords: ch 
      }

newtype WsIndex = WsIndex
  { wiIndex :: Number
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
enmessage :: forall e. CanvasElement -> WS.Message 
  -> Eff (canvas :: Canvas, ws :: WS.WebSocket, trace :: Trace | e) Unit
enmessage canelt msg = do 
  trace msg
  con2d <- getContext2D canelt
  candims <- getCanvasDimensions canelt
  let wholerect = { h: candims.height
                  , w: candims.width
                  , x: 0
                  , y: 0 }
  clearRect con2d wholerect 
  strokeText con2d msg 50 100
  let wm = readJSON msg :: F WebMessage
  case wm of 
    Right wm -> case wm of 
      WmIndex (WsIndex wi) -> do
        trace "index" 
        trace (show wi.wiIndex)
      WmSong (WebSong ws) -> do
        trace "song" 
        trace (show ws.wsName) 
    Left _ -> do 
        trace "message read failed"

-- this function is called on page load.
-- registers the onMessage callback.
enlode = do
  trace "enlode"
  doc <- document globalWindow
  myurl <- documentUrl
  let wsurl = replace "https:" "wss:" (replace "http:" "ws:" myurl)
  ws <- WS.mkWebSocket wsurl
  Just canvas <- getCanvasElementById "canvas"
  WS.onMessage ws (enmessage canvas)
  trace "enlode end"

-- boilerplate to get url for websockets.
foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)

main = do
  mbcanvas <- getCanvasElementById "canvas"
  case mbcanvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas

      setFillStyle "#0000FF" ctx

      fillPath ctx $ rect ctx 
        { x: 250
        , y: 150
        , w: 100
        , h: 100
        }

meh = do
  mbcanvas <- getCanvasElementById "canvas"
  case mbcanvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas

      setFillStyle "#FF00FF" ctx

      fillPath ctx $ rect ctx 
        { x: 150
        , y: 150
        , w: 300
        , h: 50
        }
