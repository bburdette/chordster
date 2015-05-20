module Main where

import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import Data.Maybe
import Data.Foreign
import Data.Foreign.Class
import Data.String
import Debug.Trace
import DOM
import qualified WebSocket as WS

import Data.DOM.Simple.Window

newtype WebSong = WebSong
  { wsName :: String
--  , wsChords :: [String] 
  }

{-
instance webSongIsForeign :: IsForeign WebSong where
  read value = do 
    wn <- readProp "wsName" value
--    ch <- readProp "wsChords" value
    return $ WebSong  
      { wsName = wn
--      , wsChords = ch
      }
-}

foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)

enmessage :: forall e. CanvasElement -> WS.Message 
  -> Eff (canvas :: Canvas, ws :: WS.WebSocket, trace :: Trace | e) Unit
enmessage canelt msg = do 
  trace (show msg)
  con2d <- getContext2D canelt
  candims <- getCanvasDimensions canelt
  let wholerect = { h: candims.height
                  , w: candims.width
                  , x: 0
                  , y: 0 }
  clearRect con2d wholerect 
  strokeText con2d (show msg) 50 100
  return unit

enlode = do
  trace "enlode"
  doc <- document globalWindow
  myurl <- documentUrl
  let wsurl = replace "https:" "wss:" (replace "http:" "ws:" myurl)
  ws <- WS.mkWebSocket wsurl
  Just canvas <- getCanvasElementById "canvas"
  WS.onMessage ws (enmessage canvas)
  trace "enlode end"
  -- unsafeAddEventListener "submit" (inputsubbed input ws) form
  -- docTitle <- title doc
  -- trace docTitle

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
