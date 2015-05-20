module Main where

import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import DOM
import Debug.Trace
import qualified WebSocket as WS
import Data.String

import Data.DOM.Simple.Unsafe.Events
import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Data.DOM.Simple.Document
import Data.DOM.Simple.Window
import Data.DOM.Simple.Encode
import Data.DOM.Simple.Ajax
import Data.DOM.Simple.Events
import Data.DOM.Simple.Navigator
import qualified Data.DOM.Simple.NodeList as NL

foreign import documentUrl
  """
  function documentUrl() {
    return document.URL;
  }""" :: forall eff . (Eff (dom :: DOM | eff) String)

foreign import enmessage_impl 
  """
  function enmessage_impl (elt) {
    return function (msg) {
      return function () {
        var p = document.createElement("p");
        p.appendChild(document.createTextNode(msg));
        elt.appendChild(p);
        }
      }
    }
  """ :: forall e. HTMLElement -> WS.Message -> Eff (ws :: WS.WebSocket, trace :: Trace | e) Unit

foreign import setOnClick
  """
  function setOnClick(elt) { 
    return function(evt) { 
      elt.onClick = evt;
    }
  }
  """ :: forall eff a b. a -> b -> Eff (dom :: DOM | eff) Unit

enlode = do
  doc <- document globalWindow
  Just output <- getElementById "output" doc
  Just form <- getElementById "form" doc
  Just input <- getElementById "input" doc
  wha <- documentUrl
  let wswha = replace "https:" "wss:" (replace "http:" "ws:" wha)
  ws <- WS.mkWebSocket wswha
  WS.onMessage ws (enmessage output)
  unsafeAddEventListener "submit" (inputsubbed input ws) form
  docTitle <- title doc
  trace docTitle

enmessage :: forall e. HTMLElement -> WS.Message -> Eff (ws :: WS.WebSocket, trace :: Trace | e) Unit
enmessage elt msg = do 
  enmessage_impl elt msg

inputsubbed :: forall eff. HTMLElement -> WS.Socket -> DOMEvent -> 
  Eff (dom :: DOM, ws :: WS.WebSocket, trace :: Trace | eff) Unit
inputsubbed input conn evt = do 
  txmsg <- value input
  WS.send conn txmsg 
  setValue "" input
  preventDefault evt


