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
  trace wha
  trace wswha
  ws <- WS.mkWebSocket wswha
  WS.onMessage ws enmessage
  -- setOnClick input (inputclick input ws)
  -- unsafeAddEventListener "submit" insub form
  -- form.addEventListener "submit" ftn
  unsafeAddEventListener "submit" (inputsubbed input ws) form
  -- setOnClick input enclick 
  docTitle <- title doc
  trace docTitle

enmessage :: forall e. WS.Message -> Eff (ws :: WS.WebSocket, trace :: Trace | e) Unit
enmessage msg = do 
--      var p = document.createElement("p");
--      p.appendChild(document.createTextNode(e.data));
--      output.appendChild(p);
  trace "enmessage with sock"
  trace $ show msg

inputsubbed :: forall eff. HTMLElement -> WS.Socket -> DOMEvent -> 
  Eff (dom :: DOM, ws :: WS.WebSocket, trace :: Trace | eff) Unit
inputsubbed input conn evt = do 
  txmsg <- value input
  WS.send conn txmsg 
  setValue "" input
  preventDefault evt

{-
  var url = document.URL,
      output = document.getElementById("output"),
      form = document.getElementById("form"),
      input = document.getElementById("input"),
      conn;

  url = url.replace("http:", "ws:").replace("https:", "wss:");
  conn = new WebSocket(url);

  conn.onmessage = function(e) {
      var p = document.createElement("p");
      p.appendChild(document.createTextNode(e.data));
      output.appendChild(p);
  };

  form.addEventListener("submit", function(e){
      conn.send(input.value);
      input.value = "";
      e.preventDefault();
  });
-}


