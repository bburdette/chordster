module Main where

import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import DOM
import Debug.Trace
import qualified WebSocket as WS

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

foreign import unsafeGetUrl
  """
  function unsafeGetUrl(doc) {
    return function () {
      return doc.URL;
    };
  }""" :: forall eff a. a -> (Eff (dom :: DOM | eff) String)

enlode = do
  trace "meh"
  doc <- document globalWindow
  trace (show doc)
  -- Just wut <- getElementById "notoutput" doc
  -- trace "afterwut"
  mboutput <- getElementById "output" doc
  mbform <- getElementById "form" doc
  mbinput <- getElementById "input" doc
  wha <- documentUrl
  wha <- unsafeGetUrl doc
  let blah = "blahh" ++ wha
  trace blah
  docTitle <- title doc
  trace docTitle
       
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

{-
maign :: forall eff. Eff (dom :: DOM, trace :: Trace  | eff) Unit
maign = do
  d <- document globalWindow
  addKeyboardEventListener KeypressEvent keypressEventHandler d
-}

wat :: forall eff. Eff (dom :: DOM | eff) Unit
wat = do
  d <- document globalWindow
  mboutput <- getElementById "output" d
  case mboutput of 
    Just output -> do
      setAttribute "meh" "blah" output

wut :: forall eff. Eff (dom :: DOM | eff) String
wut = do
  d <- document globalWindow
  mboutput <- getElementById "output" d
  case mboutput of 
    Just output -> do
      setAttribute "meh" "blah" output
      return "string"
    Nothing -> return "string"



