module Main where

import Data.Maybe
import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import DOM
import Debug.Trace

import Data.DOM.Simple.Ajax
import Data.DOM.Simple.Document
import Data.DOM.Simple.Element
import Data.DOM.Simple.Encode
import Data.DOM.Simple.Events hiding (read)
import Data.DOM.Simple.Types
import Data.DOM.Simple.Window


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


main = do
  mbcanvas <- getCanvasElementById "canvas"
  case mbcanvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas
      setFillStyle "#0000FF" ctx
      fillPath ctx $ rect ctx 
        { x: 250
        , y: 250
        , w: 100
        , h: 100
        }

