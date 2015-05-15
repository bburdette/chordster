module Main where

import Control.Monad.Eff
import Graphics.Canvas hiding (translate)
import Data.Maybe

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

enlode = do
  mbcanvas <- getCanvasElementById "canvas"
  case mbcanvas of 
    Just canvas -> do 
      ctx <- getContext2D canvas

      setFillStyle "#0000FF" ctx

      fillPath ctx $ rect ctx 
        { x: 25
        , y: 70
        , w: 100
        , h: 10 
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
