module Main where

import Math
import Debug.Trace
import Control.Monad.ST

import Control.Monad.Eff
import Control.Monad.Eff.Random
-- import Control.Monad.Eff.Exception

data Point = Point {x :: Number, y :: Number}

generateCoord :: forall eff. Eff (random :: Random | eff) Number
generateCoord = randomRange (-1) 1

point :: Number -> Number -> Point
point x y = Point {x: x, y: y}

randomPoint :: forall eff. Eff (random :: Random | eff) Point
randomPoint = point <$> generateCoord
                    <*> generateCoord

inUnitCircle :: Point -> Boolean
inUnitCircle (Point {x = x, y = y}) = (sqrt (x * x + y * y)) < 1


--approximatePi :: Number -> Number
approximatePi :: forall eff. Number -> Eff (random :: Random | eff) Number
approximatePi n = runST (do
        ref <- newSTRef {x: 0}
        forE 0 n $ \i -> do
                inCircle <- inUnitCircle <$> randomPoint
                modifySTRef ref (\o -> if inCircle then {x: o.x + 1} else {x: o.x}) 
                return unit
        final <- readSTRef ref
        return final.x)

main = do 
  trace "blah"
  trace "blah2"
  answer <- approximatePi 100
  trace "blah3"
  trace $ show answer
  trace "blah4"
