module Dims where

import Control.Monad.Eff
import DOM
import Data.DOM.Simple.Types
import Data.DOM.Simple.Element

foreign import getClientWidth
  "function getClientWidth(htmlelt){\
  \  return function(){\
  \    return htmlelt.clientWidth;\ 
  \  };\
  \};" :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number

foreign import getClientHeight
  "function getClientHeight(htmlelt){\
  \  return function(){\
  \    return htmlelt.clientHeight;\ 
  \  };\
  \};" :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number

foreign import setClientWidth
  "function setClientWidth(width){\
  \   return function(htmlelt){\ 
  \      return function(){\
  \        htmlelt.width = width;\ 
  \        return htmlelt;\
  \      };\
  \    };\
  \};" :: forall eff. Number -> HTMLElement -> Eff (dom :: DOM | eff) Number

foreign import setClientHeight
  "function setClientHeight(height){\
  \   return function(htmlelt){\ 
  \      return function(){\
  \        htmlelt.height = height;\ 
  \        return htmlelt;\
  \      };\
  \    };\
  \};" :: forall eff. Number -> HTMLElement -> Eff (dom :: DOM | eff) Number

foreign import getOffsetWidth
  "function getOffsetWidth(htmlelt){\
  \  return function(){\
  \    return htmlelt.offsetWidth;\ 
  \  };\
  \};" :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number

foreign import getOffsetHeight
  "function getOffsetHeight(htmlelt){\
  \  return function(){\
  \    return htmlelt.offsetHeight;\ 
  \  };\
  \};" :: forall eff. HTMLElement -> Eff (dom :: DOM | eff) Number


