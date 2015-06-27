module SongControl where

import Control.Concurrent
import Model 
import Data.IORef
import Data.Maybe

data SongControl = SongControl 
  {
    playThread :: IORef (Maybe (ThreadId, SongId))
  }


