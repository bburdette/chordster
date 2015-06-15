module SongControl where

import Control.Concurrent
import Model 

data SongControl = SongControl 
  {
    playThread :: MVar (ThreadId, SongId)
  }


