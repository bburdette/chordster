module SongControl where

import Control.Concurrent

data SongControl = SongControl 
  {
    playThread :: MVar ThreadId
  }


