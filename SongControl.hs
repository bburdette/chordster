module SongControl where

import Control.Concurrent
import Model 
import Data.IORef
import Data.Maybe
import Text.Show
import Data.Eq

data Whatever = WhatSong SongId 
              | WhatSongSequence SongSequenceId
  deriving (Eq, Show)

data SongControl = SongControl 
  { playThread :: IORef (Maybe (ThreadId, SongId))
  , whateverThread :: IORef (Maybe (ThreadId, Whatever))
  }


