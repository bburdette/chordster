module PlayWhatever where

import Import
import SongControl
import Control.Concurrent
import Model 
import Data.IORef
import Data.Maybe
import Text.Show
import Data.Eq
import Data.Traversable as TR
import Yesod.WebSockets
import Control.Monad (forever)
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import qualified Data.Text as T
import PlaySong

playWhateverWs :: (TChan Text -> IO ()) -> Maybe Text -> Whatever -> WebSocketsT Handler ()
playWhateverWs whateverftn mbnewthreadtext whateverid = do 
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  -- info for old whatever currently playing.
  oldinfo <- liftIO $ readIORef $ whateverThread $ songControl app
  liftIO $ print $ "oldinfo: " ++ show oldinfo
  case oldinfo of 
    Just (tid, id) | id == whateverid -> do  
      liftIO $ print $ "playwhatever thread exists; doing nothing. " ++ show (tid, id)
      case mbnewthreadtext of
        Just text -> do
          liftIO $ print ("at sendtextdata: " ++ show text)
          sendTextData text
          return () 
        _ -> return ()
    Just (tid, _) -> do  
      -- kill the old thread. 
      liftIO $ print $ "killing old thread: " ++ show tid
      liftIO $ TR.mapM (\(tid,_) -> killThread tid) oldinfo
      -- start a new song thread
      threadid <- liftIO $ forkIO $ whateverftn writeChan 
      -- save the new thread id and song id in the MVar.
      -- _ <- liftIO $ tryTakeMVar $ playThread $ songControl app
      res <- liftIO $ writeIORef (whateverThread $ songControl app) $ Just (threadid, whateverid)
      liftIO $ print $ "res= " ++ show res
      return ()
    Nothing -> do 
      liftIO $ print $ "starting new song thread"  
      -- start a new song thread
      threadid <- liftIO $ forkIO $ whateverftn writeChan 
      -- save the new thread id and song id in the MVar.
      res <- liftIO $ writeIORef (whateverThread $ songControl app) $ Just (threadid, whateverid)
      liftIO $ print $ "nothing res= " ++ show res
      return ()
  liftIO $ print "pre readTChan, etc"
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
  liftIO $ print "post readTChan, etc"
  return ()

listenWs :: WebSocketsT Handler ()
listenWs  = do 
  app <- getYesod
  let writeChan = songLine app
  readChan <- (liftIO . atomically) $ dupTChan writeChan
  (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
  return ()


