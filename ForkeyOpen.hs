module ForkeyOpen
    where

import qualified Database.Persist.Sqlite as PSqlite
import qualified Database.Sqlite as Sqlite
import qualified Database.Persist.Sql as Psql
import Data.Text
import Data.Int
import System.IO
import Control.Monad.IO.Class 
import Control.Monad.Trans.Control
import Control.Monad.Logger
import Data.Pool as P 

forKeyOpen :: Text -> Psql.LogFunc -> IO PSqlite.SqlBackend
forKeyOpen t lf = do 
  conn <- Sqlite.open t
  stmt <- Sqlite.prepare conn "PRAGMA foreign_keys = ON;"
  res <- Sqlite.step stmt 
  PSqlite.wrapConnection conn lf

forKeyCreateSqlitePool :: (Control.Monad.Trans.Control.MonadBaseControl IO m, MonadIO m, Control.Monad.Logger.MonadLogger m) => Text -> Int -> m (Pool Psql.SqlBackend)
forKeyCreateSqlitePool fileIguess count = do
 Psql.createSqlPool (forKeyOpen fileIguess) count


