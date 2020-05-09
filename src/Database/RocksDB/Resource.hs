module Database.RocksDB.Resource
  ( Compression,
    Options,
    DB,
    optionsCreate,
    optionsSetCreateIfMissing,
    open,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import qualified Control.Monad.Trans.Resource as R
import Database.RocksDB.C (Compression, DB (..), Options (..))
import qualified Database.RocksDB.C as C
import Debug.Trace
import Foreign.ForeignPtr (finalizeForeignPtr)

open :: MonadResource m => Options -> String -> m (ReleaseKey, DB)
open opts name = R.allocate (C.open opts name) (\(DB fptr) -> traceIO "DB.close" >> finalizeForeignPtr fptr)

optionsCreate :: MonadResource m => m (ReleaseKey, Options)
optionsCreate = do
  R.allocate C.optionsCreate (\(Options fptr) -> traceIO "options.destroy" >> finalizeForeignPtr fptr)

optionsSetCreateIfMissing :: MonadResource m => Options -> Bool -> m ()
optionsSetCreateIfMissing opts b = liftIO $ C.optionsSetCreateIfMissing opts b
