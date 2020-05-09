import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Database.RocksDB.Resource as RDB
import Debug.Trace

withDB :: MonadResource m => m a -> m a
withDB f = do
  -- runResourceT $ do
  f

main :: IO ()
main =
  runResourceT $ do
    (k1, opts) <- RDB.optionsCreate
    runResourceT $ do
      withDB $ do
        liftIO $ traceIO "XXX 1"
        RDB.optionsSetCreateIfMissing opts True
        (k2, rdb) <- RDB.open opts "mydb"
        liftIO $ traceIO "XXX 2"
        --
        release k1
        --
        return ()
    liftIO $ traceIO "XXX 3"
    return ()
