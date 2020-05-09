import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Database.RocksDB.Resource as RDB
import Debug.Trace

main :: IO ()
main = do
  runResourceT $ do
    (k1, opts) <- RDB.optionsCreate'
    runResourceT $ do
      liftIO $ traceIO "XXX 1"
      RDB.optionsSetCreateIfMissing opts True
      db <- RDB.open opts "mydb"
      liftIO $ traceIO "XXX 2"
      -- GET
      ropts <- RDB.readOptionsCreate
      v <- RDB.get db ropts (BS.pack "foo")
      liftIO $ BS.putStrLn v
      --
      release k1
      --
      return ()
    liftIO $ traceIO "XXX 3"
    return ()
