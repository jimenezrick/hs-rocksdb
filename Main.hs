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
      wopts <- RDB.writeOptionsCreate
      v <- RDB.get db ropts (BS.pack "xxx")
      liftIO $ print (v, BS.null v)
      RDB.put db wopts (BS.pack "xxx") (BS.pack "")
      --
      release k1
      --
      return ()
    liftIO $ traceIO "XXX 3"
    return ()
