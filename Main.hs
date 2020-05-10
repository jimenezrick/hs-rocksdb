import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Database.RocksDB.Resource as RDB

main :: IO ()
main = do
  runResourceT $ do
    (k1, opts) <- RDB.optionsCreate'
    runResourceT $ do
      RDB.optionsSetCreateIfMissing opts True
      db <- RDB.open opts "mydb"
      -- GET
      ropts <- RDB.readOptionsCreate
      wopts <- RDB.writeOptionsCreate
      -- NOT EXISTENT value
      v <- RDB.get db ropts (BS.pack "yyy")
      liftIO $ print (v, BS.null v)
      v' <- RDB.getPinned db ropts (BS.pack "yyy")
      liftIO $ print v'
      -- NULL value
      RDB.put db wopts (BS.pack "null") (BS.pack "")
      v2 <- RDB.get db ropts (BS.pack "null")
      liftIO $ print (v2, BS.null v2)
      v2' <- RDB.getPinned db ropts (BS.pack "null")
      liftIO $ print v2'
      --
      release k1
      --
      return ()
    return ()
