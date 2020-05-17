{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import qualified Data.ByteString.Char8 as BS
import qualified Database.RocksDB.Resource as DB
import qualified Network.MessagePack.Client as Client
import qualified Network.MessagePack.Rpc as RPC
import qualified Network.MessagePack.Server as Server
import qualified Network.MessagePack.Types as Server
import System.Environment (getArgs)

data Boom = Boom deriving (Show)

instance Exception Boom

set :: RPC.Rpc (RPC.ReturnsM IO ())
set = RPC.stubs "set" (RPC.RetM "void") $ do
  throwM $ Server.ServerError "caca"
  print "* set"
  return ()

methods :: [Server.Method IO]
methods = [RPC.method set]

runServer :: Int -> IO ()
runServer port =
  Server.runServer port methods

runClient :: String -> Int -> IO ()
runClient host port = do
  Client.runClient (BS.pack host) port $ do
    RPC.rpc set

main' :: IO ()
main' = do
  runResourceT $ do
    (k1, opts) <- DB.optionsCreate'
    runResourceT $ do
      DB.optionsSetCreateIfMissing opts True
      db <- DB.open opts "mydb"
      -- GET
      ropts <- DB.readOptionsCreate
      wopts <- DB.writeOptionsCreate
      -- NOT EXISTENT value
      v <- DB.get db ropts (BS.pack "yyy")
      liftIO $ print (v, BS.null v)
      v' <- DB.getPinned db ropts (BS.pack "yyy")
      liftIO $ print v'
      -- NULL value
      DB.put db wopts (BS.pack "null") (BS.pack "")
      v2 <- DB.get db ropts (BS.pack "null")
      liftIO $ print (v2, BS.null v2)
      v2' <- DB.getPinned db ropts (BS.pack "null")
      liftIO $ print v2'
      --
      release k1
      --
      return ()
    return ()

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Running server"
      runServer 8000
    else forM_ [1 .. 1] $ const $ runClient "localhost" 8000
