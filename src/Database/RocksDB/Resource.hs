module Database.RocksDB.Resource
  ( DB,
    Compression,
    Options,
    ReadOptions,
    open,
    open',
    listColumnFamilies,
    optionsCreate,
    optionsCreate',
    optionsSetCreateIfMissing,
    readOptionsCreate,
    readOptionsCreate',
    writeOptionsCreate,
    writeOptionsCreate',
    writeOptionsSetSync,
    get,
    getUnpinned,
    getPinned,
    put,
  )
where

import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import qualified Control.Monad.Trans.Resource as R
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer, unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Database.RocksDB.C (Compression, DB (..), Options (..), PinnableSlice (..), ReadOptions (..), WriteOptions (..))
import qualified Database.RocksDB.C as C
import Foreign.C.String (peekCString)
import Foreign.ForeignPtr (finalizeForeignPtr)
import Foreign.Ptr (castPtr, nullPtr)
import Foreign.Storable (peekElemOff)

open :: MonadResource m => Options -> String -> m DB
open opts name = snd <$> open' opts name

open' :: MonadResource m => Options -> String -> m (ReleaseKey, DB)
open' opts name = R.allocate (C.open opts name) (\(DB fptr) -> finalizeForeignPtr fptr)

optionsCreate :: MonadResource m => m Options
optionsCreate = snd <$> optionsCreate'

optionsCreate' :: MonadResource m => m (ReleaseKey, Options)
optionsCreate' =
  R.allocate C.optionsCreate (\(Options fptr) -> finalizeForeignPtr fptr)

optionsSetCreateIfMissing :: MonadResource m => Options -> Bool -> m ()
optionsSetCreateIfMissing opts b = liftIO $ C.optionsSetCreateIfMissing opts b

readOptionsCreate :: MonadResource m => m ReadOptions
readOptionsCreate = snd <$> readOptionsCreate'

readOptionsCreate' :: MonadResource m => m (ReleaseKey, ReadOptions)
readOptionsCreate' =
  R.allocate C.readoptionsCreate (\(ReadOptions fptr) -> finalizeForeignPtr fptr)

writeOptionsCreate :: MonadResource m => m WriteOptions
writeOptionsCreate = snd <$> writeOptionsCreate'

writeOptionsCreate' :: MonadResource m => m (ReleaseKey, WriteOptions)
writeOptionsCreate' =
  R.allocate C.writeoptionsCreate (\(WriteOptions fptr) -> finalizeForeignPtr fptr)

writeOptionsSetSync :: MonadResource m => WriteOptions -> Bool -> m ()
writeOptionsSetSync opts b = liftIO $ C.writeoptionsSetSync opts b

get :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
get = getPinned

getUnpinned :: MonadResource m => DB -> ReadOptions -> ByteString -> m ByteString
getUnpinned db opts key =
  liftIO $ unsafeUseAsCStringLen key $ \ckey ->
    C.get db opts ckey >>= unsafePackMallocCStringLen . C.toCStringLen

getPinned :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
getPinned db opts key =
  liftIO $ unsafeUseAsCStringLen key $ \ckey -> do
    slice@(PinnableSlice ptr) <- C.getPinned db opts ckey
    if ptr == nullPtr
      then return Nothing
      else Just <$> pinnableSliceValue slice

pinnableSliceValue :: PinnableSlice -> IO ByteString
pinnableSliceValue slice = do
  (cstr, clen) <- C.toCStringLen <$> C.pinnablesliceValue slice
  unsafePackCStringFinalizer (castPtr cstr) clen (C.pinnablesliceDestroy slice)

put :: MonadResource m => DB -> WriteOptions -> ByteString -> ByteString -> m ()
put db opts key val =
  liftIO $ unsafeUseAsCStringLen key $ \ckey ->
    liftIO $ unsafeUseAsCStringLen val $ \cval ->
      C.put db opts ckey cval

listColumnFamilies :: MonadResource m => Options -> String -> m [String]
listColumnFamilies opts name = liftIO $ do
  (ptrCStr, len) <- C.listColumnFamilies opts name
  let len' = fromIntegral len
  cfList <- mapM (peekElemOff ptrCStr >=> peekCString) [0 .. len' -1]
  mapM_ (peekElemOff ptrCStr >=> C.free . castPtr) [0 .. len' -1]
  C.free $ castPtr ptrCStr
  return cfList
