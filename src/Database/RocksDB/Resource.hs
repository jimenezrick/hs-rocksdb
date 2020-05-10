module Database.RocksDB.Resource
  ( DB,
    Compression,
    Options,
    ReadOptions,
    open,
    open',
    optionsCreate,
    optionsCreate',
    optionsSetCreateIfMissing,
    readOptionsCreate,
    readOptionsCreate',
    writeOptionsCreate,
    writeOptionsCreate',
    get,
    getPinned,
    put,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (MonadResource, ReleaseKey)
import qualified Control.Monad.Trans.Resource as R
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer, unsafePackMallocCStringLen, unsafeUseAsCStringLen)
import Database.RocksDB.C (Compression, DB (..), Options (..), PinnableSlice (..), ReadOptions (..), WriteOptions (..))
import qualified Database.RocksDB.C as C
import Foreign.ForeignPtr (finalizeForeignPtr)
import Foreign.Ptr (castPtr, nullPtr)

open :: MonadResource m => Options -> String -> m DB
open opts name = snd <$> open' opts name

open' :: MonadResource m => Options -> String -> m (ReleaseKey, DB)
open' opts name = R.allocate (C.open opts name) (\(DB fptr) -> finalizeForeignPtr fptr)

optionsCreate :: MonadResource m => m Options
optionsCreate = snd <$> optionsCreate'

optionsCreate' :: MonadResource m => m (ReleaseKey, Options)
optionsCreate' = do
  R.allocate C.optionsCreate (\(Options fptr) -> finalizeForeignPtr fptr)

optionsSetCreateIfMissing :: MonadResource m => Options -> Bool -> m ()
optionsSetCreateIfMissing opts b = liftIO $ C.optionsSetCreateIfMissing opts b

readOptionsCreate :: MonadResource m => m ReadOptions
readOptionsCreate = snd <$> readOptionsCreate'

readOptionsCreate' :: MonadResource m => m (ReleaseKey, ReadOptions)
readOptionsCreate' = do
  R.allocate C.readoptionsCreate (\(ReadOptions fptr) -> finalizeForeignPtr fptr)

writeOptionsCreate :: MonadResource m => m WriteOptions
writeOptionsCreate = snd <$> writeOptionsCreate'

writeOptionsCreate' :: MonadResource m => m (ReleaseKey, WriteOptions)
writeOptionsCreate' = do
  R.allocate C.writeoptionsCreate (\(WriteOptions fptr) -> finalizeForeignPtr fptr)

get :: MonadResource m => DB -> ReadOptions -> ByteString -> m ByteString
get db opts key = do
  liftIO $ unsafeUseAsCStringLen key $ \ckey -> do
    C.toCStringLen <$> C.get db opts ckey >>= unsafePackMallocCStringLen

getPinned :: MonadResource m => DB -> ReadOptions -> ByteString -> m (Maybe ByteString)
getPinned db opts key = do
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
put db opts key val = do
  liftIO $ unsafeUseAsCStringLen key $ \ckey -> do
    liftIO $ unsafeUseAsCStringLen val $ \cval -> do
      C.put db opts ckey cval
