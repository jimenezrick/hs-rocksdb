module Database.RocksDB.C where

import Control.Exception
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.Marshal.Alloc hiding (free)

#include "rocksdb/c.h"
#include "rocksdb_helpers.h"

{#context prefix = "rocksdb"#}

{#enum no_compression as Compression {underscoreToCase} deriving (Eq, Show)#}

{#pointer *rocksdb_t as DB foreign finalizer rocksdb_close_ifnotnull as ^ newtype#}
{#pointer *rocksdb_options_t as Options foreign finalizer rocksdb_options_destroy_ifnotnull as ^ newtype#}

{#fun open as ^ {`Options', `String', allocaNullPtr- `CString' throwIfPeekNull*-} -> `DB'#}
{#fun options_create as ^ {} -> `Options'#}
{#fun options_set_create_if_missing as ^ {`Options', `Bool'} -> `()'#}
{#fun free as ^ {`Ptr ()'} -> `()'#}

data DBError = DBError String deriving Show

instance Exception DBError

throwIfPeekNull :: Ptr CString -> IO ()
throwIfPeekNull pStr = do
  str <- peek pStr
  if nullPtr == str
    then return ()
    else do
      err <- peekCString str
      free $ castPtr str
      throwIO $ DBError err

allocaNullPtr :: Storable a => (Ptr (Ptr a) -> IO b) -> IO b
allocaNullPtr f = alloca (\ptr -> poke ptr nullPtr >> f ptr)
