module Database.RocksDB.C where

import Control.Exception
import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc hiding (free)

#include "rocksdb/c.h"
#include "rocksdb_helpers.h"

{#context prefix = "rocksdb"#}

{#typedef size_t CSize#}

{#enum no_compression as Compression {underscoreToCase} deriving (Eq, Show)#}

-- XXX rocksdb_t prefix, remove
{#pointer *rocksdb_t as DB foreign finalizer rocksdb_close_ifnotnull as ^ newtype#}
{#pointer *rocksdb_options_t as Options foreign finalizer rocksdb_options_destroy as ^ newtype#}
{#pointer *rocksdb_readoptions_t as ReadOptions foreign finalizer rocksdb_readoptions_destroy as ^ newtype#}
{#pointer *rocksdb_writeoptions_t as WriteOptions foreign finalizer rocksdb_writeoptions_destroy as ^ newtype#}

{#pointer *rocksdb_pinnableslice_t as PinnableSlice newtype#}

{#fun unsafe pinnableslice_value as ^ {`PinnableSlice', alloca- `CSize' peek*} -> `CString'#}
{#fun unsafe pinnableslice_destroy as ^ {`PinnableSlice'} -> `()'#}

{#fun open as ^ {`Options', `String', allocaNullPtr- `CString' throwIfPeekNull*-} -> `DB'#}

{#fun unsafe options_create as ^ {} -> `Options'#}
{#fun unsafe options_set_create_if_missing as ^ {`Options', `Bool'} -> `()'#}

{#fun unsafe readoptions_create as ^ {} -> `ReadOptions'#}
{#fun unsafe writeoptions_create as ^ {} -> `WriteOptions'#}

{#fun get as ^ {`DB', `ReadOptions', fromCStringLen `CStringLen'&, alloca- `CSize' peek*, allocaNullPtr- `CString' throwIfPeekNull*-} -> `CString'#}
{#fun get_pinned as ^ {`DB', `ReadOptions', fromCStringLen `CStringLen'&, allocaNullPtr- `CString' throwIfPeekNull*-} -> `PinnableSlice'#}
{#fun put as ^ {`DB', `WriteOptions', fromCStringLen `CStringLen'&, fromCStringLen `CStringLen'&, allocaNullPtr- `CString' throwIfPeekNull*-} -> `()'#}

{#fun unsafe free as ^ {`Ptr ()'} -> `()'#}

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

fromCStringLen :: CStringLen -> (CString, CSize)
fromCStringLen (cstr, len) = (cstr, fromIntegral len)

toCStringLen :: (CString, CSize) -> CStringLen
toCStringLen (cstr, csize) = (cstr, fromIntegral csize)
