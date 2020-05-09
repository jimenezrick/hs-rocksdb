#pragma once

#include "rocksdb/c.h"

extern ROCKSDB_LIBRARY_API void rocksdb_close_ifnotnull(rocksdb_t *db);
