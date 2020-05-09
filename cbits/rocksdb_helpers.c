#include "rocksdb_helpers.h"

void rocksdb_close_ifnotnull(rocksdb_t *db) {
  if (db) { rocksdb_close(db); }
}
