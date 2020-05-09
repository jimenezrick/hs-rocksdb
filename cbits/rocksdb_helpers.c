#include "rocksdb_helpers.h"

void rocksdb_close_ifnotnull(rocksdb_t *db) {
  if (db) { rocksdb_close(db); }
}

void rocksdb_options_destroy_ifnotnull(rocksdb_options_t *opts) {
  if (opts) { rocksdb_options_destroy(opts); }
}
