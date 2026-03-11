#include <string.h>
#include <stdatomic.h>

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>

#include "duckdb.h"

/* ── Exception slots ───────────────────────────────────────────────── */

static const value *exn_error = NULL;

static inline void raise_duckdb_error(const char *msg) {
  if (exn_error == NULL)
    exn_error = caml_named_value("Duckdb.Error");
  caml_raise_with_string(*exn_error, msg);
}

/* ── Database handle ───────────────────────────────────────────────── */

typedef struct db_wrap {
  duckdb_database db;
  _Atomic(int) ref_count;
} db_wrap;

#define Db_wrap_val(v) (*((db_wrap **)Data_custom_val(v)))

static void finalize_db(value v) {
  db_wrap *w = Db_wrap_val(v);
  if (w != NULL && atomic_fetch_sub(&w->ref_count, 1) == 1) {
    duckdb_close(&w->db);
    caml_stat_free(w);
  }
}

static struct custom_operations db_ops = {
    "duckdb.database",
    finalize_db,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* ── Connection handle ─────────────────────────────────────────────── */

typedef struct conn_wrap {
  duckdb_connection conn;
  db_wrap *db;  /* prevents db from being freed while connection alive */
} conn_wrap;

#define Conn_wrap_val(v) (*((conn_wrap **)Data_custom_val(v)))

static void finalize_conn(value v) {
  conn_wrap *w = Conn_wrap_val(v);
  if (w != NULL) {
    duckdb_disconnect(&w->conn);
    if (w->db != NULL && atomic_fetch_sub(&w->db->ref_count, 1) == 1) {
      duckdb_close(&w->db->db);
      caml_stat_free(w->db);
    }
    caml_stat_free(w);
  }
}

static struct custom_operations conn_ops = {
    "duckdb.connection",
    finalize_conn,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* ── Result handle ─────────────────────────────────────────────────── */

typedef struct result_wrap {
  duckdb_result result;
  int valid;
} result_wrap;

#define Result_wrap_val(v) (*((result_wrap **)Data_custom_val(v)))

static void finalize_result(value v) {
  result_wrap *w = Result_wrap_val(v);
  if (w != NULL) {
    if (w->valid)
      duckdb_destroy_result(&w->result);
    caml_stat_free(w);
  }
}

static struct custom_operations result_ops = {
    "duckdb.result",
    finalize_result,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* ── Prepared statement handle ─────────────────────────────────────── */

/* The stmt ref-counts the db_wrap to prevent the database from being freed
   while the prepared statement is alive. The connection's own finalizer
   handles disconnection independently. */

typedef struct stmt_wrap2 {
  duckdb_prepared_statement stmt;
  db_wrap *db;  /* prevents db from being freed while stmt alive */
} stmt_wrap2;

#define Stmt_wrap_val(v) (*((stmt_wrap2 **)Data_custom_val(v)))

static void finalize_stmt2(value v) {
  stmt_wrap2 *w = Stmt_wrap_val(v);
  if (w != NULL) {
    if (w->stmt != NULL)
      duckdb_destroy_prepare(&w->stmt);
    if (w->db != NULL && atomic_fetch_sub(&w->db->ref_count, 1) == 1) {
      duckdb_close(&w->db->db);
      caml_stat_free(w->db);
    }
    caml_stat_free(w);
  }
}

static struct custom_operations stmt_ops = {
    "duckdb.prepared_statement",
    finalize_stmt2,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* ── Data chunk handle ─────────────────────────────────────────────── */

typedef struct chunk_wrap {
  duckdb_data_chunk chunk;
} chunk_wrap;

#define Chunk_wrap_val(v) (*((chunk_wrap **)Data_custom_val(v)))

static void finalize_chunk(value v) {
  chunk_wrap *w = Chunk_wrap_val(v);
  if (w != NULL) {
    if (w->chunk != NULL)
      duckdb_destroy_data_chunk(&w->chunk);
    caml_stat_free(w);
  }
}

static struct custom_operations chunk_ops = {
    "duckdb.data_chunk",
    finalize_chunk,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default,
};

/* ── Init ──────────────────────────────────────────────────────────── */

CAMLprim value caml_duckdb_init(value v_unit) {
  (void)v_unit;
  exn_error = caml_named_value("Duckdb.Error");
  return Val_unit;
}

/* ── Database open / close ─────────────────────────────────────────── */

CAMLprim value caml_duckdb_open(value v_path) {
  const char *path;
  duckdb_database db;
  char *err = NULL;
  duckdb_state rc;
  db_wrap *dbw;
  value v_res;

  if (v_path == Val_none) {
    path = NULL;
  } else {
    path = String_val(Field(v_path, 0));
  }

  caml_enter_blocking_section();
  rc = duckdb_open_ext(path, &db, NULL, &err);
  caml_leave_blocking_section();

  if (rc == DuckDBError) {
    value v_msg = caml_copy_string(err ? err : "duckdb_open failed");
    if (err) duckdb_free(err);
    raise_duckdb_error(String_val(v_msg));
  }

  dbw = caml_stat_alloc(sizeof(db_wrap));
  dbw->db = db;
  atomic_init(&dbw->ref_count, 1);

  v_res = caml_alloc_custom(&db_ops, sizeof(db_wrap *), 0, 1);
  Db_wrap_val(v_res) = dbw;
  return v_res;
}

CAMLprim value caml_duckdb_close(value v_db) {
  db_wrap *dbw = Db_wrap_val(v_db);
  if (dbw != NULL && dbw->db != NULL) {
    duckdb_close(&dbw->db);
    dbw->db = NULL;
  }
  return Val_unit;
}

/* ── Connection ────────────────────────────────────────────────────── */

CAMLprim value caml_duckdb_connect(value v_db) {
  db_wrap *dbw = Db_wrap_val(v_db);
  duckdb_connection conn;
  conn_wrap *cw;
  value v_res;
  duckdb_state rc;

  if (dbw == NULL || dbw->db == NULL)
    raise_duckdb_error("database is closed");

  rc = duckdb_connect(dbw->db, &conn);
  if (rc == DuckDBError)
    raise_duckdb_error("duckdb_connect failed");

  atomic_fetch_add(&dbw->ref_count, 1);

  cw = caml_stat_alloc(sizeof(conn_wrap));
  cw->conn = conn;
  cw->db = dbw;

  v_res = caml_alloc_custom(&conn_ops, sizeof(conn_wrap *), 0, 1);
  Conn_wrap_val(v_res) = cw;
  return v_res;
}

/* ── Query ─────────────────────────────────────────────────────────── */

CAMLprim value caml_duckdb_query(value v_conn, value v_sql) {
  conn_wrap *cw = Conn_wrap_val(v_conn);
  const char *sql = String_val(v_sql);
  result_wrap *rw;
  duckdb_state rc;
  value v_res;

  if (cw == NULL)
    raise_duckdb_error("connection is closed");

  rw = caml_stat_alloc(sizeof(result_wrap));
  memset(rw, 0, sizeof(result_wrap));

  caml_enter_blocking_section();
  rc = duckdb_query(cw->conn, sql, &rw->result);
  caml_leave_blocking_section();

  if (rc == DuckDBError) {
    const char *err = duckdb_result_error(&rw->result);
    value v_msg = caml_copy_string(err ? err : "query failed");
    duckdb_destroy_result(&rw->result);
    caml_stat_free(rw);
    raise_duckdb_error(String_val(v_msg));
  }

  rw->valid = 1;
  v_res = caml_alloc_custom(&result_ops, sizeof(result_wrap *), 0, 1);
  Result_wrap_val(v_res) = rw;
  return v_res;
}

/* ── Prepared statements ───────────────────────────────────────────── */

CAMLprim value caml_duckdb_prepare(value v_conn, value v_sql) {
  conn_wrap *cw = Conn_wrap_val(v_conn);
  const char *sql = String_val(v_sql);
  duckdb_prepared_statement ps;
  duckdb_state rc;
  stmt_wrap2 *sw;
  value v_res;

  if (cw == NULL)
    raise_duckdb_error("connection is closed");

  rc = duckdb_prepare(cw->conn, sql, &ps);
  if (rc == DuckDBError) {
    const char *err = duckdb_prepare_error(ps);
    value v_msg = caml_copy_string(err ? err : "prepare failed");
    duckdb_destroy_prepare(&ps);
    raise_duckdb_error(String_val(v_msg));
  }

  /* Ref-count the db so it stays alive */
  atomic_fetch_add(&cw->db->ref_count, 1);

  sw = caml_stat_alloc(sizeof(stmt_wrap2));
  sw->stmt = ps;
  sw->db = cw->db;

  v_res = caml_alloc_custom(&stmt_ops, sizeof(stmt_wrap2 *), 0, 1);
  Stmt_wrap_val(v_res) = sw;
  return v_res;
}

CAMLprim value caml_duckdb_execute_prepared(value v_stmt) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  result_wrap *rw;
  duckdb_state rc;
  value v_res;

  if (sw == NULL || sw->stmt == NULL)
    raise_duckdb_error("prepared statement is destroyed");

  rw = caml_stat_alloc(sizeof(result_wrap));
  memset(rw, 0, sizeof(result_wrap));

  caml_enter_blocking_section();
  rc = duckdb_execute_prepared(sw->stmt, &rw->result);
  caml_leave_blocking_section();

  if (rc == DuckDBError) {
    const char *err = duckdb_result_error(&rw->result);
    value v_msg = caml_copy_string(err ? err : "execute failed");
    duckdb_destroy_result(&rw->result);
    caml_stat_free(rw);
    raise_duckdb_error(String_val(v_msg));
  }

  rw->valid = 1;
  v_res = caml_alloc_custom(&result_ops, sizeof(result_wrap *), 0, 1);
  Result_wrap_val(v_res) = rw;
  return v_res;
}

/* ── Bind functions ────────────────────────────────────────────────── */

#define BIND_CHECK(sw) \
  if ((sw) == NULL || (sw)->stmt == NULL) \
    raise_duckdb_error("prepared statement is destroyed")

#define BIND_STATE_CHECK(rc) \
  if ((rc) == DuckDBError) \
    raise_duckdb_error("bind failed")

void caml_duckdb_bind_bool(value v_stmt, intnat idx, value v_val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_boolean(sw->stmt, (idx_t)idx, Bool_val(v_val)));
}

CAMLprim value caml_duckdb_bind_bool_bc(value v_stmt, value v_idx, value v_val) {
  caml_duckdb_bind_bool(v_stmt, Long_val(v_idx), v_val);
  return Val_unit;
}

void caml_duckdb_bind_int32(value v_stmt, intnat idx, int32_t val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_int32(sw->stmt, (idx_t)idx, val));
}

CAMLprim value caml_duckdb_bind_int32_bc(value v_stmt, value v_idx, value v_val) {
  caml_duckdb_bind_int32(v_stmt, Long_val(v_idx), Int32_val(v_val));
  return Val_unit;
}

void caml_duckdb_bind_int64(value v_stmt, intnat idx, int64_t val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_int64(sw->stmt, (idx_t)idx, val));
}

CAMLprim value caml_duckdb_bind_int64_bc(value v_stmt, value v_idx, value v_val) {
  caml_duckdb_bind_int64(v_stmt, Long_val(v_idx), Int64_val(v_val));
  return Val_unit;
}

void caml_duckdb_bind_double(value v_stmt, intnat idx, double val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_double(sw->stmt, (idx_t)idx, val));
}

CAMLprim value caml_duckdb_bind_double_bc(value v_stmt, value v_idx, value v_val) {
  caml_duckdb_bind_double(v_stmt, Long_val(v_idx), Double_val(v_val));
  return Val_unit;
}

CAMLprim value caml_duckdb_bind_string(value v_stmt, value v_idx, value v_val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_varchar_length(sw->stmt, (idx_t)Long_val(v_idx),
                                               String_val(v_val),
                                               caml_string_length(v_val)));
  return Val_unit;
}

CAMLprim value caml_duckdb_bind_blob(value v_stmt, value v_idx, value v_val) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_blob(sw->stmt, (idx_t)Long_val(v_idx),
                                     Bytes_val(v_val),
                                     caml_string_length(v_val)));
  return Val_unit;
}

void caml_duckdb_bind_null(value v_stmt, intnat idx) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_bind_null(sw->stmt, (idx_t)idx));
}

CAMLprim value caml_duckdb_bind_null_bc(value v_stmt, value v_idx) {
  caml_duckdb_bind_null(v_stmt, Long_val(v_idx));
  return Val_unit;
}

CAMLprim value caml_duckdb_clear_bindings(value v_stmt) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  BIND_CHECK(sw);
  BIND_STATE_CHECK(duckdb_clear_bindings(sw->stmt));
  return Val_unit;
}

intnat caml_duckdb_nparams(value v_stmt) {
  stmt_wrap2 *sw = Stmt_wrap_val(v_stmt);
  if (sw == NULL || sw->stmt == NULL) return 0;
  return (intnat)duckdb_nparams(sw->stmt);
}

CAMLprim value caml_duckdb_nparams_bc(value v_stmt) {
  return Val_long(caml_duckdb_nparams(v_stmt));
}

/* ── Result metadata — untagged/noalloc ────────────────────────────── */

intnat caml_duckdb_column_count(value v_res) {
  result_wrap *rw = Result_wrap_val(v_res);
  return (intnat)duckdb_column_count(&rw->result);
}

CAMLprim value caml_duckdb_column_count_bc(value v_res) {
  return Val_long(caml_duckdb_column_count(v_res));
}

intnat caml_duckdb_row_count(value v_res) {
  result_wrap *rw = Result_wrap_val(v_res);
  return (intnat)duckdb_row_count(&rw->result);
}

CAMLprim value caml_duckdb_row_count_bc(value v_res) {
  return Val_long(caml_duckdb_row_count(v_res));
}

intnat caml_duckdb_rows_changed(value v_res) {
  result_wrap *rw = Result_wrap_val(v_res);
  return (intnat)duckdb_rows_changed(&rw->result);
}

CAMLprim value caml_duckdb_rows_changed_bc(value v_res) {
  return Val_long(caml_duckdb_rows_changed(v_res));
}

/* ── Column name ───────────────────────────────────────────────────── */

CAMLprim value caml_duckdb_column_name(value v_res, value v_col) {
  result_wrap *rw = Result_wrap_val(v_res);
  idx_t col = Long_val(v_col);
  const char *name = duckdb_column_name(&rw->result, col);
  if (name == NULL)
    raise_duckdb_error("invalid column index");
  return caml_copy_string(name);
}

/* ── Column type ───────────────────────────────────────────────────── */

intnat caml_duckdb_column_type(value v_res, intnat col) {
  result_wrap *rw = Result_wrap_val(v_res);
  return (intnat)duckdb_column_type(&rw->result, (idx_t)col);
}

CAMLprim value caml_duckdb_column_type_bc(value v_res, value v_col) {
  return Val_long(caml_duckdb_column_type(v_res, Long_val(v_col)));
}

/* ── Typed column access — unboxed/noalloc ─────────────────────────── */

int64_t caml_duckdb_value_int64(value v_res, intnat col, intnat row) {
  result_wrap *rw = Result_wrap_val(v_res);
  return duckdb_value_int64(&rw->result, (idx_t)col, (idx_t)row);
}

CAMLprim value caml_duckdb_value_int64_bc(value v_res, value v_col, value v_row) {
  return caml_copy_int64(caml_duckdb_value_int64(v_res, Long_val(v_col), Long_val(v_row)));
}

double caml_duckdb_value_double(value v_res, intnat col, intnat row) {
  result_wrap *rw = Result_wrap_val(v_res);
  return duckdb_value_double(&rw->result, (idx_t)col, (idx_t)row);
}

CAMLprim value caml_duckdb_value_double_bc(value v_res, value v_col, value v_row) {
  return caml_copy_double(caml_duckdb_value_double(v_res, Long_val(v_col), Long_val(v_row)));
}

int32_t caml_duckdb_value_int32(value v_res, intnat col, intnat row) {
  result_wrap *rw = Result_wrap_val(v_res);
  return duckdb_value_int32(&rw->result, (idx_t)col, (idx_t)row);
}

CAMLprim value caml_duckdb_value_int32_bc(value v_res, value v_col, value v_row) {
  return caml_copy_int32(caml_duckdb_value_int32(v_res, Long_val(v_col), Long_val(v_row)));
}

/* ── String column value ───────────────────────────────────────────── */

CAMLprim value caml_duckdb_value_string(value v_res, value v_col, value v_row) {
  result_wrap *rw = Result_wrap_val(v_res);
  idx_t col = Long_val(v_col);
  idx_t row = Long_val(v_row);
  char *str = duckdb_value_varchar(&rw->result, col, row);
  value v_str;
  if (str == NULL)
    return caml_copy_string("");
  v_str = caml_copy_string(str);
  duckdb_free(str);
  return v_str;
}

/* ── Null check — noalloc ──────────────────────────────────────────── */

value caml_duckdb_value_is_null(value v_res, intnat col, intnat row) {
  result_wrap *rw = Result_wrap_val(v_res);
  return Val_bool(duckdb_value_is_null(&rw->result, (idx_t)col, (idx_t)row));
}

CAMLprim value caml_duckdb_value_is_null_bc(value v_res, value v_col, value v_row) {
  return caml_duckdb_value_is_null(v_res, Long_val(v_col), Long_val(v_row));
}

/* ── Data chunk access ─────────────────────────────────────────────── */

intnat caml_duckdb_result_chunk_count(value v_res) {
  result_wrap *rw = Result_wrap_val(v_res);
  return (intnat)duckdb_result_chunk_count(rw->result);
}

CAMLprim value caml_duckdb_result_chunk_count_bc(value v_res) {
  return Val_long(caml_duckdb_result_chunk_count(v_res));
}

CAMLprim value caml_duckdb_result_get_chunk(value v_res, value v_idx) {
  result_wrap *rw = Result_wrap_val(v_res);
  idx_t idx = Long_val(v_idx);
  duckdb_data_chunk chunk = duckdb_result_get_chunk(rw->result, idx);
  chunk_wrap *cw;
  value v_chunk;

  if (chunk == NULL)
    raise_duckdb_error("invalid chunk index");

  cw = caml_stat_alloc(sizeof(chunk_wrap));
  cw->chunk = chunk;

  v_chunk = caml_alloc_custom(&chunk_ops, sizeof(chunk_wrap *), 0, 1);
  Chunk_wrap_val(v_chunk) = cw;
  return v_chunk;
}

intnat caml_duckdb_chunk_get_size(value v_chunk) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  return (intnat)duckdb_data_chunk_get_size(cw->chunk);
}

CAMLprim value caml_duckdb_chunk_get_size_bc(value v_chunk) {
  return Val_long(caml_duckdb_chunk_get_size(v_chunk));
}

intnat caml_duckdb_chunk_get_column_count(value v_chunk) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  return (intnat)duckdb_data_chunk_get_column_count(cw->chunk);
}

CAMLprim value caml_duckdb_chunk_get_column_count_bc(value v_chunk) {
  return Val_long(caml_duckdb_chunk_get_column_count(v_chunk));
}

/* ── Vector data as Bigstring (zero-copy) ──────────────────────────── */

CAMLprim value caml_duckdb_vector_data(value v_chunk, value v_col) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  idx_t col = Long_val(v_col);
  duckdb_vector vec = duckdb_data_chunk_get_vector(cw->chunk, col);
  void *data = duckdb_vector_get_data(vec);
  idx_t size = duckdb_data_chunk_get_size(cw->chunk);

  if (data == NULL)
    raise_duckdb_error("vector has no data");

  /* Wrap raw pointer as Bigarray with CAML_BA_EXTERNAL (no ownership) */
  /* We use the max possible size: vector_size * 16 bytes (for duckdb_string_t) */
  intnat dim = (intnat)(size * 16);  /* conservative upper bound */

  return caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
                       1, data, &dim);
}

/* ── Vector validity as Bigstring (zero-copy) ──────────────────────── */

CAMLprim value caml_duckdb_vector_validity(value v_chunk, value v_col) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  idx_t col = Long_val(v_col);
  duckdb_vector vec = duckdb_data_chunk_get_vector(cw->chunk, col);
  uint64_t *validity = duckdb_vector_get_validity(vec);

  if (validity == NULL)
    return Val_none;  /* all valid */

  /* Validity is ceil(vector_size / 64) * 8 bytes */
  idx_t size = duckdb_data_chunk_get_size(cw->chunk);
  intnat dim = (intnat)(((size + 63) / 64) * 8);

  value v_ba = caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
                              1, validity, &dim);
  /* Wrap in Some */
  value v_some = caml_alloc_small(1, 0);
  Field(v_some, 0) = v_ba;
  return v_some;
}

/* ── Vector null check — noalloc ───────────────────────────────────── */

value caml_duckdb_vector_is_valid(value v_chunk, intnat col, intnat row) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  duckdb_vector vec = duckdb_data_chunk_get_vector(cw->chunk, (idx_t)col);
  uint64_t *validity = duckdb_vector_get_validity(vec);
  if (validity == NULL)
    return Val_true;  /* all valid */
  return Val_bool(duckdb_validity_row_is_valid(validity, (idx_t)row));
}

CAMLprim value caml_duckdb_vector_is_valid_bc(value v_chunk, value v_col, value v_row) {
  return caml_duckdb_vector_is_valid(v_chunk, Long_val(v_col), Long_val(v_row));
}

/* ── Vector string element access ──────────────────────────────────── */

CAMLprim value caml_duckdb_vector_get_string(value v_chunk, value v_col, value v_row) {
  chunk_wrap *cw = Chunk_wrap_val(v_chunk);
  idx_t col = Long_val(v_col);
  idx_t row = Long_val(v_row);
  duckdb_vector vec = duckdb_data_chunk_get_vector(cw->chunk, col);
  duckdb_string_t *strings = (duckdb_string_t *)duckdb_vector_get_data(vec);
  duckdb_string_t s = strings[row];
  uint32_t len = duckdb_string_t_length(s);
  const char *data = duckdb_string_t_data(&strings[row]);
  return caml_alloc_initialized_string(len, data);
}

/* ── Library version ───────────────────────────────────────────────── */

CAMLprim value caml_duckdb_library_version(value v_unit) {
  (void)v_unit;
  return caml_copy_string(duckdb_library_version());
}

/* ── Vector size constant ──────────────────────────────────────────── */

intnat caml_duckdb_vector_size(value v_unit) {
  (void)v_unit;
  return (intnat)duckdb_vector_size();
}

CAMLprim value caml_duckdb_vector_size_bc(value v_unit) {
  return Val_long(caml_duckdb_vector_size(v_unit));
}
