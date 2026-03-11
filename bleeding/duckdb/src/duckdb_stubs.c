#include <string.h>
#include <stdatomic.h>

#include <caml/alloc.h>
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
    /* release reference to database */
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
    if (w->valid) {
      duckdb_destroy_result(&w->result);
    }
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
    path = NULL;  /* in-memory */
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

  /* Increment db ref count */
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

/* ── Library version ───────────────────────────────────────────────── */

CAMLprim value caml_duckdb_library_version(value v_unit) {
  (void)v_unit;
  return caml_copy_string(duckdb_library_version());
}
