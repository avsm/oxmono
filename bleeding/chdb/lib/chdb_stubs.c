/* chdb_stubs.c -- OCaml C stubs for chdb (ClickHouse Embedded Database)

   Wraps the chdb C API for use from OCaml, providing:
   - Connection lifecycle (custom blocks with finalizers)
   - Query execution (binary-safe via chdb_query_n)
   - Zero-copy result access (Bigarray wrapping with CAML_BA_EXTERNAL)
   - Unboxed statistics access (no allocation for hot-path metrics)
   - Streaming query support (fetch/cancel)
   - Local allocation for stats records (OxCaml caml_alloc_local)

   Memory: uses caml_stat_alloc/caml_stat_free for temporary C buffers
   (raises on OOM, no NULL check needed). Custom blocks use heap
   allocation with finalizers since they need to outlive the call.

   Domain safety: heavyweight C calls (connect, close, query, stream)
   release the OCaml domain lock via caml_enter/leave_blocking_section
   so other domains can run concurrently. OCaml strings are copied to
   caml_stat memory before entering the blocking section since the GC
   may move OCaml heap objects while the lock is released. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <caml/threads.h>

#include <chdb.h>
#include <string.h>

/* ---------- Connection custom block ---------- */

/* The connection custom block stores a chdb_connection * (pointer returned
   by chdb_connect). We use explicit close rather than finalizer-based
   cleanup because ClickHouse connections are heavyweight. */

static void finalize_connection(value v) {
    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v);
    if (*conn_pp != NULL) {
        chdb_close_conn(*conn_pp);
        *conn_pp = NULL;
    }
}

static struct custom_operations connection_ops = {
    "chdb.connection",
    finalize_connection,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* ---------- Result custom block ---------- */

static void finalize_result(value v) {
    chdb_result **result_pp = (chdb_result **)Data_custom_val(v);
    if (*result_pp != NULL) {
        chdb_destroy_query_result(*result_pp);
        *result_pp = NULL;
    }
}

static struct custom_operations result_ops = {
    "chdb.result",
    finalize_result,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default,
    custom_fixed_length_default
};

/* ---------- Connection management ---------- */

CAMLprim value caml_chdb_connect(value v_args) {
    CAMLparam1(v_args);
    CAMLlocal1(v_conn);

    int argc = Wosize_val(v_args);

    /* Use caml_stat_alloc for temporary argv array.
       caml_stat_alloc raises on OOM (no NULL check needed). */
    char **argv = (char **)caml_stat_alloc((argc + 1) * sizeof(char *));

    for (int i = 0; i < argc; i++) {
        const char *s = String_val(Field(v_args, i));
        size_t len = caml_string_length(Field(v_args, i));
        argv[i] = (char *)caml_stat_alloc(len + 1);
        memcpy(argv[i], s, len);
        argv[i][len] = '\0';
    }
    argv[argc] = NULL;

    /* argv is now in caml_stat memory, safe across blocking section */
    caml_enter_blocking_section();
    chdb_connection *conn_p = chdb_connect(argc, argv);
    caml_leave_blocking_section();

    for (int i = 0; i < argc; i++) caml_stat_free(argv[i]);
    caml_stat_free(argv);

    if (conn_p == NULL) {
        CAMLreturn(Val_null);
    }

    v_conn = caml_alloc_custom(&connection_ops,
                               sizeof(chdb_connection *), 0, 1);
    *(chdb_connection **)Data_custom_val(v_conn) = conn_p;

    CAMLreturn(v_conn);  /* or_null: This(conn) = conn directly */
}

CAMLprim value caml_chdb_close_conn(value v_conn) {
    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    chdb_connection *conn_p = *conn_pp;
    if (conn_p != NULL) {
        *conn_pp = NULL;
        caml_enter_blocking_section();
        chdb_close_conn(conn_p);
        caml_leave_blocking_section();
    }
    return Val_unit;
}

CAMLprim value caml_chdb_conn_is_open(value v_conn) {
    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    return Val_bool(*conn_pp != NULL);
}

/* ---------- Query execution ---------- */

/* Binary-safe query with explicit lengths */
CAMLprim value caml_chdb_query(value v_conn, value v_query, value v_format) {
    CAMLparam3(v_conn, v_query, v_format);
    CAMLlocal1(v_result);

    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    if (*conn_pp == NULL) {
        caml_failwith("chdb_query: connection closed");
    }

    /* Dereference to get chdb_connection (which is a pointer type) */
    chdb_connection conn = *(*conn_pp);

    /* Copy OCaml strings to C memory before releasing domain lock --
       the GC may relocate OCaml heap objects during blocking section */
    size_t query_len = caml_string_length(v_query);
    char *query = (char *)caml_stat_alloc(query_len + 1);
    memcpy(query, String_val(v_query), query_len);
    query[query_len] = '\0';

    size_t format_len = caml_string_length(v_format);
    char *format = (char *)caml_stat_alloc(format_len + 1);
    memcpy(format, String_val(v_format), format_len);
    format[format_len] = '\0';

    caml_enter_blocking_section();
    chdb_result *result = chdb_query_n(conn, query, query_len,
                                       format, format_len);
    caml_leave_blocking_section();

    caml_stat_free(query);
    caml_stat_free(format);

    if (result == NULL) {
        CAMLreturn(Val_null);
    }

    v_result = caml_alloc_custom(&result_ops,
                                 sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(v_result) = result;

    CAMLreturn(v_result);  /* or_null: This(result) = result directly */
}

/* ---------- Result access ---------- */

/* Return result data as a string (copies data) */
CAMLprim value caml_chdb_result_to_string(value v_result) {
    CAMLparam1(v_result);
    CAMLlocal1(v_str);

    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) caml_failwith("chdb_result: already destroyed");

    char *buf = chdb_result_buffer(result);
    size_t len = chdb_result_length(result);

    if (buf == NULL || len == 0) {
        v_str = caml_alloc_string(0);
    } else {
        v_str = caml_alloc_string(len);
        memcpy(Bytes_val(v_str), buf, len);
    }

    CAMLreturn(v_str);
}

/* Return result data as a zero-copy Bigarray (CAML_BA_EXTERNAL) */
CAMLprim value caml_chdb_result_to_bigstring(value v_result) {
    CAMLparam1(v_result);
    CAMLlocal1(v_ba);

    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) caml_failwith("chdb_result: already destroyed");

    char *buf = chdb_result_buffer(result);
    size_t len = chdb_result_length(result);

    if (buf == NULL || len == 0) {
        /* Return empty bigarray */
        v_ba = caml_ba_alloc_dims(
            CAML_BA_UINT8 | CAML_BA_C_LAYOUT, 1, NULL, 0);
    } else {
        /* Wrap existing buffer - NO COPY.
           CAML_BA_EXTERNAL means OCaml does not own the memory. */
        v_ba = caml_ba_alloc_dims(
            CAML_BA_UINT8 | CAML_BA_C_LAYOUT | CAML_BA_EXTERNAL,
            1, buf, (intnat)len);
    }

    CAMLreturn(v_ba);
}

/* Result length - no allocation */
CAMLprim value caml_chdb_result_length(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return Val_long(0);
    return Val_long((long)chdb_result_length(result));
}

/* Error message - returns empty string if no error */
CAMLprim value caml_chdb_result_error(value v_result) {
    CAMLparam1(v_result);
    CAMLlocal1(v_str);

    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) {
        v_str = caml_alloc_string(0);
        CAMLreturn(v_str);
    }

    const char *err = chdb_result_error(result);
    if (err == NULL || err[0] == '\0') {
        v_str = caml_alloc_string(0);
    } else {
        size_t len = strlen(err);
        v_str = caml_alloc_string(len);
        memcpy(Bytes_val(v_str), err, len);
    }

    CAMLreturn(v_str);
}

/* Has error? No allocation. */
CAMLprim value caml_chdb_result_has_error(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return Val_false;
    const char *err = chdb_result_error(result);
    return Val_bool(err != NULL && err[0] != '\0');
}

/* --- Unboxed statistics access --- */

/* Elapsed time (seconds) - unboxed float */
double caml_chdb_result_elapsed_unboxed(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return 0.0;
    return chdb_result_elapsed(result);
}

CAMLprim value caml_chdb_result_elapsed_boxed(value v_result) {
    return caml_copy_double(caml_chdb_result_elapsed_unboxed(v_result));
}

/* Rows read - unboxed int64 */
int64_t caml_chdb_result_rows_read_unboxed(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return 0;
    return (int64_t)chdb_result_rows_read(result);
}

CAMLprim value caml_chdb_result_rows_read_boxed(value v_result) {
    return caml_copy_int64(caml_chdb_result_rows_read_unboxed(v_result));
}

/* Bytes read - unboxed int64 */
int64_t caml_chdb_result_bytes_read_unboxed(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return 0;
    return (int64_t)chdb_result_bytes_read(result);
}

CAMLprim value caml_chdb_result_bytes_read_boxed(value v_result) {
    return caml_copy_int64(caml_chdb_result_bytes_read_unboxed(v_result));
}

/* Storage rows read */
int64_t caml_chdb_result_storage_rows_read_unboxed(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return 0;
    return (int64_t)chdb_result_storage_rows_read(result);
}

CAMLprim value caml_chdb_result_storage_rows_read_boxed(value v_result) {
    return caml_copy_int64(caml_chdb_result_storage_rows_read_unboxed(v_result));
}

/* Storage bytes read */
int64_t caml_chdb_result_storage_bytes_read_unboxed(value v_result) {
    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);
    if (result == NULL) return 0;
    return (int64_t)chdb_result_storage_bytes_read(result);
}

CAMLprim value caml_chdb_result_storage_bytes_read_boxed(value v_result) {
    return caml_copy_int64(caml_chdb_result_storage_bytes_read_unboxed(v_result));
}

/* --- Local stats record ---

   Returns all five statistics fields as a locally-allocated 5-field
   record (flat tuple).  The float and int64 fields are boxed inside
   the record, but the record itself lives on the local stack frame
   so it can be destructured without a minor-heap allocation.

   OCaml declaration:
     external get_stats_local
       : result_handle -> local_ stats_record
       = "caml_chdb_result_stats_local"
*/
CAMLprim value caml_chdb_result_stats_local(value v_result) {
    CAMLparam1(v_result);

    chdb_result *result = *(chdb_result **)Data_custom_val(v_result);

    double elapsed = 0.0;
    int64_t rows_read = 0, bytes_read = 0;
    int64_t storage_rows_read = 0, storage_bytes_read = 0;

    if (result != NULL) {
        elapsed = chdb_result_elapsed(result);
        rows_read = (int64_t)chdb_result_rows_read(result);
        bytes_read = (int64_t)chdb_result_bytes_read(result);
        storage_rows_read = (int64_t)chdb_result_storage_rows_read(result);
        storage_bytes_read = (int64_t)chdb_result_storage_bytes_read(result);
    }

    /* Allocate the record on the local region (stack).
       5 fields: elapsed (boxed float), rows_read (boxed int64),
       bytes_read (boxed int64), storage_rows_read (boxed int64),
       storage_bytes_read (boxed int64). */
    value v_rec = caml_alloc_local(5, 0);

    /* The boxed values inside the record are heap-allocated, but
       the record shell is local. */
    Store_field(v_rec, 0, caml_copy_double(elapsed));
    Store_field(v_rec, 1, caml_copy_int64(rows_read));
    Store_field(v_rec, 2, caml_copy_int64(bytes_read));
    Store_field(v_rec, 3, caml_copy_int64(storage_rows_read));
    Store_field(v_rec, 4, caml_copy_int64(storage_bytes_read));

    CAMLreturn(v_rec);
}

/* Explicit result destruction */
CAMLprim value caml_chdb_destroy_result(value v_result) {
    chdb_result **result_pp = (chdb_result **)Data_custom_val(v_result);
    if (*result_pp != NULL) {
        chdb_destroy_query_result(*result_pp);
        *result_pp = NULL;
    }
    return Val_unit;
}

/* ---------- Streaming query ---------- */

CAMLprim value caml_chdb_stream_query(value v_conn, value v_query,
                                      value v_format) {
    CAMLparam3(v_conn, v_query, v_format);
    CAMLlocal1(v_result);

    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    if (*conn_pp == NULL) {
        caml_failwith("chdb_stream_query: connection closed");
    }

    chdb_connection conn = *(*conn_pp);

    /* Copy OCaml strings to C memory before releasing domain lock */
    size_t query_len = caml_string_length(v_query);
    char *query = (char *)caml_stat_alloc(query_len + 1);
    memcpy(query, String_val(v_query), query_len);
    query[query_len] = '\0';

    size_t format_len = caml_string_length(v_format);
    char *format = (char *)caml_stat_alloc(format_len + 1);
    memcpy(format, String_val(v_format), format_len);
    format[format_len] = '\0';

    caml_enter_blocking_section();
    chdb_result *result = chdb_stream_query_n(conn, query, query_len,
                                              format, format_len);
    caml_leave_blocking_section();

    caml_stat_free(query);
    caml_stat_free(format);

    if (result == NULL) {
        CAMLreturn(Val_null);
    }

    v_result = caml_alloc_custom(&result_ops,
                                 sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(v_result) = result;

    CAMLreturn(v_result);
}

CAMLprim value caml_chdb_stream_fetch(value v_conn, value v_stream) {
    CAMLparam2(v_conn, v_stream);
    CAMLlocal1(v_result);

    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    chdb_result *stream = *(chdb_result **)Data_custom_val(v_stream);

    if (*conn_pp == NULL || stream == NULL) {
        CAMLreturn(Val_null);
    }

    chdb_connection conn = *(*conn_pp);

    caml_enter_blocking_section();
    chdb_result *result = chdb_stream_fetch_result(conn, stream);
    caml_leave_blocking_section();

    if (result == NULL) {
        CAMLreturn(Val_null);
    }

    /* Check for empty result (stream end) */
    size_t len = chdb_result_length(result);
    if (len == 0) {
        chdb_destroy_query_result(result);
        CAMLreturn(Val_null);
    }

    v_result = caml_alloc_custom(&result_ops,
                                 sizeof(chdb_result *), 0, 1);
    *(chdb_result **)Data_custom_val(v_result) = result;

    CAMLreturn(v_result);
}

CAMLprim value caml_chdb_stream_cancel(value v_conn, value v_stream) {
    chdb_connection **conn_pp = (chdb_connection **)Data_custom_val(v_conn);
    chdb_result *stream = *(chdb_result **)Data_custom_val(v_stream);

    if (*conn_pp != NULL && stream != NULL) {
        chdb_connection conn = *(*conn_pp);
        caml_enter_blocking_section();
        chdb_stream_cancel_query(conn, stream);
        caml_leave_blocking_section();
    }
    return Val_unit;
}
