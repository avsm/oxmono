type t = Chdb_raw.result_handle

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let to_string t = Chdb_raw.result_to_string t

let to_bigstring t : bigstring = Chdb_raw.result_to_bigstring t

let[@inline always] length t = Chdb_raw.result_length t

let[@inline always] is_empty t = Chdb_raw.result_length t = 0

let error t =
  let s = Chdb_raw.result_error t in
  if String.length s = 0 then None else Some s

let[@inline always] has_error t = Chdb_raw.result_has_error t

let[@inline always] elapsed t = Chdb_raw.result_elapsed t

let[@inline always] rows_read t = Chdb_raw.result_rows_read t

let[@inline always] bytes_read t = Chdb_raw.result_bytes_read t

let[@inline always] storage_rows_read t = Chdb_raw.result_storage_rows_read t

let[@inline always] storage_bytes_read t = Chdb_raw.result_storage_bytes_read t

let get_stats t = Chdb_stats.of_result_handle t

let get_stats_local t = exclave_ (Chdb_stats.of_result_handle_local t)

let destroy t = Chdb_raw.destroy_result t

let of_raw (h : Chdb_raw.result_handle) : t = h

let check_error t =
  if Chdb_raw.result_has_error t then begin
    let err = Chdb_raw.result_error t in
    Chdb_raw.destroy_result t;
    Chdb_error.raise_query_failed err
  end
