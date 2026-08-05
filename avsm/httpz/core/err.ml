(* err.ml - Error handling combinators for HTTP parsing *)

(* Re-export status type for convenience *)
type status = Buf_read.status =
  | Complete
  | Partial
  | Invalid_method
  | Invalid_target
  | Invalid_version
  | Invalid_header
  | Headers_too_large
  | Malformed
  | Content_length_overflow
  | Ambiguous_framing
  | Bare_cr_detected
  | Missing_host_header
  | Unsupported_transfer_encoding

(* Core exception *)
exception Parse_error of status

(* Basic fail - raise with specific status *)
let[@inline] fail status = raise (Parse_error status)

(* Common status shortcuts *)
let[@inline] partial () = raise (Parse_error Partial)
let[@inline] malformed () = raise (Parse_error Malformed)

(* Conditional raises - raise if condition is TRUE *)
let[@inline] when_ cond status = if cond then raise (Parse_error status)
let[@inline] partial_when cond = if cond then raise (Parse_error Partial)
let[@inline] malformed_when cond = if cond then raise (Parse_error Malformed)

(* Guard - raise if condition is FALSE (i.e., require condition to be true) *)
let[@inline] guard cond status = if not cond then raise (Parse_error status)
let[@inline] partial_unless cond = if not cond then raise (Parse_error Partial)
let[@inline] malformed_unless cond = if not cond then raise (Parse_error Malformed)

(* Optional: try parser, return Null on failure, restore pos via callback *)
let[@inline] optional ~(save : unit -> 'pos) ~(restore : 'pos -> unit) (f : unit -> 'a) : 'a or_null =
  let saved = save () in
  match f () with
  | v -> This v
  | exception Parse_error _ -> restore saved; Null
