open! Core
include On_cleanup_error_intf

type 'return t =
  | Call of (exn -> 'return)
  | Ignore
  | Log
  | Raise
[@@deriving sexp_of, typed_variants]

let message = "Error while cleaning up temporary file or directory"

let on_cleanup_error t ~backtrace ~exn ~log_s ~path ~return =
  match t with
  | Call f -> f exn
  | Ignore -> return ()
  | Log ->
    log_s
      [%message
        message
          (path : File_path.Absolute.t)
          (exn : Exn.t)
          (backtrace : (Backtrace.t option[@sexp.option]))]
  | Raise ->
    (match backtrace with
     | Some backtrace -> Exn.raise_with_original_backtrace exn backtrace
     | None -> Exn.reraise exn message)
;;
