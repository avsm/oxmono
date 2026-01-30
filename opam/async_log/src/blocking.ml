open! Core
open! Async_kernel
open! Import

(* This module is intended to be blocking, so we explicitly don't open [Async] at the top.
*)

module Output = struct
  type t = Message.t -> unit

  let create = Fn.id
  let write print msg = print (Message.to_write_only_text msg)
  let stdout = write (Core.Printf.printf "%s\n%!")
  let stderr = write (Core.Printf.eprintf "%s\n%!")
end

let level : Level.t ref = ref `Info
let write = ref Output.stderr

let time_source =
  ref
    (if Ppx_inline_test_lib.am_running
     then Synchronous_time_source.(read_only (create ~now:Time_ns.epoch ()))
     else Synchronous_time_source.wall_clock ())
;;

let transform = ref None
let set_level l = level := l
let level () = !level
let set_output output = write := output
let set_time_source ts = time_source := ts
let set_transform f = transform := f

let write msg =
  if Async_unix.Scheduler.is_running ()
  then failwith "Log.Global.Blocking function called after scheduler started";
  let msg =
    match !transform with
    | None -> msg
    | Some f -> f msg
  in
  !write msg
;;

let would_log msg_level =
  (* we don't need to test for empty output here because the interface only allows one
     Output.t and ensures that it is always set to something. *)
  Level.as_or_more_verbose_than ~log_level:(level ()) ~msg_level
;;

let create_message ?level ?time ?tags msg =
  let time =
    match time with
    | None ->
      Synchronous_time_source.now !time_source |> Time_ns.to_time_float_round_nearest
    | Some time -> time
  in
  Message.create ?level ?tags msg ~time
;;

let gen ?level:msg_level ?time ?tags k =
  ksprintf
    (fun msg ->
      if would_log msg_level
      then (
        let msg = `String msg in
        write (create_message ?level:msg_level ?time ?tags msg)))
    k
;;

let printf ?level ?time ?tags k = gen ?level ?time ?tags k
let raw ?time ?tags k = gen ?time ?tags k
let debug ?time ?tags k = gen ~level:`Debug ?time ?tags k
let info ?time ?tags k = gen ~level:`Info ?time ?tags k
let error ?time ?tags k = gen ~level:`Error ?time ?tags k

let sexp ?level ?time ?tags sexp =
  if would_log level then write (create_message ?level ?time ?tags (`Sexp sexp))
;;

let raw_s ?time ?tags the_sexp = sexp ?time ?tags the_sexp
let debug_s ?time ?tags the_sexp = sexp ~level:`Debug ?time ?tags the_sexp
let info_s ?time ?tags the_sexp = sexp ~level:`Info ?time ?tags the_sexp
let error_s ?time ?tags the_sexp = sexp ~level:`Error ?time ?tags the_sexp
