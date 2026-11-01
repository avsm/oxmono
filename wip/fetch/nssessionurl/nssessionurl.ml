open Eio.Std

external is_available : unit -> bool = "ocaml_nsurl_available" [@@noalloc]

let available = is_available ()

module Session = struct
  type t

  type cache_policy =
    | Use_protocol_cache_policy
    | Reload_ignoring_local_cache
    | Return_cache_data_else_load
    | Return_cache_data_dont_load
    | Reload_revalidating_cache

  type cookie_accept_policy = Always | Never | Only_from_main_document_domain

  type tls_version = TLS_1_2 | TLS_1_3

  (* Field order and constructor numbering must match
     ocaml_nsurl_session_create in urlsession_stubs.c. *)
  type config = {
    ephemeral : bool;
    request_timeout : float option;
    resource_timeout : float option;
    max_connections_per_host : int option;
    allows_cellular : bool option;
    allows_expensive : bool option;
    allows_constrained : bool option;
    waits_for_connectivity : bool option;
    cache_policy : cache_policy option;
    set_cookies : bool option;
    cookie_accept_policy : cookie_accept_policy option;
    min_tls : tls_version option;
    additional_headers : (string * string) array;
  }
  [@@warning "-69"] (* fields are read by the C stub *)

  external create_with : config -> t = "ocaml_nsurl_session_create"

  let create ?(ephemeral = false) ?request_timeout ?resource_timeout
      ?max_connections_per_host ?allows_cellular ?allows_expensive
      ?allows_constrained ?waits_for_connectivity ?cache_policy ?set_cookies
      ?cookie_accept_policy ?min_tls ?(additional_headers = []) () =
    create_with
      { ephemeral; request_timeout; resource_timeout; max_connections_per_host;
        allows_cellular; allows_expensive; allows_constrained;
        waits_for_connectivity; cache_policy; set_cookies;
        cookie_accept_policy; min_tls;
        additional_headers = Array.of_list additional_headers }
end

(* All fetches share one session (hence one connection pool) unless the
   caller supplies their own. *)
let default_session = lazy (Session.create ())

type info = { status : int; headers : (string * string) list }

type body =
  | Fixed of string
  | Stream : _ Eio.Flow.source -> body

(* Field order must match make_error in nssessionurl_stubs.c. *)
type error = { domain : string; code : int; message : string }

exception Error of error

let () =
  Printexc.Safe.register_printer (function
      | Error { domain; code; message } ->
        Some (Printf.sprintf "Nssessionurl.Error(%s %d: %s)" domain code message)
      | _ -> None)

type handle

(* Constructed by the C stubs, so the unused-constructor warning is wrong;
   the layout here must match ocaml_nsurl_poll_info. *)
type info_poll =
  | Pending
  | Info of int * (string * string) array
  | Failed of error
[@@warning "-37"]

(* Each pipe's write fd's ownership transfers to the C side on these calls:
   the delegate closes fd, the upload pump closes fd2 (or the failure path
   of the stub closes both).  The OCaml side must only ever close read ends. *)
external start_data :
  Session.t -> string -> string -> (string * string) array -> string option ->
  bool -> Unix.file_descr -> int -> handle
  = "ocaml_nsurl_start_data_byte" "ocaml_nsurl_start_data"

external start_stream :
  Session.t -> string -> string -> (string * string) array -> bool ->
  Unix.file_descr -> Unix.file_descr -> int -> handle
  = "ocaml_nsurl_start_stream_byte" "ocaml_nsurl_start_stream"

external poll_info : handle -> info_poll = "ocaml_nsurl_poll_info"

(* >0 bytes copied, 0 would-block, -1 EOF, -2 error pending. *)
external read_into : handle -> Cstruct.buffer -> int -> int -> int
  = "ocaml_nsurl_read" [@@noalloc]

(* >0 bytes accepted, 0 buffer full, -1 upload closed/broken.
   Not noalloc: raises Out_of_memory. *)
external write_body : handle -> Cstruct.buffer -> int -> int -> int
  = "ocaml_nsurl_write_body"

external close_body : handle -> unit = "ocaml_nsurl_close_body" [@@noalloc]

external error_info : handle -> error = "ocaml_nsurl_error_info"

external cancel : handle -> unit = "ocaml_nsurl_cancel" [@@noalloc]

let drain fd =
  let len = 64 in
  let b = Bytes.create len in
  let rec go () =
    match Unix.read fd b 0 len with
    | n when n = len -> go ()
    | _ -> ()
    | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> ()
  in
  go ()

(* Poll, then drain the signal pipe, then poll again before suspending: a
   state change after the second poll leaves a fresh byte in the pipe, so
   the wakeup cannot be lost. *)
let rec wait fd poll =
  match poll () with
  | Some v -> v
  | None ->
    drain fd;
    match poll () with
    | Some v -> v
    | None -> Eio_unix.await_readable fd; wait fd poll

type stream = { h : handle; r : Unix.file_descr }

module Body_flow = struct
  type t = stream

  let read_methods = []

  let single_read t (buf : Cstruct.t @ local) =
    let buffer = buf.buffer and off = buf.off and len = buf.len in
    wait t.r (fun () ->
        match read_into t.h buffer off len with
        | 0 -> None
        | n when n > 0 -> Some n
        | -1 -> raise End_of_file
        | -2 -> raise (Error (error_info t.h))
        | _ -> assert false)
end

let body_handler = Eio.Flow.Pi.source (module Body_flow)

(* Copy the user's source into the C-side upload queue.  A negative
   write_body result means the task is dead (cancelled, or the server
   stopped reading); the response path reports the failure, so just stop
   feeding quietly. *)
let feed_body h fd src =
  let exception Upload_closed in
  let buf = Cstruct.create 65536 in
  let rec push n ofs =
    if ofs < n then
      match
        wait fd (fun () ->
            match write_body h buf.buffer (buf.off + ofs) (n - ofs) with
            | 0 -> None
            | k -> Some k)
      with
      | sent when sent > 0 -> push n (ofs + sent)
      | _ -> raise Upload_closed
  in
  try
    while true do
      push (Eio.Flow.single_read src buf) 0
    done
  with
  | End_of_file -> close_body h
  | Upload_closed -> ()

let fetch ~sw ?session ?(meth = "GET") ?(headers = []) ?body
    ?(max_buffer = 1 lsl 20) ?(follow_redirects = true) url =
  let session =
    match session with Some s -> s | None -> Lazy.force default_session
  in
  let hdrs = Array.of_list headers in
  let r, w = Unix.pipe ~cloexec:true () in
  Unix.set_nonblock r;
  let buffered data =
    try start_data session url meth hdrs data follow_redirects w max_buffer
    with e -> Unix.close r; raise e
  in
  let h =
    match body with
    | None -> buffered None
    | Some (Fixed data) -> buffered (Some data)
    | Some (Stream src) ->
      let r2, w2 = Unix.pipe ~cloexec:true () in
      Unix.set_nonblock r2;
      let h =
        try start_stream session url meth hdrs follow_redirects w w2 max_buffer
        with e -> Unix.close r; Unix.close r2; raise e
      in
      Switch.on_release sw (fun () -> Unix.close r2);
      Fiber.fork ~sw (fun () -> feed_body h r2 src);
      h
  in
  let t = { h; r } in
  Switch.on_release sw (fun () -> cancel h; Unix.close r);
  let info =
    match
      wait r (fun () ->
          match poll_info h with Pending -> None | i -> Some i)
    with
    | Info (status, hdrs) -> { status; headers = Array.to_list hdrs }
    | Failed msg -> raise (Error msg)
    | Pending -> assert false
  in
  info, Eio.Resource.T (t, body_handler)

let get ~sw ?session ?headers url = fetch ~sw ?session ?headers url

let post ~sw ?session ?headers ~body url =
  fetch ~sw ?session ~meth:"POST" ?headers ~body url

let fetch_into ?session ?meth ?headers ?body ?max_buffer ?follow_redirects
    ~sink url =
  Switch.run @@ fun sw ->
  let info, src =
    fetch ~sw ?session ?meth ?headers ?body ?max_buffer ?follow_redirects url
  in
  Eio.Flow.copy src sink;
  info

let fetch_string ?session ?meth ?headers ?body ?max_buffer ?follow_redirects
    url =
  Switch.run @@ fun sw ->
  let info, src =
    fetch ~sw ?session ?meth ?headers ?body ?max_buffer ?follow_redirects url
  in
  info, Eio.Flow.read_all src
