(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private

let ( let* ) = Result.bind
let src = Logs.Src.create "okit.session"

module Log = (val Logs.src_log src : Logs.LOG)

(* A server's own output is not this library's to print, and the terminal
   belongs to the caller. The last of it is kept instead, because a server that
   fails to start explains itself there and nowhere else. *)
module Tail = struct
  let size = 4096

  type t = { buf : Bytes.t; mutable len : int; mutable next : int }

  let create () = { buf = Bytes.create size; len = 0; next = 0 }

  let add t s =
    let n = String.length s in
    let off = if n > size then n - size else 0 in
    let n = n - off in
    let first = min n (size - t.next) in
    Bytes.blit_string s off t.buf t.next first;
    Bytes.blit_string s (off + first) t.buf 0 (n - first);
    t.next <- (t.next + n) mod size;
    t.len <- min size (t.len + n)

  let contents t =
    if t.len < size then Bytes.sub_string t.buf 0 t.len
    else
      Bytes.sub_string t.buf t.next (size - t.next)
      ^ Bytes.sub_string t.buf 0 t.next
end

let with_tail tail msg =
  match String.trim (Tail.contents tail) with
  | "" -> msg
  | out -> Printf.sprintf "%s\nThe server's last output was:\n%s" msg out

(* A daemon fiber, so that a session left open does not keep the switch from
   finishing. Every tail in [tails] gets the same bytes: the session's spans
   every server it has had, and each server has one of its own, so that a
   failure can be named by the server that failed.

   [source] is closed once it ends, rather than left to the switch. A session
   relaunches a server as often as one dies, and a switch that outlives them all
   would hold a descriptor for each. *)
let drain ~sw source tails =
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let r = Eio.Buf_read.of_flow source ~max_size:0x10000 in
      let rec go () =
        if Eio.Buf_read.at_end_of_input r then ()
        else begin
          let s = Eio.Buf_read.take (Eio.Buf_read.buffered_bytes r) r in
          List.iter (fun tail -> Tail.add tail s) tails;
          go ()
        end
      in
      (try go () with End_of_file | Eio.Io _ -> ());
      (try Eio.Resource.close source with Eio.Io _ | Invalid_argument _ -> ());
      `Stop_daemon)

(* Dune declares [build] in src/dune_rpc_impl/decl.ml and does not export it, so
   a client that wants it declares the same thing and offers it in the menu it
   negotiates. Version 2 only: a server too old for it drops the method rather
   than serving a version 1 payload this cannot decode. *)
let build_decl =
  Decl.Request.make
    ~method_:(Method.Name.of_string "build")
    ~generations:
      [
        Decl.Request.make_current_gen ~req:(Conv.list Conv.string)
          ~resp:Build_outcome_with_diagnostics.sexp_v2 ~version:2;
      ]

(* Everything the session sends, prepared once against the negotiated menu. A
   method the server dropped fails here rather than at the call that needed
   it. *)
type calls = {
  build :
    ( string list,
      Build_outcome_with_diagnostics.t )
    Dune_rpc_eio.Client.Versioned.request;
  runtest :
    ( string list,
      Build_outcome_with_diagnostics.t )
    Dune_rpc_eio.Client.Versioned.request;
  diagnostics : (unit, Diagnostic.t list) Dune_rpc_eio.Client.Versioned.request;
  flush :
    (unit, [ `Ok | `Not_in_watch_mode ]) Dune_rpc_eio.Client.Versioned.request;
  promote : (Path.t, unit) Dune_rpc_eio.Client.Versioned.request;
}

type conn = {
  client : Dune_rpc_eio.Client.t;
  calls : calls;
  stop : unit Eio.Promise.u;
}

let prepare c =
  let ( let* ) x f = Result.bind (Result.map_error Version_error.message x) f in
  let* build =
    Dune_rpc_eio.Client.Versioned.prepare_request c
      (Decl.Request.witness build_decl)
  in
  let* runtest =
    Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.runtest
  in
  let* diagnostics =
    Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.diagnostics
  in
  let* flush =
    Dune_rpc_eio.Client.Versioned.prepare_request c
      Public.Request.flush_file_watcher
  in
  let* promote =
    Dune_rpc_eio.Client.Versioned.prepare_request c Public.Request.promote
  in
  Ok { build; runtest; diagnostics; flush; promote }

(* A log notification describes work rather than a result, which a caller
   waiting on one has no use for. An abort is the server ending the session, and
   the handler dune-rpc installs by default raises it on the connection's own
   fiber, where nothing would answer the request a caller is parked on. Reading
   on lets the connection end instead, which is what fails that request. *)
let handler =
  Dune_rpc_eio.Client.Handler.create
    ~log:(fun (m : Message.t) -> Log.debug (fun f -> f "dune: %s" m.message))
    ~abort:(fun (m : Message.t) ->
      Log.err (fun f -> f "the dune server ended the session: %s" m.message))
    ()

(* A relaunch ends the connection it replaces, and {!stop} ends the one the
   session finished on. A relaunch that failed leaves those two the same, so the
   end must be safe to ask for twice. *)
let disconnect conn = ignore (Eio.Promise.try_resolve conn.stop ())

(* [connect_with_menu] runs its callback and returns, and a session outlives any
   one call, so the connection lives on a fiber of its own. That fiber hands
   back the client and then parks until {!disconnect}, which is what closes the
   connection. A daemon fiber, since a switch waits for the fibers it owns and
   would otherwise wait for this one past the release handler that stops it. *)
let attach ~sw ~client:id chan =
  let ready, set_ready = Eio.Promise.create () in
  let stopping, stop = Eio.Promise.create () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
      let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom id)) in
      let private_menu = [ Dune_rpc_eio.Client.Request build_decl ] in
      (match
         Dune_rpc_eio.Client.connect_with_menu ~handler ~private_menu chan init
           ~f:(fun c ->
             match prepare c with
             | Error e -> Eio.Promise.resolve set_ready (Error e)
             | Ok calls ->
                 Eio.Promise.resolve set_ready (Ok { client = c; calls; stop });
                 Eio.Promise.await stopping)
       with
      | () -> ()
      | exception e ->
          (* Either the handshake failed, which is what the caller is waiting to
             hear, or the switch cancelled a session that already had its
             client, which is nobody's to hear. *)
          ignore
            (Eio.Promise.try_resolve set_ready (Error (Printexc.to_string e))));
      `Stop_daemon);
  Eio.Promise.await ready

(* A caller shows a trace line in a column a few dozen characters wide. The rest
   of a message still reaches it, in the result. *)
let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* A session traces from inside the critical section that serialises its
   requests, and an exception out of there disables the mutex, after which every
   later call raises [Eio.Mutex.Poisoned] rather than reporting what went wrong.
   Cancellation is a fiber ending rather than a fault of the callback's, and it
   is passed on. *)
let guarded trace s =
  try trace s with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ()

type t = {
  dir : string;
  mutable owns : bool;
      (** Whether this session started the server it talks to. A relaunch may
          land on one another program started, which is not this session's to
          stop. *)
  relaunch : unit -> (conn * bool, string) result;
  tail : Tail.t;
  lock : Eio.Mutex.t;
  mutable conn : conn;
  mutable stopped : bool;
  trace : string -> unit;
  now : unit -> float;
      (** The session's clock, read either side of a request so that a step can
          say how long the server took. *)
}

type build = { ok : bool; diagnostics : Diagnostic.t list }

let traced t r =
  (match r with
  | Error e -> t.trace ("dune: error " ^ first_line e)
  | Ok _ -> ());
  r

let gone t ~what =
  if t.owns then
    with_tail t.tail
      (Printf.sprintf
         "the dune server this session started for %s exited while answering \
          %s, and so did the one started in its place."
         t.dir what)
  else
    Printf.sprintf
      "the dune server that holds %s went away while answering %s. This \
       session did not start that server and will not start another for a \
       workspace it does not own. Start one with dune build \
       --passive-watch-mode in %s."
      t.dir what t.dir

let stopped_error t =
  Printf.sprintf "the session with the dune server for %s is stopped" t.dir

let call t ~what f =
  let started = t.now () in
  match f t.conn with
  | Ok x ->
      t.trace (Printf.sprintf "dune: reply %.1fs" (t.now () -. started));
      Ok (`Answered x)
  | Error (e : Response.Error.t) -> (
      match Response.Error.kind e with
      | Response.Error.Connection_dead -> Ok `Gone
      | Invalid_request | Code_error ->
          Error
            (Printf.sprintf "dune refused %s: %s" what
               (Response.Error.message e)))

(* One request and its answer, restarting an owned server that has died. The
   request is sent again on the new connection, since the server that dropped it
   never acted on it. *)
let request t ~what f =
  let* r = call t ~what f in
  match r with
  | `Answered x -> Ok x
  | `Gone ->
      (* The connection also ends when {!stop} closes it, and a session that has
         been stopped is not owed a server. *)
      if t.stopped then Error (stopped_error t)
      else if not t.owns then Error (gone t ~what)
      else begin
        t.trace "dune: server gone, relaunching";
        disconnect t.conn;
        match t.relaunch () with
        | Error e -> Error e
        | Ok (conn, owns) -> (
            t.conn <- conn;
            t.owns <- owns;
            let* r = call t ~what f in
            match r with `Answered x -> Ok x | `Gone -> Error (gone t ~what))
      end

(* Every call holds the session for as long as its exchanges last, so that two
   fibers cannot have requests in flight at once, and so that {!stop} waits for a
   request rather than closing the connection under it. [stopped] is read again
   under the lock, since a stop may have been the thing this call waited for.
   Nothing may leave here as an exception, for the reason {!guarded} gives, which
   is what the last clause holds against dune-rpc: it raises a Stdune Code_error,
   a class this library cannot name, when a reply does not match its
   declaration. *)
let guard t f =
  if t.stopped then Error (stopped_error t)
  else
    Eio.Mutex.use_rw ~protect:true t.lock (fun () ->
        if t.stopped then Error (stopped_error t)
        else
          try f () with
          | Failure m ->
              Error (Printf.sprintf "the dune server for %s: %s" t.dir m)
          | Invalid_argument m ->
              Error
                (Printf.sprintf
                   "the connection to the dune server for %s was closed while \
                    the session was using it: %s"
                   t.dir m)
          | Eio.Cancel.Cancelled _ as e -> raise e
          | e ->
              Error
                (Printf.sprintf "the dune server for %s: %s" t.dir
                   (Printexc.to_string e)))

(* The forms a caller may write, quoted in every refusal below so that a bad
   target is answered with the whole grammar rather than with one alternative. *)
let target_forms =
  "Write a path such as \".\", \"lib\" or \"src/foo.exe\", an alias such as \
   \"@check\" for that directory and every one below it, or an alias such as \
   \"@@check\" for that directory alone. A directory goes in the alias name, \
   as in \"@lib/runtest\"."

(* These characters would end the atom or the list early and hand the server a
   dep-spec other than the one the caller asked for. *)
let bad_in_alias = function
  | ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' -> true
  | _ -> false

let alias form name target =
  if name = "" || String.exists bad_in_alias name then
    Error
      (Printf.sprintf
         "%S is not a target dune will take, because an alias name must not be \
          empty and must hold no whitespace, parenthesis or double quote. %s"
         target target_forms)
  else Ok (Printf.sprintf "(%s %s)" form name)

(* Dune resolves an RPC target as a path or as a dep-spec s-expression naming an
   alias. A session takes dune's command-line spelling and writes the dep-spec
   itself, refusing one written by hand, because the server answers a malformed
   dep-spec with a Code_error that names nothing the caller can act on. *)
let dep_spec target =
  let tail n = String.sub target n (String.length target - n) in
  if String.starts_with ~prefix:"@@" target then alias "alias" (tail 2) target
  else if String.starts_with ~prefix:"@" target then
    alias "alias_rec" (tail 1) target
  else if String.starts_with ~prefix:"(" target then
    Error
      (Printf.sprintf
         "%S is a dep-spec, which this session writes for itself rather than \
          takes. %s"
         target target_forms)
  else Ok target

let dep_specs targets =
  List.fold_right
    (fun target acc ->
      let* acc = acc in
      let* spec = dep_spec target in
      Ok (spec :: acc))
    targets (Ok [])

let count t ds =
  let n = List.length ds in
  t.trace (Printf.sprintf "dune: %d diagnostic%s" n (if n = 1 then "" else "s"))

let ok = function
  | Build_outcome_with_diagnostics.Success -> true
  | Failure _ -> false

let flushed t =
  let* r =
    request t ~what:"flush-file-watcher" (fun c ->
        Dune_rpc_eio.Client.request c.client c.calls.flush ())
  in
  match r with
  | `Ok -> Ok ()
  | `Not_in_watch_mode ->
      Error
        (Printf.sprintf
           "the dune server for %s is not watching, so a build would not see \
            changes made since it started"
           t.dir)

let diagnostics t =
  request t ~what:"diagnostics" (fun c ->
      Dune_rpc_eio.Client.request c.client c.calls.diagnostics ())

let build t ~targets =
  traced t
  @@
  let* targets = dep_specs targets in
  guard t (fun () ->
      t.trace "dune: flush";
      let* () = flushed t in
      t.trace ("dune: build " ^ String.concat " " targets);
      let* outcome =
        request t ~what:"build" (fun c ->
            Dune_rpc_eio.Client.request c.client c.calls.build targets)
      in
      t.trace "dune: diagnostics";
      let* diagnostics = diagnostics t in
      count t diagnostics;
      Ok { ok = ok outcome; diagnostics })

(* The whole workspace is the root directory, which dune tests recursively. An
   empty list of directories is not the same thing: dune runs no test at all and
   still answers Success, so a failing test was reported as a pass. *)
let runtest t =
  traced t
  @@ guard t (fun () ->
      t.trace "dune: flush";
      let* () = flushed t in
      t.trace "dune: runtest";
      let* outcome =
        request t ~what:"runtest" (fun c ->
            Dune_rpc_eio.Client.request c.client c.calls.runtest [ "." ])
      in
      t.trace "dune: diagnostics";
      let* diagnostics = diagnostics t in
      count t diagnostics;
      Ok { ok = ok outcome; diagnostics })

let promote t ~path =
  traced t
  @@ guard t (fun () ->
      t.trace ("dune: promote " ^ path);
      let* () =
        (* Dune resolves a relative path against the workspace it serves, so a
           caller may pass one. *)
        request t ~what:"promote" (fun c ->
            Dune_rpc_eio.Client.request c.client c.calls.promote path)
      in
      Ok ())

(* Stopping waits for a request in flight, because ending the connection under
   one would leave the caller with a torn session to explain and, for a session
   that owns its server, could look like a death worth restarting. It is
   protected from cancellation, since it also runs from the switch's release,
   and it still ends the connection if the mutex has been disabled. *)
let stop t =
  let close () =
    if not t.stopped then begin
      t.stopped <- true;
      (if t.owns then
         match
           Dune_rpc_eio.Client.Versioned.prepare_notification t.conn.client
             Public.Notification.shutdown
         with
         | Ok n -> (
             try Dune_rpc_eio.Client.notification t.conn.client n ()
             with _ -> ())
         | Error e ->
             (* [stop] answers to a switch release as often as to a caller, so
                there is nobody to return this to. A server left holding the
                workspace's build lock will refuse every later dune, so it is
                worth saying. *)
             Log.err (fun f ->
                 f
                   "the dune server this session started for %s is left \
                    running, because its shutdown notification could not be \
                    prepared: %s"
                   t.dir (Version_error.message e)));
      disconnect t.conn
    end
  in
  Eio.Cancel.protect (fun () ->
      try Eio.Mutex.use_rw ~protect:true t.lock close
      with Eio.Mutex.Poisoned _ -> close ())

let trace t s = t.trace s

let dune_on_path ~dir =
  let dirs =
    match Sys.getenv_opt "PATH" with
    | None -> []
    | Some p -> String.split_on_char ':' p
  in
  let exe d =
    Filename.concat (if d = "" then Filename.current_dir_name else d) "dune"
  in
  match List.find_opt (fun d -> Sys.file_exists (exe d)) dirs with
  | Some d -> Ok (exe d)
  | None ->
      Error
        (Printf.sprintf
           "dune is not on PATH, so this session cannot start a server for %s. \
            Install dune 3.24 or later, or start a server yourself with dune \
            build --passive-watch-mode there."
           dir)

(* DUNE_BUILD_DIR moves where a server opens its socket, DUNE_RPC moves where a
   dune looks for one, and INSIDE_DUNE makes a dune take the directory it starts
   in for the workspace root. A program run under dune exec has all three, and a
   child that inherited them would not serve the workspace this session named. *)
let environment () =
  let dropped = [ "INSIDE_DUNE="; "DUNE_BUILD_DIR="; "DUNE_RPC=" ] in
  let keep v =
    not (List.exists (fun prefix -> String.starts_with ~prefix v) dropped)
  in
  Array.of_list (List.filter keep (Array.to_list (Unix.environment ())))

let start ?(trace = ignore) ?(client = "okit") ~sw ~proc ~net ~clock ~root () =
  (* Wrapped once, so that every trace below and on the session it makes is
     already guarded. *)
  let trace = guarded trace in
  (* Eio names a directory it holds a capability on with a trailing separator.
     It comes off here rather than at each use, so that no message below reports
     the workspace /ws as /ws/. *)
  let dir =
    let d = Eio.Path.native_exn root in
    let n = String.length d in
    if n > 1 && d.[n - 1] = '/' then String.sub d 0 (n - 1) else d
  in
  let where =
    Dune_rpc_eio.Where.default ~build_dir:Eio.Path.(root / "_build") ()
  in
  let socket = match where with `Unix p -> p | `Ip _ -> "" in
  let tail = Tail.create () in
  (* A connection refused, or reset as it was made, means nobody is listening,
     and a socket that has gone between the look and the connect says the same.
     Every other failure, a refusal of permission among them, leaves open the
     question of who owns the socket. *)
  let classify e =
    match e with
    | Eio.Io
        ( ( Eio.Net.E
              ( Eio.Net.Connection_failure (Eio.Net.Refused _)
              | Eio.Net.Connection_reset _ )
          | Eio.Fs.E (Eio.Fs.Not_found _) ),
          _ ) ->
        `Refused (Printexc.to_string e)
    | e -> `Other (Printexc.to_string e)
  in
  let why = function `Refused e | `Other e -> e in
  let connect () =
    match Dune_rpc_eio.connect ~sw ~net where with
    | chan -> Ok chan
    | exception (Eio.Io _ as e) -> Error (classify e)
  in
  (* Eio gives the socket to the switch before it connects and leaves it there
     when the connect fails, so an attempt made on the session's switch strands
     a descriptor for as long as the session lasts. A server that is still
     starting refuses every attempt until the last, so the waiting is done on a
     switch of its own, which closes what each attempt left behind. *)
  let answers () =
    Eio.Switch.run @@ fun attempt ->
    match Dune_rpc_eio.connect ~sw:attempt ~net where with
    | _ -> Ok ()
    | exception (Eio.Io _ as e) -> Error (classify e)
  in
  (* A socket that accepts and then never answers is indistinguishable from a
     busy server, and the reads the handshake makes have nothing bounding them,
     so the step is named before it begins rather than after it returns. *)
  let handshake chan =
    trace "dune: handshake";
    match attach ~sw ~client chan with
    | Ok conn -> Ok conn
    | Error e ->
        (* dune-rpc closes the channel once its callback has run, which a
           handshake that failed never reaches. *)
        Dune_rpc_eio.Chan.close chan;
        Error e
  in
  (* A session may spawn more than one dune, so the oldest line in its own tail
     may belong to a server that failed some time ago. Only a tail of the
     child's own can name that child's failure. *)
  let spawn () =
    let* executable = dune_on_path ~dir in
    let out, into = Eio_unix.pipe sw in
    let mine = Tail.create () in
    drain ~sw out [ tail; mine ];
    let child =
      (* An empty standard input rather than this process's own, which may be a
         pipe carrying a protocol of the caller's. A server that read a byte of
         it would take a message meant for the caller. *)
      Eio.Process.spawn ~sw proc ~cwd:root
        ~stdin:(Eio.Flow.string_source "")
        ~stdout:into ~stderr:into ~executable ~env:(environment ())
        [ "dune"; "build"; "--passive-watch-mode" ]
    in
    (* The child has its own copy of the write end by the time [spawn] returns,
       and this process's copy is what keeps the drain above from ever reaching
       the end of the child's output. Held open, it would strand a fiber and two
       descriptors on the session's switch for every server the session
       starts. *)
    (try Eio.Resource.close into with Eio.Io _ | Invalid_argument _ -> ());
    (* A spawn is a fork of this process, and a process that holds a model's
       address space can be minutes in one. Saying that the child exists
       separates that from a server that is merely slow to open its socket. *)
    trace "dune: server spawned";
    Ok (child, mine)
  in
  (* A server the session started and then could not use holds the workspace's
     build lock for as long as it runs, and dune refuses to build a workspace
     another dune holds. Left alive, it would fail every build a caller falling
     back to a shell then asked for, and say nothing about why. *)
  let terminate child =
    Eio.Cancel.protect @@ fun () ->
    try
      Eio.Process.signal child Sys.sigterm;
      match
        Eio.Time.with_timeout clock 5. (fun () -> Ok (Eio.Process.await child))
      with
      | Ok _ -> ()
      | Error `Timeout ->
          Eio.Process.signal child Sys.sigkill;
          ignore (Eio.Process.await child)
    with Eio.Io _ | Invalid_argument _ -> ()
  in
  (* Whether the child the session spawned is still there. Eio resolves a
     child's exit status once it has reaped it, so a status that is ready to
     take is a child that has already gone, and the wait is what a live child
     costs. *)
  let still_running child =
    match
      Eio.Time.with_timeout clock 0.1 (fun () -> Ok (Eio.Process.await child))
    with
    | Ok _ -> false
    | Error `Timeout -> true
  in
  (* How long a server that is still starting has to open its socket. A machine
     whose memory a model has wired takes tens of seconds to start one. *)
  let window = 30. in
  (* How long a socket is still worth waiting for once the child the session
     spawned has gone. Two dunes that start together race for the workspace's
     build lock, and the loser exits at once, so the winner's socket is either
     already there or a moment away. A dune that refuses the workspace outright
     also exits without opening anything, and no wait will produce a socket for
     that one. *)
  let grace = 2. in
  (* A refusal names the instance that holds the workspace on the child's first
     line, which is the only account of it there is. The grace above is what
     gives the drain fiber time to have read that line. *)
  let last_words output =
    match String.trim (Tail.contents output) with
    | "" -> "it said nothing"
    | out -> first_line out
  in
  (* [output] is what this child alone has said, and [gone] is when it was first
     seen to have exited, which shortens what is left of the wait to [grace]. *)
  let wait ~output child =
    let started = Eio.Time.now clock in
    let rec go ~announced ~gone =
      match answers () with
      | Ok () -> (
          match connect () with
          | Ok chan -> Ok chan
          | Error e ->
              Error
                (with_tail tail
                   (Printf.sprintf
                      "the dune server for %s answered at %s and then stopped \
                       answering: %s"
                      dir socket (why e))))
      | Error e -> (
          match gone with
          | Some since ->
              (* Nothing paces this branch but the sleep, since a child that has
                 been reaped is answered at once. *)
              if Eio.Time.now clock -. since >= grace then begin
                let said = last_words output in
                trace ("dune: server exited: " ^ said);
                Error
                  (with_tail tail
                     (Printf.sprintf
                        "the dune this session started in %s exited without \
                         opening its RPC socket at %s: %s"
                        dir socket said))
              end
              else begin
                Eio.Time.sleep clock 0.1;
                go ~announced ~gone
              end
          | None ->
              (* Asking whether the child is there is also what paces the loop
                 for as long as it is. *)
              if not (still_running child) then begin
                trace "dune: server exited, checking for another";
                go ~announced ~gone:(Some (Eio.Time.now clock))
              end
              else
                let waited = Eio.Time.now clock -. started in
                if waited >= window then
                  Error
                    (with_tail tail
                       (Printf.sprintf
                          "dune did not open its RPC socket at %s within %d \
                           seconds of starting in %s: %s"
                          socket (int_of_float window) dir (why e)))
                else
                  let second = int_of_float waited in
                  let announced =
                    if second > announced then begin
                      trace
                        (Printf.sprintf "dune: waiting for socket %ds" second);
                      second
                    end
                    else announced
                  in
                  go ~announced ~gone)
    in
    go ~announced:0 ~gone:None
  in
  (* Spawning does not make the server that answers this session's own: the
     loser of the build-lock race reaches the winner's socket and hands it back
     here. Whether this session's own child is still running is what says which
     of the two happened, and only a session whose child is running may shut its
     server down or start another in its place. *)
  let launch () =
    let* child, output = spawn () in
    match wait ~output child with
    | Error e ->
        terminate child;
        Error e
    | Ok chan -> (
        match handshake chan with
        | Ok conn -> Ok (conn, still_running child)
        | Error e ->
            terminate child;
            Error e)
  in
  (* A socket file is not proof of a server: a dune killed outright cannot
     unlink the one it made. A connection refused there is that leftover, so the
     workspace may be free and a dune is spawned, which replaces the file. One
     that answers and then fails the handshake is a server the session did not
     start, and it is reported rather than replaced. *)
  let session () =
    if not (Sys.file_exists socket) then begin
      trace "dune: spawning server";
      launch ()
    end
    else
      let unreachable e =
        Error
          (Printf.sprintf
             "a dune server holds %s but this session cannot reach it at %s: \
              %s. Stop the server that owns the workspace, or remove the \
              socket if none is running."
             dir socket e)
      in
      (* One connection, asked of the socket that is there rather than of a
         probe, so that a server hears from the session once and the session is
         made on the connection that answered. *)
      match connect () with
      | Ok chan ->
          let* conn = handshake chan in
          trace "dune: attached to running server";
          Ok (conn, false)
      | Error (`Refused _) ->
          trace "dune: stale socket, spawning";
          launch ()
      | Error (`Other e) -> unreachable e
  in
  let* conn, owns =
    match session () with
    | Ok r -> Ok r
    | Error e ->
        trace ("dune: error " ^ first_line e);
        Error e
  in
  let t =
    {
      dir;
      owns;
      relaunch = launch;
      tail;
      lock = Eio.Mutex.create ();
      conn;
      stopped = false;
      trace;
      now = (fun () -> Eio.Time.now clock);
    }
  in
  Eio.Switch.on_release sw (fun () -> stop t);
  Ok t
