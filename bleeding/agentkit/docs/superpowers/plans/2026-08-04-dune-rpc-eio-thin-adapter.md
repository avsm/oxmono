# dune-rpc-eio thin adapter Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace `dune_rpc_eio`'s 1755-line reimplementation of dune's RPC protocol with a ~160-line Eio adapter over the `dune-rpc` opam package, and move the spawn-or-attach policy into okit.

**Architecture:** `dune_rpc_eio` supplies the two functor arguments `dune-rpc` asks for, a `Fiber` (Eio is direct style, so `'a t = 'a`) and a `Chan` (an Eio socket with a `Buf_read.t`), then applies `Dune_rpc.Private.Client.Make` and `Dune_rpc.V1.Where.Make`. This is what `otherlibs/dune-rpc-lwt/src/dune_rpc_lwt.ml` does for Lwt in 141 lines. Everything okit needs beyond that, spawning a server, waiting for its socket, relaunching one it owns, validating targets, becomes `okit/session.ml`.

**Tech Stack:** OCaml 5.2+, Eio 1.4+, `dune-rpc` 3.24, `csexp` 1.5, `pp` 2.0, dune 3.21.

## Global Constraints

- Every signature in this plan was verified to compile against `dune-rpc` 3.24.1 in the default switch. Do not substitute an API you have not compiled.
- `dune build`, `dune runtest` and `dune build @fmt` must all be clean before each commit. `ocamlformat` is pinned at 0.29.0, and it lives in the default opam switch, so `dune build @fmt` needs `~/.opam/default/bin` on PATH.
- Prose follows `ocaml-deepseek/CLAUDE.md`: the density of a POSIX manual page. No em-dashes. No two clauses joined by a semicolon. Document a value as `[foo x y] is ...`, naming its arguments. A comment must explain what the code cannot.
- One commit per task, one-line imperative message, no trailers or sign-off.
- `Session.t`, `Session.build` and the calls `start`, `build`, `runtest`, `promote`, `stop` and `trace` keep their names and argument labels. Only the element type of `build.diagnostics` changes.
- `Client` is exported at `Dune_rpc.Private.Client.S`, not `Dune_rpc.V1.Client.S`. `V1` makes `Request.t` abstract, so a client held to it cannot send `build`, which dune declares in `src/dune_rpc_impl/decl.ml` and does not export.
- okit needs `dune-rpc`, `csexp` and `pp`. It does **not** need `stdune`: `Dune_rpc.Private.Loc.t` is a concrete record `{ start : Lexing.position; stop : Lexing.position }`.
- The library keeps `unix` in its dependencies. `Where.Make` takes a `read_file` and an `analyze_path` over plain paths, with no capability to thread through, exactly as the Lwt adapter does.

---

## File Structure

| File | Responsibility |
| --- | --- |
| `dune_rpc_eio/dune_rpc_eio.ml(i)` | The whole library. `Fiber`, `Chan`, `Client`, `Where`, `connect`. |
| `okit/session.ml(i)` | Spawn or attach, wait for a socket, relaunch, validate targets, serialise calls. |
| `okit/report.ml` | Gains `to_text` for a `Dune_rpc.Private.Diagnostic.t`. |
| `okit/project.ml` | Uses the `csexp` package, with its three s-expression helpers local. |
| `test/rpc_eio.ml` | The adapter against a stub server over a socket pair. |
| `test/okit_dune.ml` | Unchanged cases, retargeted at `Okit.Session`, stubs rebuilt on `Private.Packet`. |

Deleted: `dune_rpc_eio/{csexp,pp_text,wire,session}.{ml,mli}`, `test/rpc_wire.ml`.

Tasks 1 and 2 add the new code beside the old, so the tree builds at every commit. Task 3 switches the consumers over and deletes the old code.

---

### Task 1: The Eio adapter

Added as `dune_rpc_eio/adapter.ml` beside the existing modules, so this commit builds. Task 3 renames it to `dune_rpc_eio.ml` once it is the only module in the library.

**Files:**
- Create: `dune_rpc_eio/adapter.ml`, `dune_rpc_eio/adapter.mli`
- Modify: `dune_rpc_eio/dune`, `dune-project`
- Test: `test/rpc_eio.ml`, `test/dune`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces, all under `Dune_rpc_eio.Adapter` in this task and under `Dune_rpc_eio` after Task 3:
  - `module Chan : sig type t val create : _ Eio.Net.stream_socket -> t val read : t -> Csexp.t option val write : t -> Csexp.t list -> unit val close : t -> unit end`
  - `module Client : Dune_rpc.Private.Client.S with type 'a fiber := 'a and type chan := Chan.t`
  - `module Where : Dune_rpc.V1.Where.S with type 'a fiber := 'a`
  - `val connect : sw:Eio.Switch.t -> net:_ Eio.Net.t -> Dune_rpc.V1.Where.t -> Chan.t`

- [ ] **Step 1: Add the dependencies**

`dune_rpc_eio/dune`:

```
(library
 (name dune_rpc_eio)
 (public_name dune-rpc-eio)
 (libraries csexp dune-rpc eio eio.unix logs unix))
```

`logs` stays only until Task 3 deletes `session.ml`, which is what uses it.

In `dune-project`, the `dune-rpc-eio` package's depends gains `(dune-rpc (>= 3.24))` and `(csexp (>= 1.5))`, and drops nothing yet.

- [ ] **Step 2: Write the failing test**

Create `test/rpc_eio.ml`. It runs a stub dune server on one end of a socket pair and a real client on the other. The stub answers the handshake using `dune-rpc`'s own encoders, so no wire format is written by hand.

```ocaml
(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private
module Adapter = Dune_rpc_eio.Adapter

let failures = ref 0

let check name cond =
  if cond then Printf.printf "ok   - %s\n%!" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n%!" name
  end

(* A stand-in dune server. It answers the handshake, selecting the highest
   version the client offered for each method, and then hands every other
   request to [f] as its method name. [cut] drops the last four bytes of one
   reply, which is the one thing a real server cannot be asked to do on cue. *)
let serve ?(cut = false) flow ~f =
  let chan = Adapter.Chan.create flow in
  let send sexp = Eio.Flow.copy_string (Csexp.to_string sexp) flow in
  let reply id payload =
    let s = Csexp.to_string (Conv.to_sexp Packet.sexp (Packet.Response (id, Ok payload))) in
    if cut then Eio.Flow.copy_string (String.sub s 0 (String.length s - 4)) flow
    else Eio.Flow.copy_string s flow
  in
  let rec go () =
    match Adapter.Chan.read chan with
    | None -> ()
    | Some sexp -> (
        match Conv.of_sexp Packet.sexp ~version:Version.latest sexp with
        | Error _ -> ()
        | Ok (Packet.Request (id, call)) -> (
            match Method.Name.to_string call.method_ with
            | "initialize" ->
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        (id, Ok (Initialize.Response.to_response (Initialize.Response.create ())))));
                go ()
            | "version_menu" ->
                let (Menu offered) =
                  Result.get_ok
                    (Version_negotiation.Request.of_call call ~version:Version.latest)
                in
                let picked = List.map (fun (m, vs) -> (m, List.fold_left max 1 vs)) offered in
                send
                  (Conv.to_sexp Packet.sexp
                     (Packet.Response
                        ( id,
                          Ok
                            (Version_negotiation.Response.to_response
                               (Version_negotiation.Response.create picked)) )));
                go ()
            | m ->
                reply id (f m);
                go ())
        | Ok _ -> go ())
  in
  go ()

let pair ~sw = Eio_unix.Net.socketpair_stream ~sw ()

(* A request reaches the server and its answer reaches the caller. *)
let round_trip () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let client_end, server_end = pair ~sw in
  Eio.Fiber.fork ~sw (fun () -> serve server_end ~f:(fun _ -> Csexp.List []));
  let chan = Adapter.Chan.create client_end in
  let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom "rpc_eio")) in
  let answered =
    Adapter.Client.connect chan init ~f:(fun c ->
        let ping =
          Result.get_ok (Adapter.Client.Versioned.prepare_request c Public.Request.ping)
        in
        Adapter.Client.request c ping ())
  in
  check "a ping is answered" (answered = Ok ())

(* A reply cut off part way through is a server that went away, not bytes that
   were wrong, so the client reports a dead connection rather than raising. *)
let cut_reply () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let client_end, server_end = pair ~sw in
  Eio.Fiber.fork ~sw (fun () ->
      serve ~cut:true server_end ~f:(fun _ -> Csexp.List []));
  let chan = Adapter.Chan.create client_end in
  let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom "rpc_eio")) in
  let answered =
    Adapter.Client.connect chan init ~f:(fun c ->
        let ping =
          Result.get_ok (Adapter.Client.Versioned.prepare_request c Public.Request.ping)
        in
        Adapter.Client.request c ping ())
  in
  check "a cut reply reports a dead connection"
    (match answered with
     | Error e -> Response.Error.kind e = Response.Error.Connection_dead
     | Ok () -> false)

let () =
  round_trip ();
  cut_reply ();
  if !failures > 0 then exit 1
```

Add to `test/dune`, beside the existing `rpc_wire` stanza:

```
; rpc_eio: the Eio adapter against a stub dune server over a socket pair. No
; processes, and no okit.

(test
 (name rpc_eio)
 (libraries csexp dune-rpc dune-rpc-eio eio eio.unix eio_main)
 (modules rpc_eio))
```

- [ ] **Step 3: Run the test to verify it fails**

Run: `opam exec -- dune build @runtest 2>&1 | head -20`
Expected: FAIL, `Unbound module Dune_rpc_eio.Adapter`.

- [ ] **Step 4: Write the implementation**

Create `dune_rpc_eio/adapter.ml`. This body was compiled against `dune-rpc` 3.24.1 and builds clean.

```ocaml
(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Eio is direct style, so a fiber is its own result and every combinator here
   is the Eio one under another name. *)
module Fiber = struct
  type 'a t = 'a

  let return x = x
  let fork_and_join_unit x y = snd (Eio.Fiber.pair x y)
  let finalize f ~finally = Fun.protect ~finally f

  (* Cancellation is a fiber ending rather than a fault to report, so it passes
     through instead of being collected. *)
  let collect_errors f =
    match f () with
    | x -> Ok x
    | exception (Eio.Cancel.Cancelled _ as e) -> raise e
    | exception e -> Error [ e ]

  let parallel_iter next ~f =
    Eio.Switch.run @@ fun sw ->
    let rec go () =
      match next () with
      | None -> ()
      | Some x ->
          Eio.Fiber.fork ~sw (fun () -> f x);
          go ()
    in
    go ()

  module O = struct
    let ( let* ) x f = f x
    let ( let+ ) x f = f x
  end

  module Ivar = struct
    type 'a t = 'a Eio.Promise.t * 'a Eio.Promise.u

    let create () = Eio.Promise.create ()
    let read (p, _) = Eio.Promise.await p
    let fill (_, u) x = Eio.Promise.resolve u x
  end
end

module Chan = struct
  type t = {
    sock : [ `Close | Eio.Flow.two_way_ty ] Eio.Resource.t;
    r : Eio.Buf_read.t;
  }

  (* A diagnostic for a large file, or a workspace with many of them, is the
     longest thing a server sends, and the reader must hold one whole message. *)
  let max_size = 16 * 1024 * 1024

  let create sock =
    let sock = (sock :> [ `Close | Eio.Flow.two_way_ty ] Eio.Resource.t) in
    { sock; r = Eio.Buf_read.of_flow sock ~max_size }

  let write t sexps =
    Eio.Buf_write.with_flow t.sock (fun w ->
        List.iter (fun s -> Eio.Buf_write.string w (Csexp.to_string s)) sexps)

  let close t =
    try Eio.Resource.close t.sock with Eio.Io _ | Invalid_argument _ -> ()

  (* A value that stops part way through is a server that died as it wrote, and
     the caller is told the session ended rather than handed a parse error. *)
  let read t =
    let open Csexp.Parser in
    let lexer = Lexer.create () in
    let rec loop depth stack =
      match Eio.Buf_read.any_char t.r with
      | exception End_of_file ->
          Lexer.feed_eoi lexer;
          None
      | c -> (
          match Lexer.feed lexer c with
          | Await -> loop depth stack
          | Lparen -> loop (depth + 1) (Stack.open_paren stack)
          | Rparen ->
              let stack = Stack.close_paren stack in
              let depth = depth - 1 in
              if depth = 0 then Some (List.hd (Stack.to_list stack))
              else loop depth stack
          | Atom n -> loop depth (Stack.add_atom (Eio.Buf_read.take n t.r) stack))
    in
    try loop 0 Stack.Empty
    with End_of_file | Eio.Io _ | Csexp.Parser.Parse_error _ -> None
end

module Client = Dune_rpc.Private.Client.Make (Fiber) (Chan)

module Where =
  Dune_rpc.V1.Where.Make
    (Fiber)
    (struct
      let read_file s =
        try Ok (In_channel.with_open_bin s In_channel.input_all)
        with e -> Error e

      let analyze_path s =
        match (Unix.stat s).st_kind with
        | Unix.S_SOCK -> Ok `Unix_socket
        | Unix.S_REG -> Ok `Normal_file
        | _ -> Ok `Other
        | exception e -> Error e
    end)

let connect ~sw ~net where =
  let addr =
    match where with
    | `Unix p -> `Unix p
    | `Ip (`Host h, `Port p) ->
        `Tcp (Eio_unix.Net.Ipaddr.of_unix (Unix.inet_addr_of_string h), p)
  in
  Chan.create (Eio.Net.connect ~sw net addr)
```

Create `dune_rpc_eio/adapter.mli`:

```ocaml
(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Dune's RPC client over Eio.

    This is the Eio counterpart to the [dune-rpc-lwt] package. It supplies the
    two arguments [dune-rpc] asks for, a fiber and a channel, and applies the
    functors. Everything a caller sends and receives is declared by [dune-rpc]
    itself.

    Eio is direct style, so every call here returns its result rather than a
    promise. *)

module Chan : sig
  type t
  (** One connection to a server. *)

  val create : _ Eio.Net.stream_socket -> t
  (** [create sock] reads and writes s-expressions over [sock]. Closing [t]
      closes [sock]. *)

  val read : t -> Csexp.t option
  (** [read t] is the next value, or [None] once the connection has ended. A
      value that stops part way through ends the connection, since a server that
      died as it wrote is the same loss as one that closed cleanly. *)

  val write : t -> Csexp.t list -> unit
  (** [write t sexps] sends [sexps], flushing before it returns. *)

  val close : t -> unit
  (** [close t] closes the connection. Closing one that is already closed does
      nothing. *)
end

module Client :
  Dune_rpc.Private.Client.S with type 'a fiber := 'a and type chan := Chan.t
(** The client. It is exported at the private signature rather than at
    {!Dune_rpc.V1.Client.S}, because [V1] keeps [Request.t] abstract and a
    caller needing a request dune declares privately, [build] among them, must
    pass its own witness to [connect_with_menu]. *)

module Where : Dune_rpc.V1.Where.S with type 'a fiber := 'a
(** Where a server for a build directory listens. *)

val connect :
  sw:Eio.Switch.t -> net:_ Eio.Net.t -> Dune_rpc.V1.Where.t -> Chan.t
(** [connect ~sw ~net where] opens a connection to [where]. The socket belongs
    to [sw].

    @raise Eio.Io if nothing is listening there. *)
```

- [ ] **Step 5: Run the test to verify it passes**

Run: `opam exec -- dune build 2>&1 | head -30 && opam exec -- dune exec test/rpc_eio.exe`
Expected: both checks print `ok`.

- [ ] **Step 6: Format and commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && opam exec -- dune runtest
git add dune_rpc_eio/adapter.ml dune_rpc_eio/adapter.mli dune_rpc_eio/dune dune-project test/rpc_eio.ml test/dune dune-rpc-eio.opam
git commit -m "add an Eio adapter over the dune-rpc package"
```

---

### Task 2: The session as okit's own

**Files:**
- Create: `okit/session.ml`, `okit/session.mli`
- Modify: `okit/dune`, `test/okit_dune.ml`, `test/dune`, `humpty.opam`

**Interfaces:**
- Consumes: `Dune_rpc_eio.Adapter.{Chan, Client, Where, connect}` from Task 1.
- Produces:
  - `type Okit.Session.t`
  - `type Okit.Session.build = { ok : bool; diagnostics : Dune_rpc.Private.Diagnostic.t list }`
  - `val start : ?trace:(string -> unit) -> ?client:string -> sw:Eio.Switch.t -> proc:[> [ `Generic | `Unix ] Eio.Process.mgr_ty ] Eio.Resource.t -> net:_ Eio.Net.t -> clock:_ Eio.Time.clock -> root:Eio.Fs.dir_ty Eio.Path.t -> unit -> (t, string) result`
  - `val build : t -> targets:string list -> (build, string) result`
  - `val runtest : t -> (build, string) result`
  - `val promote : t -> path:string -> (unit, string) result`
  - `val stop : t -> unit`
  - `val trace : t -> string -> unit`

- [ ] **Step 1: Add the dependencies**

`okit/dune` libraries become, in alphabetical order:

```
(library
 (name okit)
 (public_name humpty.okit)
 (libraries
  cstruct
  csexp
  deepseek
  deepseek.dsml
  dune-rpc
  dune-rpc-eio
  eio
  eio.unix
  jsont
  logs
  pp
  unix))
```

In `dune-project`, the `humpty` package's depends gains `(dune-rpc (>= 3.24))`, `(csexp (>= 1.5))` and `(pp (>= 2.0.0))`.

- [ ] **Step 2: Point the existing session tests at the new module**

`test/okit_dune.ml` keeps every case. Change its three alias lines at the top from

```ocaml
module Csexp = Dune_rpc_eio.Csexp
module Wire = Dune_rpc_eio.Wire
module Dune_rpc = Dune_rpc_eio.Session
```

to

```ocaml
open Dune_rpc.Private
module Session = Okit.Session
```

and replace every `Dune_rpc.` prefix on a session call with `Session.`. `Csexp` now resolves to the `csexp` package, whose `Csexp.t` has the same `Atom`/`List` constructors, and whose `Csexp.parse_string : string -> (t, int * string) result` replaces `Csexp.of_string`.

The stub servers at `test/okit_dune.ml:87-125` (`answer`, `handshake_on`, `dying_server`) and at `:381-405` (`winning_server`) built packets through `Wire.Packet`. Replace `answer` and `handshake_on` with the `serve` helper written for `test/rpc_eio.ml` in Task 1, copied into this file, since the two tests are separate executables and neither is a library. `dying_server` calls `serve ~cut:true`, and `winning_server` calls `serve` with an `f` that records the method name it was asked for.

The case at `:622` calls `Wire.Diagnostic.to_text d`. It becomes `Okit.Report.to_text d`, which Task 3 adds. Until then, inline the two lines:

```ocaml
let text = Format.asprintf "%a" Pp.to_fmt (Diagnostic.message d) in
```

`test/dune`'s `okit_dune` stanza gains `csexp`, `dune-rpc` and `pp`:

```
(test
 (name okit_dune)
 (libraries csexp dune-rpc dune-rpc-eio humpty.okit eio eio_main pp unix)
 (modules okit_dune))
```

- [ ] **Step 3: Run the tests to verify they fail**

Run: `opam exec -- dune build 2>&1 | head -20`
Expected: FAIL, `Unbound module Okit.Session`.

- [ ] **Step 4: Write `okit/session.ml`**

Most of this file is moved from `dune_rpc_eio/session.ml` unchanged. Move these, verbatim, keeping their comments but cutting each to the length `CLAUDE.md` asks for:

| From `dune_rpc_eio/session.ml` | What it is |
| --- | --- |
| `:29-56` `module Tail`, `with_tail` | The ring buffer holding the server's last output. |
| `:58-74` `drain` | The daemon fiber reading the child's pipe. |
| `:162-174` `first_line`, `guarded` | Trace helpers. |
| `:292-339` `target_forms`, `bad_in_alias`, `alias`, `dep_spec`, `dep_specs` | Target validation. Unchanged, wording included. |
| `:424-454` `dune_on_path`, `environment` | Finding dune and stripping `INSIDE_DUNE`, `DUNE_BUILD_DIR`, `DUNE_RPC`. |
| `:508-687` `classify`, `answers`, `spawn`, `terminate`, `still_running`, `window`, `grace`, `last_words`, `wait` | Spawning and waiting for a socket. `connect` is replaced, see below. |

Delete outright, because `dune-rpc` does it: `initialize_id`, `menu_id`, `ended_mid_value`, `send`, `message`, `unexpected`, `await`, `exchange`, `request`'s id allocation, and `handshake`.

The genuinely new code is how a scoped `Client.connect_with_menu` becomes an unscoped `Session.t`. Write it exactly as follows.

```ocaml
(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Dune_rpc.Private
module Adapter = Dune_rpc_eio.Adapter

let ( let* ) = Result.bind
let src = Logs.Src.create "okit.session"

module Log = (val Logs.src_log src : Logs.LOG)

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
   method the server dropped fails here rather than at the call that needed it. *)
type calls = {
  build : (string list, Build_outcome_with_diagnostics.t) Adapter.Client.Versioned.request;
  runtest : (string list, Build_outcome_with_diagnostics.t) Adapter.Client.Versioned.request;
  diagnostics : (unit, Diagnostic.t list) Adapter.Client.Versioned.request;
  flush : (unit, [ `Ok | `Not_in_watch_mode ]) Adapter.Client.Versioned.request;
  promote : (Path.t, unit) Adapter.Client.Versioned.request;
}

type conn = { client : Adapter.Client.t; calls : calls; stop : unit Eio.Promise.u }

let prepare c =
  let ( let* ) x f = Result.bind (Result.map_error Version_error.message x) f in
  let* build =
    Adapter.Client.Versioned.prepare_request c (Decl.Request.witness build_decl)
  in
  let* runtest = Adapter.Client.Versioned.prepare_request c Public.Request.runtest in
  let* diagnostics =
    Adapter.Client.Versioned.prepare_request c Public.Request.diagnostics
  in
  let* flush =
    Adapter.Client.Versioned.prepare_request c Public.Request.flush_file_watcher
  in
  let* promote = Adapter.Client.Versioned.prepare_request c Public.Request.promote in
  Ok { build; runtest; diagnostics; flush; promote }

(* [Client.connect_with_menu] runs its callback and returns, and a session
   outlives any one call, so the connection lives on a fiber of its own. That
   fiber hands back the client and then parks until [stop] resolves, which is
   what closes the connection. *)
let attach ~sw ~client:id chan =
  let ready, set_ready = Eio.Promise.create () in
  let stopping, stop = Eio.Promise.create () in
  Eio.Fiber.fork ~sw (fun () ->
      let init = Initialize.Request.create ~id:(Id.make (Csexp.Atom id)) in
      let private_menu = [ Adapter.Client.Request build_decl ] in
      match
        Adapter.Client.connect_with_menu ~private_menu chan init ~f:(fun c ->
            match prepare c with
            | Error e ->
                Eio.Promise.resolve set_ready (Error e);
                ()
            | Ok calls ->
                Eio.Promise.resolve set_ready (Ok { client = c; calls; stop });
                Eio.Promise.await stopping)
      with
      | () -> ()
      | exception e ->
          (* The handshake failed, and nobody is waiting on [ready] once it has
             been resolved, so resolving twice must not raise. *)
          if not (Eio.Promise.is_resolved ready) then
            Eio.Promise.resolve set_ready (Error (Printexc.to_string e)));
  Eio.Promise.await ready
```

The rest of the file keeps the existing shape. `type t` loses `ids` and keeps everything else, with `conn` now the record above:

```ocaml
type t = {
  dir : string;
  mutable owns : bool;
  relaunch : unit -> (conn * bool, string) result;
  tail : Tail.t;
  lock : Eio.Mutex.t;
  mutable conn : conn;
  mutable stopped : bool;
  trace : string -> unit;
  now : unit -> float;
}

type build = { ok : bool; diagnostics : Diagnostic.t list }
```

`request` at `:223-258` keeps its relaunch structure, with the exchange replaced. A `Response.Error.t` whose kind is `Connection_dead` is the server going away, which is what used to be `` `Gone ``:

```ocaml
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
          Error (Printf.sprintf "dune refused %s: %s" what (Response.Error.message e)))

let request t ~what f =
  let* r = call t ~what f in
  match r with
  | `Answered x -> Ok x
  | `Gone ->
      if t.stopped then Error (stopped_error t)
      else if not t.owns then Error (gone t ~what)
      else begin
        t.trace "dune: server gone, relaunching";
        Eio.Promise.resolve t.conn.stop ();
        match t.relaunch () with
        | Error e -> Error e
        | Ok (conn, owns) -> (
            t.conn <- conn;
            t.owns <- owns;
            let* r = call t ~what f in
            match r with
            | `Answered x -> Ok x
            | `Gone -> Error (gone t ~what))
      end
```

`build`, `runtest` and `promote` keep their bodies from `:345-403`, with each `ask` becoming a `request` through the prepared call:

```ocaml
let ok = function Build_outcome_with_diagnostics.Success -> true | Failure _ -> false

let flushed t =
  let* r =
    request t ~what:"flush-file-watcher" (fun c ->
        Adapter.Client.request c.client c.calls.flush ())
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
      Adapter.Client.request c.client c.calls.diagnostics ())

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
            Adapter.Client.request c.client c.calls.build targets)
      in
      t.trace "dune: diagnostics";
      let* diagnostics = diagnostics t in
      count t diagnostics;
      Ok { ok = ok outcome; diagnostics })

let runtest t =
  traced t
  @@ guard t (fun () ->
         t.trace "dune: flush";
         let* () = flushed t in
         t.trace "dune: runtest";
         let* outcome =
           request t ~what:"runtest" (fun c ->
               Adapter.Client.request c.client c.calls.runtest [ "." ])
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
           request t ~what:"promote" (fun c ->
               Adapter.Client.request c.client c.calls.promote (Path.absolute path))
         in
         Ok ())
```

`stop` at `:410-420` sends the shutdown notification through the client and then resolves the stop promise, which is what ends the connection fiber:

```ocaml
let stop t =
  let close () =
    if not t.stopped then begin
      t.stopped <- true;
      if t.owns then
        (match
           Adapter.Client.Versioned.prepare_notification t.conn.client
             Public.Notification.shutdown
         with
        | Ok n -> (try Adapter.Client.notification t.conn.client n () with _ -> ())
        | Error _ -> ());
      Eio.Promise.resolve t.conn.stop ()
    end
  in
  Eio.Cancel.protect (fun () ->
      try Eio.Mutex.use_rw ~protect:true t.lock close
      with Eio.Mutex.Poisoned _ -> close ())
```

In `start`, the socket path is no longer written by hand. Replace

```ocaml
let socket = dir ^ "/_build/.rpc/dune" in
```

with

```ocaml
let where = Adapter.Where.default ~build_dir:(dir ^ "/_build") () in
let socket = match where with `Unix p -> p | `Ip _ -> "" in
```

and replace the local `connect ()` and `answers ()` with calls to `Adapter.connect ~sw ~net where`, keeping the switch-per-attempt structure at `:528-538` unchanged. `handshake` at `:459-486` is deleted, and each place that called it calls `attach ~sw ~client chan` instead, whose error is already a string.

`Path.absolute` above is `Dune_rpc.Private.Path.absolute`, and `promote` takes an absolute path, which is what a diagnostic's `in_source` already is.

- [ ] **Step 5: Write `okit/session.mli`**

About 55 lines, against the 140 of `dune_rpc_eio/session.mli`. Keep the signatures exactly as the Interfaces block above gives them. For each value say what it does and what a caller must know, and no more. The material to cut, because it belongs beside the invariant in the `.ml` and not in the interface: the four paragraphs of `start` describing the spawn race, the grace window, the trace cadence and the stale socket. Keep, because a caller cannot act without it: that `start` spawns or attaches and that `stop` only shuts down a server it started, the target grammar accepted by `build`, that `runtest` covers the whole workspace, that `promote` takes a diagnostic's `in_source` path, and that a call after `stop` is refused with an error rather than raising.

```ocaml
type build = {
  ok : bool;  (** Whether the build reached its targets. *)
  diagnostics : Dune_rpc.Private.Diagnostic.t list;
      (** Every error and warning the server holds, which is the whole
          workspace and not only the requested targets. *)
}
```

- [ ] **Step 6: Run the tests to verify they pass**

Run: `opam exec -- dune build && opam exec -- dune runtest 2>&1 | tail -40`
Expected: every `okit_dune` and `rpc_eio` check prints `ok`, and the run exits 0.

If `okit_dune`'s live cases fail, run one alone to read its output: `opam exec -- dune exec test/okit_dune.exe`. These spawn a real dune, so a failure here is a real behavioural difference and not a test artefact.

- [ ] **Step 7: Format and commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && opam exec -- dune runtest
git add okit/session.ml okit/session.mli okit/dune dune-project humpty.opam test/okit_dune.ml test/dune
git commit -m "move the dune session into okit, on the dune-rpc client"
```

---

### Task 3: Switch okit over and delete the old library

**Files:**
- Modify: `okit/report.ml`, `okit/report.mli`, `okit/server.ml`, `okit/server.mli`, `okit/client.ml`, `okit/project.ml`, `okit/project.mli`
- Rename: `dune_rpc_eio/adapter.ml` to `dune_rpc_eio/dune_rpc_eio.ml`, and the `.mli` likewise
- Delete: `dune_rpc_eio/{csexp,pp_text,wire,session}.{ml,mli}`, `test/rpc_wire.ml`
- Modify: `dune_rpc_eio/dune`, `test/dune`, `test/okit_dune.ml`

**Interfaces:**
- Consumes: `Okit.Session` from Task 2, `Dune_rpc_eio.Adapter` from Task 1.
- Produces: `val Okit.Report.to_text : Dune_rpc.Private.Diagnostic.t -> string`, and every `Dune_rpc_eio.Adapter.X` becomes `Dune_rpc_eio.X`.

- [ ] **Step 1: Write the failing test for `to_text`**

`test/okit_dune.ml` at `:622` currently renders a diagnostic inline. Replace that with a call to the new function, and add a case beside it that pins the shape:

```ocaml
(* A diagnostic reads as dune prints it: a File header from the location, then
   the message, then one line per promotion. *)
let renders text =
  check "a diagnostic names its file" (holds text "File \"");
  check "a diagnostic carries its message" (holds text "Error")
```

Call `renders (Okit.Report.to_text d)` where the existing case has the diagnostic in hand.

- [ ] **Step 2: Run to verify it fails**

Run: `opam exec -- dune build 2>&1 | head -10`
Expected: FAIL, `Unbound value Okit.Report.to_text`.

- [ ] **Step 3: Add `to_text` to `okit/report.ml`**

This replaces `dune_rpc_eio/pp_text.ml` and `Wire.Diagnostic.to_text`. `Pp.to_fmt` renders the tree that `pp_text.ml` walked by hand.

`report.ml` is inside the wrapped `okit` library, so its sibling modules are named directly: `Session`, not `Okit.Session`.

```ocaml
open Dune_rpc.Private

(* [to_text d] is [d] as the model sees it, ending in a newline. A location
   gives the File header dune prints, which is not part of the message. A
   severity that the message does not already open with gives a line of its
   own. *)
let to_text (d : Diagnostic.t) =
  let buf = Buffer.create 256 in
  (match d.loc with
  | None -> ()
  | Some l ->
      let s = Loc.start l and e = Loc.stop l in
      Buffer.add_string buf
        (Printf.sprintf "File %S, line %d, characters %d-%d:\n" s.pos_fname
           s.pos_lnum
           (s.pos_cnum - s.pos_bol)
           (e.pos_cnum - e.pos_bol)));
  let body = Format.asprintf "%a" Pp.to_fmt (Diagnostic.message d) in
  (match d.severity with
  | Some sev ->
      let word = match sev with Diagnostic.Error -> "Error" | Warning -> "Warning" in
      if not (String.starts_with ~prefix:word body) then begin
        Buffer.add_string buf word;
        Buffer.add_string buf ":\n"
      end
  | None -> ());
  Buffer.add_string buf body;
  if not (String.ends_with ~suffix:"\n" body) then Buffer.add_char buf '\n';
  List.iter
    (fun (p : Diagnostic.Promotion.t) ->
      Buffer.add_string buf (Printf.sprintf "Promote %s\n" p.in_source))
    d.promotion;
  Buffer.contents buf
```

Add to `okit/report.mli`:

```ocaml
val to_text : Dune_rpc.Private.Diagnostic.t -> string
(** [to_text d] is [d] as dune prints it, ending in a newline: the [File "…"]
    header when [d] has a location, the severity when the message does not
    already open with it, the message, then one line per promotion. *)
```

- [ ] **Step 4: Retype the rest of `okit/report.ml`**

At `:6-7` the two aliases are replaced by the `open Dune_rpc.Private` above. At `:27` `Wire.Diagnostic.to_text d` becomes `to_text d`. At `:40` and `:74` the parameter type `Dune_rpc.build` becomes `Session.build`. At `:68` `about` reads the location through the new record:

```ocaml
let about ~path (d : Diagnostic.t) =
  let path = plain path in
  match d.loc with
  | None -> false
  | Some l ->
      let file = (Loc.start l).pos_fname in
      file = path || String.ends_with ~suffix:("/" ^ path) file
```

In `okit/report.mli`, `Dune_rpc_eio.Session.build` becomes `Session.build` at both occurrences, and the `to_text` signature above names its argument `Dune_rpc.Private.Diagnostic.t` in full, since an `.mli` in this repository writes the full path.

- [ ] **Step 5: Retype `okit/server.ml`, `okit/client.ml`, `okit/project.ml`**

- `okit/server.ml:6`: `module Dune_rpc = Dune_rpc_eio.Session` becomes `module Dune_rpc = Session`. Every other line in the file is unchanged, since it already goes through that alias.
- `okit/server.mli:33` and `okit/client.ml:27`: the reference `{!Dune_rpc_eio.Session}` becomes `{!Session}`.
- `okit/project.mli:17`: the same substitution.
- `okit/project.ml:6`: delete `module Csexp = Dune_rpc_eio.Csexp`, so that `Csexp` resolves to the package. At `:273`, `Csexp.of_string out` becomes

  ```ocaml
  match Csexp.parse_string out with
  ```

  whose error is `(int * string)` rather than `string`, so the arm that reported it becomes `| Error (_, m) ->` with the same message.

  The package has no `field` or `atom`, so add them at the top of `project.ml`, moved from `dune_rpc_eio/csexp.ml`:

  ```ocaml
  (* Reading a record out of [dune describe --format csexp] output. *)
  let atom = function Csexp.Atom a -> Some a | Csexp.List _ -> None
  let to_list = function Csexp.List l -> Some l | Csexp.Atom _ -> None

  let field t name =
    match t with
    | Csexp.List entries ->
        List.find_map
          (function
            | Csexp.List [ Csexp.Atom n; v ] when n = name -> Some v | _ -> None)
          entries
    | Csexp.Atom _ -> None
  ```

  The existing calls `Csexp.field`, `Csexp.atom` and `Csexp.to_list` become `field`, `atom` and `to_list`. `Csexp.pp` is used at `test/okit_dune.ml:455`; the package has no `pp`, so that line becomes `print_endline (Csexp.to_string sexp)`.

- [ ] **Step 6: Delete the old library modules and unwrap**

```bash
git rm dune_rpc_eio/csexp.ml dune_rpc_eio/csexp.mli \
       dune_rpc_eio/pp_text.ml dune_rpc_eio/pp_text.mli \
       dune_rpc_eio/wire.ml dune_rpc_eio/wire.mli \
       dune_rpc_eio/session.ml dune_rpc_eio/session.mli \
       test/rpc_wire.ml
git mv dune_rpc_eio/adapter.ml dune_rpc_eio/dune_rpc_eio.ml
git mv dune_rpc_eio/adapter.mli dune_rpc_eio/dune_rpc_eio.mli
```

With one module whose name is the library's, dune stops wrapping, so `Dune_rpc_eio.Adapter.Client` becomes `Dune_rpc_eio.Client`. Replace `module Adapter = Dune_rpc_eio.Adapter` with `module Adapter = Dune_rpc_eio` in `okit/session.ml` and `test/rpc_eio.ml`, or drop the alias and write `Dune_rpc_eio.` directly. Prefer dropping it.

`dune_rpc_eio/dune` loses `logs`, which only `session.ml` used:

```
(library
 (name dune_rpc_eio)
 (public_name dune-rpc-eio)
 (libraries csexp dune-rpc eio eio.unix unix))
```

`dune-project`'s `dune-rpc-eio` package drops `(logs (>= 0.7.0))`.

`test/dune` loses the `rpc_wire` stanza entirely.

- [ ] **Step 7: Run the full suite**

Run: `opam exec -- dune build && opam exec -- dune runtest 2>&1 | tail -40`
Expected: clean build, every check `ok`, exit 0.

Run: `opam exec -- dune build @doc 2>&1 | head -20`
Expected: no output. The library is public API, so a broken reference in a doc comment fails here.

- [ ] **Step 8: Format and commit**

```bash
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt --auto-promote
opam exec -- dune build && opam exec -- dune runtest && opam exec -- dune build @doc
git add okit/report.ml okit/report.mli okit/server.ml okit/server.mli \
        okit/client.ml okit/project.ml okit/project.mli \
        dune_rpc_eio/dune_rpc_eio.ml dune_rpc_eio/dune_rpc_eio.mli \
        dune_rpc_eio/dune dune-project dune-rpc-eio.opam \
        test/dune test/okit_dune.ml test/rpc_eio.ml
git commit -m "drop the hand-written dune RPC protocol for the dune-rpc package"
```

---

### Task 4: Documentation

**Files:**
- Modify: `ARCH.md`, `CHANGES.md`, `docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md`

**Interfaces:**
- Consumes: the finished tree from Task 3.
- Produces: nothing code depends on.

- [ ] **Step 1: Rewrite the `dune_rpc_eio/` section of ARCH.md**

Find the section listing `csexp.ml`, `pp_text.ml`, `wire.ml` and `session.ml`. Replace the file map with the one file the library now has, and say what it is:

> `dune_rpc_eio/` is the Eio counterpart to the `dune-rpc-lwt` package. It
> supplies the fiber and the channel that `dune-rpc` asks for and applies its
> functors, so the protocol itself is upstream's. `dune_rpc_eio.ml` is the
> whole library.

Add `session.ml` to the okit file map, describing it as the spawn-or-attach policy: it starts `dune build --passive-watch-mode` or joins a server already holding the workspace, waits for the socket, relaunches a server it owns, and validates targets before sending them. Note that it declares `build` itself, because dune keeps that request in `src/dune_rpc_impl/decl.ml` and does not export it.

Any paragraph elsewhere in ARCH.md citing `okit/csexp.ml`, `okit/wire.ml`, `dune_rpc_eio/wire.ml` or `dune_rpc_eio/session.ml` gets its path corrected or removed if the file it describes is gone. Search with `grep -n "wire\.\|pp_text\|csexp\.ml" ARCH.md`.

- [ ] **Step 2: Add the CHANGES.md entry**

One or two lines, under the current unreleased heading:

```markdown
- `dune-rpc-eio` is now a thin Eio adapter over the `dune-rpc` package rather
  than its own implementation of the protocol. The session that spawns or
  attaches to a dune server has moved to `Okit.Session`.
```

- [ ] **Step 3: Mark the protocol note**

`docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md` described the wire format this change stops implementing. Add one sentence at the top saying it records what dune's protocol looks like on the wire, and that the code no longer encodes it, `dune-rpc` does.

- [ ] **Step 4: Verify and commit**

```bash
opam exec -- dune build && opam exec -- dune runtest && opam exec -- dune build @doc
git add ARCH.md CHANGES.md docs/superpowers/plans/2026-08-02-dune-rpc-protocol.md
git commit -m "document dune-rpc-eio as an adapter and the session as okit's"
```

---

## Verification

After Task 4, from the repository root:

```bash
opam exec -- dune build
opam exec -- dune runtest
opam exec -- dune build @doc
PATH="$HOME/.opam/default/bin:$PATH" opam exec -- dune build @fmt
```

All four clean. `DS4_LIVE=1 dune runtest` is not needed, since nothing here touches the FFI, the engine's session lifetime or the agent loop. The live cases in `test/okit_dune.ml` and `test/okit_tools.ml` run against a real dune under the plain `dune runtest` and are what prove the session still spawns, builds and reports.

Count the result to confirm the goal was met:

```bash
wc -l dune_rpc_eio/*.ml dune_rpc_eio/*.mli okit/session.ml okit/session.mli
```

Expected: the library around 160 lines across its two files, and `okit/session.ml(i)` around 385, against 1755 before.
