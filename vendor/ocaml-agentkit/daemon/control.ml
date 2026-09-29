(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal
module Line = Agentkit.Line
module Memory = Agentkit.Memory

type request =
  | Status
  | Jobs
  | Memory of { at : int option }
  | Log of { since : float option; kinds : string list; limit : int }
  | Follow of { kinds : string list }

type answer =
  | Running of Status.running
  | Jobs_are of Status.jobs
  | Memory_is of Status.memory
  | Lines of Status.lines
  | Refused of string

module type S = sig
  val status : unit -> Status.running
  val jobs : unit -> Status.jobs
  val memory : at:int option -> Status.memory
  val log : since:float option -> kinds:string list -> limit:int -> Status.lines
  val follow : kinds:string list -> emit:(string -> unit) -> unit
end

(* The implementation. Everything about the run comes from the snapshot the
   agent loop published, and everything else from the files, so no question
   asked here waits for the engine. *)

let tail n xs =
  let len = List.length xs in
  if len <= n then xs else List.filteri (fun i _ -> i >= len - n) xs

let make ~snapshot ~clock ~root =
  let root = (root :> Eio.Fs.dir_ty Eio.Path.t) in
  let journal_dir = Store.journal_dir root in
  let module Impl = struct
    let status () = Status.running (snapshot ())
    let jobs () = Status.jobs (snapshot ())

    let memory ~at =
      let m = Memory.create ~clock (Store.memory_dir root) in
      { Status.m_version = Memory.version m; m_text = Show.show m ~at }

    let log ~since ~kinds ~limit =
      let out = ref [] in
      let kinds = match kinds with [] -> None | k -> Some k in
      Show.log ?since ?kinds journal_dir (fun l -> out := l :: !out);
      { Status.lines = tail limit (List.rev !out) }

    (* The newest segment alone, since a record appended now is at the end of
       it. Reading every segment on every poll would cost more each month for an
       answer that is always in today's file. *)
    let follow ~kinds ~emit =
      let kinds = match kinds with [] -> None | k -> Some k in
      let seen = ref (Journal.recover journal_dir).Journal.next_seq in
      let newest () =
        match List.rev (Journal.segments journal_dir) with
        | [] -> None
        | newest :: _ -> Some newest
      in
      while true do
        (match newest () with
        | None -> ()
        | Some name ->
            Journal.iter_segment ?kinds journal_dir name (fun r ->
                if r.Journal.seq >= !seen then begin
                  seen := r.Journal.seq + 1;
                  emit (Show.record_line r)
                end));
        Eio.Time.sleep clock 0.5
      done
  end in
  (module Impl : S)

let answer (module Impl : S) = function
  | Status -> Running (Impl.status ())
  | Jobs -> Jobs_are (Impl.jobs ())
  | Memory { at } -> Memory_is (Impl.memory ~at)
  | Log { since; kinds; limit } -> Lines (Impl.log ~since ~kinds ~limit)
  | Follow _ ->
      Refused
        "follow is a stream rather than one answer, so it is not served here"

(* The line adapter. The envelope is one member naming the method, as the other
   two line protocols in this repository use, and the payload is the record's own
   jsont codec so that a capnp struct maps onto it without rearrangement. *)

let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobj mems = Jsont.Json.object' mems
let envelope name json = Dsml.Json.Value.to_string (jobj [ jmem name json ])

let encoded name codec v =
  match Jsont.Json.encode codec v with
  | Ok json -> envelope name json
  | Error e -> envelope "error" (jobj [ jmem "what" (Jsont.Json.string e) ])

let answer_line = function
  | Running r -> encoded "status" Status.running_jsont r
  | Jobs_are j -> encoded "jobs" Status.jobs_jsont j
  | Memory_is m -> encoded "memory" Status.memory_jsont m
  | Lines l -> encoded "log" Status.lines_jsont l
  | Refused what ->
      envelope "error"
        (jobj [ jmem "what" (Jsont.Json.string (Line.utf_8 what)) ])

let payload line =
  match Dsml.Json.Value.of_string line with
  | Ok (Jsont.Object ([ (name, (Jsont.Object _ as json)) ], _)) ->
      Some (fst name, json)
  | Ok _ | Error _ -> None

let decoded codec json =
  match Jsont.Json.decode codec json with Ok v -> Some v | Error _ -> None

let answer_of line =
  match payload line with
  | Some ("status", json) ->
      Option.map (fun r -> Running r) (decoded Status.running_jsont json)
  | Some ("jobs", json) ->
      Option.map (fun j -> Jobs_are j) (decoded Status.jobs_jsont json)
  | Some ("memory", json) ->
      Option.map (fun m -> Memory_is m) (decoded Status.memory_jsont json)
  | Some ("log", json) ->
      Option.map (fun l -> Lines l) (decoded Status.lines_jsont json)
  | Some ("error", Jsont.Object (mems, _)) -> (
      match Jsont.Json.find_mem "what" mems with
      | Some (_, Jsont.String (what, _)) -> Some (Refused what)
      | _ -> None)
  | Some _ | None -> None

let request_line = function
  | Status -> envelope "status" (jobj [])
  | Jobs -> envelope "jobs" (jobj [])
  | Memory { at } ->
      envelope "memory"
        (jobj
           (match at with
           | None -> []
           | Some v -> [ jmem "at" (Jsont.Json.number (float_of_int v)) ]))
  | Log { since; kinds; limit } ->
      envelope "log"
        (jobj
           ((match since with
              | None -> []
              | Some t -> [ jmem "since" (Jsont.Json.number t) ])
           @ [
               jmem "kinds"
                 (Jsont.Json.list
                    (List.map (fun k -> Jsont.Json.string (Line.utf_8 k)) kinds));
               jmem "limit" (Jsont.Json.number (float_of_int limit));
             ]))
  | Follow { kinds } ->
      envelope "follow"
        (jobj
           [
             jmem "kinds"
               (Jsont.Json.list
                  (List.map (fun k -> Jsont.Json.string (Line.utf_8 k)) kinds));
           ])

let mems = function Jsont.Object (mems, _) -> mems | _ -> []
let member k json = Option.map snd (Jsont.Json.find_mem k (mems json))

let int_member k json =
  match member k json with
  | Some (Jsont.Number (v, _)) when Float.is_integer v -> Some (int_of_float v)
  | _ -> None

let float_member k json =
  match member k json with Some (Jsont.Number (v, _)) -> Some v | _ -> None

let strings_member k json =
  match member k json with
  | None -> Some []
  | Some (Jsont.Array (elts, _)) ->
      List.fold_right
        (fun elt acc ->
          match (elt, acc) with
          | Jsont.String (v, _), Some vs -> Some (v :: vs)
          | _ -> None)
        elts (Some [])
  | Some _ -> None

let request_of line =
  match payload line with
  | Some ("status", _) -> Some Status
  | Some ("jobs", _) -> Some Jobs
  | Some ("memory", json) -> Some (Memory { at = int_member "at" json })
  | Some ("log", json) ->
      Option.map
        (fun kinds ->
          Log
            {
              since = float_member "since" json;
              kinds;
              limit = Option.value ~default:200 (int_member "limit" json);
            })
        (strings_member "kinds" json)
  | Some ("follow", json) ->
      Option.map (fun kinds -> Follow { kinds }) (strings_member "kinds" json)
  | Some _ | None -> None

(* A unix socket address is limited to about a hundred characters, the same
   bound that keeps a dune server out of a deep workspace. *)
let max_path = 100

let session impl flow =
  let r = Eio.Buf_read.of_flow flow ~max_size:Line.max_line in
  Eio.Buf_write.with_flow flow @@ fun w ->
  let write a =
    Line.write answer_line w a;
    Eio.Buf_write.flush w
  in
  let rec loop () =
    match Line.read request_of r with
    | `Eof -> ()
    | `Bad _ ->
        (* A line that does not parse is a terminal fault for this connection.
           The reader is part way through a line it will never finish, so no
           later read is trustworthy. *)
        write
          (Refused
             "that is not a request this numpty knows, so the connection ends \
              here")
    | `Msg (Follow { kinds }) ->
        let (module Impl : S) = impl in
        Impl.follow ~kinds ~emit:(fun line ->
            write (Lines { Status.lines = [ line ] }))
    | `Msg request ->
        write (answer impl request);
        loop ()
  in
  (* A client that goes away mid-answer ends this fiber and nothing else. *)
  try loop () with Eio.Io _ | End_of_file -> ()

let serve ~sw ~net ~root impl =
  let path = Eio.Path.native_exn (Store.control_path root) in
  if String.length path > max_path then
    `Unbound
      (Printf.sprintf
         "the store's path makes the control socket %d characters, past the \
          hundred or so a unix address holds, so there is no socket to ask on. \
          A shorter --store gives one."
         (String.length path))
  else begin
    (* The store's lock is taken before this, so a socket file here belongs to
       no live run. *)
    (try Unix.unlink path with Unix.Unix_error _ -> ());
    let listening = Eio.Net.listen ~sw ~backlog:8 net (`Unix path) in
    (* The store directory is already the owner's alone, so the window between
       the bind and this is not one another user can reach through. *)
    (try Unix.chmod path 0o600 with Unix.Unix_error _ -> ());
    (* The connections live on a switch of their own, inside a daemon fiber, so
       that the release of [sw] cancels them rather than waiting for them. A
       client sitting on an open connection would otherwise keep the run from
       ever finishing, which is the opposite of what a socket for watching a
       daemon is for. *)
    Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Switch.run @@ fun conns ->
        while true do
          Eio.Net.accept_fork ~sw:conns listening
            ~on_error:(fun e ->
              Logs.info (fun m ->
                  m "a control connection ended: %s" (Printexc.to_string e)))
            (fun flow _ -> session impl flow)
        done;
        `Stop_daemon);
    `Serving path
  end

(* The client. *)

let connect ~sw ~net ~root =
  let path = Eio.Path.native_exn (Store.control_path root) in
  match Eio.Net.connect ~sw net (`Unix path) with
  | flow -> Ok flow
  | exception Eio.Io _ ->
      Error
        (Printf.sprintf
           "nothing is listening on %s, so no numpty is running on this store."
           path)

let with_connection ~sw ~net ~root f =
  match connect ~sw ~net ~root with
  | Error e -> Error e
  | Ok flow ->
      let r = Eio.Buf_read.of_flow flow ~max_size:Line.max_line in
      Eio.Buf_write.with_flow flow (fun w -> f r w)

let ask ~sw ~net ~root request =
  with_connection ~sw ~net ~root @@ fun r w ->
  Line.write request_line w request;
  Eio.Buf_write.flush w;
  match Line.read answer_of r with
  | `Msg a -> Ok a
  | `Eof ->
      Error "the numpty on this store closed the connection without an answer"
  | `Bad _ ->
      Error "what answered on this store's socket does not speak this protocol"

let stream ~sw ~net ~root ~kinds emit =
  with_connection ~sw ~net ~root @@ fun r w ->
  Line.write request_line w (Follow { kinds });
  Eio.Buf_write.flush w;
  let rec loop () =
    match Line.read answer_of r with
    | `Msg (Lines l) ->
        List.iter emit l.Status.lines;
        loop ()
    | `Msg (Refused what) -> Error what
    | `Msg _ ->
        Error "the numpty on this store answered a follow with something else"
    | `Eof -> Ok ()
    | `Bad _ ->
        Error
          "what answered on this store's socket does not speak this protocol"
  in
  loop ()
