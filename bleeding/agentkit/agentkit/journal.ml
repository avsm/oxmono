(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let schema_version = 1

(* Kinds. *)

type run_start = {
  pid : int;
  version : string;
  backend : string;
  model : string;
  ctx_size : int;
}

type wake = { task : string; due : string; why : string; serial : int option }
type brief = { version : int; open_items : int; bytes : int }
type tool_call = { call : int; name : string; arguments : string }

type tool_result = {
  call : int;
  name : string;
  output : string;
  seconds : float;
  truncated : bool;
}

type continued = { task : string; session : int; previous : int }
type memory_write = { from : int; to_ : int; entry : string; why : string }
type error = { where : string; what : string }
type schedule_load = { tasks : string list; changed : string list }
type unknown = { name : string; json : Jsont.json }
type cut_off = { tokens : int; tool_call : bool }

type kind =
  | Run_start of run_start
  | Run_stop of string
  | Wake of wake
  | Brief of brief
  | Prompt of string
  | Reasoning of string
  | Content of string
  | Tool_call of tool_call
  | Tool_result of tool_result
  | Stats of Agent.stats
  | Expanded of int
  | Squeezed of int
  | Cut_off of cut_off
  | Compacted of Agent.compaction
  | Continued of continued
  | Memory_write of memory_write
  | Handover of int
  | Error of error
  | Schedule_load of schedule_load
  | Unknown of unknown

let kind_name = function
  | Run_start _ -> "run_start"
  | Run_stop _ -> "run_stop"
  | Wake _ -> "wake"
  | Brief _ -> "brief"
  | Prompt _ -> "prompt"
  | Reasoning _ -> "reasoning"
  | Content _ -> "content"
  | Tool_call _ -> "tool_call"
  | Tool_result _ -> "tool_result"
  | Stats _ -> "stats"
  | Expanded _ -> "expanded"
  | Squeezed _ -> "squeezed"
  | Cut_off _ -> "cut_off"
  | Compacted _ -> "compacted"
  | Continued _ -> "continued"
  | Memory_write _ -> "memory_write"
  | Handover _ -> "handover"
  | Error _ -> "error"
  | Schedule_load _ -> "schedule_load"
  | Unknown { name; _ } -> name

let kind_names =
  [
    "run_start";
    "run_stop";
    "wake";
    "brief";
    "prompt";
    "reasoning";
    "content";
    "tool_call";
    "tool_result";
    "stats";
    "expanded";
    "squeezed";
    "cut_off";
    "compacted";
    "continued";
    "memory_write";
    "handover";
    "error";
    "schedule_load";
  ]

type record = { v : int; seq : int; time : string; run : int; kind : kind }

(* Codecs. A journal carries tool output and model replies, which are not
   always valid UTF-8, so every string is scrubbed on the way out. A record the
   codec refused to write would be a hole in the account over somebody else's
   bytes. *)

let string = Jsont.map ~dec:Fun.id ~enc:Fun.id Jsont.string

(* The kinds whose payload is one value are objects of one member, so that
   adding a second member to one of them later is not a change of shape. *)

let text_jsont ~kind name =
  Jsont.Object.map ~kind Fun.id
  |> Jsont.Object.mem name string ~enc:Fun.id
  |> Jsont.Object.finish

let int_jsont ~kind name =
  Jsont.Object.map ~kind Fun.id
  |> Jsont.Object.mem name Jsont.int ~enc:Fun.id
  |> Jsont.Object.finish

let run_start_jsont =
  Jsont.Object.map ~kind:"run_start"
    (fun pid version backend model ctx_size : run_start ->
      { pid; version; backend; model; ctx_size })
  |> Jsont.Object.mem "pid" Jsont.int ~enc:(fun (r : run_start) -> r.pid)
  |> Jsont.Object.mem "version" string ~enc:(fun (r : run_start) -> r.version)
  |> Jsont.Object.mem "backend" string ~enc:(fun (r : run_start) -> r.backend)
  |> Jsont.Object.mem "model" string ~enc:(fun (r : run_start) -> r.model)
  |> Jsont.Object.mem "ctx_size" Jsont.int ~enc:(fun (r : run_start) ->
      r.ctx_size)
  |> Jsont.Object.finish

let wake_jsont =
  Jsont.Object.map ~kind:"wake" (fun task due why serial : wake ->
      { task; due; why; serial })
  |> Jsont.Object.mem "task" string ~enc:(fun (r : wake) -> r.task)
  |> Jsont.Object.mem "due" string ~enc:(fun (r : wake) -> r.due)
  |> Jsont.Object.mem "why" string ~enc:(fun (r : wake) -> r.why)
  |> Jsont.Object.opt_mem "serial" Jsont.int ~enc:(fun (r : wake) -> r.serial)
  |> Jsont.Object.finish

let brief_jsont =
  Jsont.Object.map ~kind:"brief" (fun version open_items bytes : brief ->
      { version; open_items; bytes })
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun (r : brief) -> r.version)
  |> Jsont.Object.mem "open_items" Jsont.int ~enc:(fun (r : brief) ->
      r.open_items)
  |> Jsont.Object.mem "bytes" Jsont.int ~enc:(fun (r : brief) -> r.bytes)
  |> Jsont.Object.finish

let tool_call_jsont =
  Jsont.Object.map ~kind:"tool_call" (fun call name arguments : tool_call ->
      { call; name; arguments })
  |> Jsont.Object.mem "call" Jsont.int ~enc:(fun (r : tool_call) -> r.call)
  |> Jsont.Object.mem "name" string ~enc:(fun (r : tool_call) -> r.name)
  |> Jsont.Object.mem "arguments" string ~enc:(fun (r : tool_call) ->
      r.arguments)
  |> Jsont.Object.finish

let tool_result_jsont =
  Jsont.Object.map ~kind:"tool_result"
    (fun call name output seconds truncated : tool_result ->
      { call; name; output; seconds; truncated })
  |> Jsont.Object.mem "call" Jsont.int ~enc:(fun (r : tool_result) -> r.call)
  |> Jsont.Object.mem "name" string ~enc:(fun (r : tool_result) -> r.name)
  |> Jsont.Object.mem "output" string ~enc:(fun (r : tool_result) -> r.output)
  |> Jsont.Object.mem "seconds" Jsont.number ~enc:(fun (r : tool_result) ->
      r.seconds)
  |> Jsont.Object.mem "truncated" Jsont.bool ~enc:(fun (r : tool_result) ->
      r.truncated)
  |> Jsont.Object.finish

let continued_jsont =
  Jsont.Object.map ~kind:"continued" (fun task session previous : continued ->
      { task; session; previous })
  |> Jsont.Object.mem "task" string ~enc:(fun (r : continued) -> r.task)
  |> Jsont.Object.mem "session" Jsont.int ~enc:(fun (r : continued) ->
      r.session)
  |> Jsont.Object.mem "previous" Jsont.int ~enc:(fun (r : continued) ->
      r.previous)
  |> Jsont.Object.finish

let memory_write_jsont =
  Jsont.Object.map ~kind:"memory_write"
    (fun from to_ entry why : memory_write -> { from; to_; entry; why })
  |> Jsont.Object.mem "from" Jsont.int ~enc:(fun (r : memory_write) -> r.from)
  |> Jsont.Object.mem "to" Jsont.int ~enc:(fun (r : memory_write) -> r.to_)
  |> Jsont.Object.mem "entry" string ~enc:(fun (r : memory_write) -> r.entry)
  |> Jsont.Object.mem "why" string ~enc:(fun (r : memory_write) -> r.why)
  |> Jsont.Object.finish

let error_jsont =
  Jsont.Object.map ~kind:"error" (fun where what : error -> { where; what })
  |> Jsont.Object.mem "where" string ~enc:(fun (r : error) -> r.where)
  |> Jsont.Object.mem "what" string ~enc:(fun (r : error) -> r.what)
  |> Jsont.Object.finish

let schedule_load_jsont =
  Jsont.Object.map ~kind:"schedule_load" (fun tasks changed : schedule_load ->
      { tasks; changed })
  |> Jsont.Object.mem "tasks" (Jsont.list string)
       ~enc:(fun (r : schedule_load) -> r.tasks)
  |> Jsont.Object.mem "changed" (Jsont.list string)
       ~enc:(fun (r : schedule_load) -> r.changed)
  |> Jsont.Object.finish

let stats_jsont =
  let open Agent in
  Jsont.Object.map ~kind:"stats"
    (fun
      ctx_used
      ctx_size
      prompt_tokens
      generated
      generate_seconds
      prefill_seconds
      tool_calls
      turns
      drafted
      total_generated
      total_generate_seconds
      :
      stats
    ->
      {
        ctx_used;
        ctx_size;
        prompt_tokens;
        generated;
        generate_seconds;
        prefill_seconds;
        tool_calls;
        turns;
        drafted;
        total_generated;
        total_generate_seconds;
      })
  |> Jsont.Object.mem "ctx_used" Jsont.int ~enc:(fun (s : stats) -> s.ctx_used)
  |> Jsont.Object.mem "ctx_size" Jsont.int ~enc:(fun (s : stats) -> s.ctx_size)
  |> Jsont.Object.mem "prompt_tokens" Jsont.int ~enc:(fun (s : stats) ->
      s.prompt_tokens)
  |> Jsont.Object.mem "generated" Jsont.int ~enc:(fun (s : stats) ->
      s.generated)
  |> Jsont.Object.mem "generate_seconds" Jsont.number ~enc:(fun (s : stats) ->
      s.generate_seconds)
  |> Jsont.Object.mem "prefill_seconds" Jsont.number ~enc:(fun (s : stats) ->
      s.prefill_seconds)
  |> Jsont.Object.mem "tool_calls" Jsont.int ~enc:(fun (s : stats) ->
      s.tool_calls)
  |> Jsont.Object.mem "turns" Jsont.int ~enc:(fun (s : stats) -> s.turns)
  (* Added after this shape shipped, so a journal written before it must still
     read: a record with no [drafted] member held nothing that was drafted. *)
  |> Jsont.Object.mem "drafted" Jsont.int ~dec_absent:0 ~enc:(fun (s : stats) ->
      s.drafted)
  |> Jsont.Object.mem "total_generated" Jsont.int ~enc:(fun (s : stats) ->
      s.total_generated)
  |> Jsont.Object.mem "total_generate_seconds" Jsont.number
       ~enc:(fun (s : stats) -> s.total_generate_seconds)
  |> Jsont.Object.finish

let run_stop_jsont = text_jsont ~kind:"run_stop" "why"
let prompt_jsont = text_jsont ~kind:"prompt" "text"
let reasoning_jsont = text_jsont ~kind:"reasoning" "text"
let content_jsont = text_jsont ~kind:"content" "text"
let expanded_jsont = int_jsont ~kind:"expanded" "ctx_size"
let squeezed_jsont = int_jsont ~kind:"squeezed" "tokens"

let cut_off_jsont =
  Jsont.Object.map ~kind:"cut_off" (fun tokens tool_call : cut_off ->
      { tokens; tool_call })
  |> Jsont.Object.mem "tokens" Jsont.int ~enc:(fun (r : cut_off) -> r.tokens)
  |> Jsont.Object.mem "tool_call" Jsont.bool ~enc:(fun (r : cut_off) ->
      r.tool_call)
  |> Jsont.Object.finish

let compacted_jsont =
  let open Agent in
  Jsont.Object.map ~kind:"compacted" (fun before after summary : compaction ->
      { before; after; summary })
  |> Jsont.Object.mem "before" Jsont.int ~enc:(fun (r : compaction) -> r.before)
  |> Jsont.Object.mem "after" Jsont.int ~enc:(fun (r : compaction) -> r.after)
  |> Jsont.Object.mem "summary" string ~enc:(fun (r : compaction) -> r.summary)
  |> Jsont.Object.finish

let handover_jsont = int_jsont ~kind:"handover" "version"

(* The kind member. Its name selects the payload codec, so a name this build
   does not know is carried whole rather than refused: a later journal read by
   an earlier build must still show every record in it. *)

let dec_kind name json =
  let dec codec inject =
    match Jsont.Json.decode codec json with
    | Ok v -> inject v
    | Error e -> Jsont.Error.msgf Jsont.Meta.none "journal %s: %s" name e
  in
  match name with
  | "run_start" -> dec run_start_jsont (fun v -> Run_start v)
  | "run_stop" -> dec run_stop_jsont (fun v -> Run_stop v)
  | "wake" -> dec wake_jsont (fun v -> Wake v)
  | "brief" -> dec brief_jsont (fun v -> Brief v)
  | "prompt" -> dec prompt_jsont (fun v -> Prompt v)
  | "reasoning" -> dec reasoning_jsont (fun v -> Reasoning v)
  | "content" -> dec content_jsont (fun v -> Content v)
  | "tool_call" -> dec tool_call_jsont (fun v -> Tool_call v)
  | "tool_result" -> dec tool_result_jsont (fun v -> Tool_result v)
  | "stats" -> dec stats_jsont (fun v -> Stats v)
  | "expanded" -> dec expanded_jsont (fun v -> Expanded v)
  | "squeezed" -> dec squeezed_jsont (fun v -> Squeezed v)
  | "cut_off" -> dec cut_off_jsont (fun v -> Cut_off v)
  | "compacted" -> dec compacted_jsont (fun v -> Compacted v)
  | "continued" -> dec continued_jsont (fun v -> Continued v)
  | "memory_write" -> dec memory_write_jsont (fun v -> Memory_write v)
  | "handover" -> dec handover_jsont (fun v -> Handover v)
  | "error" -> dec error_jsont (fun v -> Error v)
  | "schedule_load" -> dec schedule_load_jsont (fun v -> Schedule_load v)
  | _ -> Unknown { name; json }

let kind_of_mems = function
  | Jsont.Object ([ ((name, _), json) ], _) -> dec_kind name json
  | Jsont.Object ([], _) ->
      Jsont.Error.msg Jsont.Meta.none
        "a journal record has a fifth member naming its kind, and this one has \
         none"
  | Jsont.Object (mems, _) ->
      Jsont.Error.msgf Jsont.Meta.none
        "a journal record names one kind, and this one names %d: %s"
        (List.length mems)
        (String.concat ", " (Jsont.Json.object_names mems))
  | _ -> Jsont.Error.msg Jsont.Meta.none "a journal record is a JSON object"

let mems_of_kind k =
  let one json =
    Jsont.Json.object' [ Jsont.Json.mem (Jsont.Json.name (kind_name k)) json ]
  in
  let enc codec v =
    match Jsont.Json.encode codec v with
    | Ok json -> one json
    | Error e ->
        Jsont.Error.msgf Jsont.Meta.none "journal %s: %s" (kind_name k) e
  in
  match k with
  | Run_start v -> enc run_start_jsont v
  | Run_stop v -> enc run_stop_jsont v
  | Wake v -> enc wake_jsont v
  | Brief v -> enc brief_jsont v
  | Prompt v -> enc prompt_jsont v
  | Reasoning v -> enc reasoning_jsont v
  | Content v -> enc content_jsont v
  | Tool_call v -> enc tool_call_jsont v
  | Tool_result v -> enc tool_result_jsont v
  | Stats v -> enc stats_jsont v
  | Expanded v -> enc expanded_jsont v
  | Squeezed v -> enc squeezed_jsont v
  | Cut_off v -> enc cut_off_jsont v
  | Compacted v -> enc compacted_jsont v
  | Continued v -> enc continued_jsont v
  | Memory_write v -> enc memory_write_jsont v
  | Handover v -> enc handover_jsont v
  | Error v -> enc error_jsont v
  | Schedule_load v -> enc schedule_load_jsont v
  | Unknown { json; _ } -> one json

(* [v] is refused before the kind is looked at, since a record whose common
   members may have moved cannot be shown honestly under any kind. *)
let jsont =
  Jsont.Object.map ~kind:"journal record" (fun v seq time run mems ->
      if v > schema_version then
        Jsont.Error.msgf Jsont.Meta.none
          "journal record at schema version %d, and this build reads up to %d" v
          schema_version;
      { v; seq; time; run; kind = kind_of_mems mems })
  |> Jsont.Object.mem "v" Jsont.int ~enc:(fun r -> r.v)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun r -> r.seq)
  |> Jsont.Object.mem "t" string ~enc:(fun r -> r.time)
  |> Jsont.Object.mem "run" Jsont.int ~enc:(fun r -> r.run)
  |> Jsont.Object.keep_unknown Jsont.json_mems ~enc:(fun r ->
      mems_of_kind r.kind)
  |> Jsont.Object.finish

let to_string r =
  match Jsont_bytesrw.encode_string jsont r with
  | Ok s -> s
  | Error e -> invalid_arg ("Journal.to_string: " ^ e)

let of_string s = Jsont_bytesrw.decode_string jsont s

(* Time. The journal is UTC throughout, so nothing here consults a zone. *)

let rfc3339 t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ" (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let utc_date t =
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02d" (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday

let stamp ~now ~run ~seq kind =
  { v = schema_version; seq; time = rfc3339 now; run; kind }

(* Writing. *)

type dir = Eio.Fs.dir_ty Eio.Path.t

type t = {
  sw : Eio.Switch.t;
  now : unit -> float;
  dir : dir;
  run : int;
  mutable seq : int;
  mutable segment : (string * Eio.File.rw_ty Eio.Resource.t) option;
  mutable lock : (Unix.file_descr * string) option;
  locked : bool;
}

exception Locked of { path : string; pid : int }

let () =
  Printexc.register_printer (function
    | Locked { path; pid } ->
        Some
          (Printf.sprintf "another agent journal writer holds %s as process %d"
             path pid)
    | _ -> None)
  [@alert "-unsafe_multidomain"]

let held : (string, int) Hashtbl.t = Hashtbl.create 4

let read_pid fd =
  let buf = Bytes.create 32 in
  ignore (Unix.lseek fd 0 Unix.SEEK_SET);
  let n = try Unix.read fd buf 0 32 with Unix.Unix_error _ -> 0 in
  Option.value ~default:0
    (int_of_string_opt (String.trim (Bytes.sub_string buf 0 n)))

let write_pid fd =
  Unix.ftruncate fd 0;
  ignore (Unix.lseek fd 0 Unix.SEEK_SET);
  let text = string_of_int (Unix.getpid ()) ^ "\n" in
  ignore (Unix.write_substring fd text 0 (String.length text))

let take_lock path =
  (match Hashtbl.find_opt held path with
  | Some pid -> raise (Locked { path; pid })
  | None -> ());
  let fd =
    Unix.openfile path [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_CLOEXEC ] 0o600
  in
  match Unix.lockf fd Unix.F_TLOCK 0 with
  | () ->
      write_pid fd;
      Hashtbl.replace held path (Unix.getpid ());
      (fd, path)
  | exception e -> (
      let pid = read_pid fd in
      Unix.close fd;
      match e with
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EACCES | Unix.EDEADLK), _, _) ->
          raise (Locked { path; pid })
      | e -> raise e)

let create_with_lock ~sw ~clock ~run ~seq dir lock =
  let dir = (dir :> dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  {
    sw;
    now = (fun () -> Eio.Time.now clock);
    dir;
    run;
    seq;
    segment = None;
    lock;
    locked = Option.is_some lock;
  }

let create ~sw ~clock ~run ~seq dir =
  create_with_lock ~sw ~clock ~run ~seq dir None

let next_seq t = t.seq
let run t = t.run

let close_segment t =
  match t.segment with
  | None -> ()
  | Some (_, file) ->
      t.segment <- None;
      Eio.Resource.close file

let close t =
  close_segment t;
  match t.lock with
  | None -> ()
  | Some (fd, path) -> (
      t.lock <- None;
      Hashtbl.remove held path;
      try Unix.close fd with Unix.Unix_error _ -> ())

let segment_name date = date ^ ".jsonl"

(* The open segment is dropped before the new one is opened, so an open that
   fails leaves the writer with no segment rather than with the wrong one. *)
let segment t date =
  match t.segment with
  | Some (d, file) when String.equal d date -> file
  | _ ->
      close_segment t;
      let file =
        Eio.Path.open_out ~sw:t.sw ~append:true ~create:(`If_missing 0o600)
          Eio.Path.(t.dir / segment_name date)
      in
      t.segment <- Some (date, file);
      file

let append t kind =
  if t.locked && t.lock = None then
    invalid_arg "Journal.append: the journal is closed";
  let now = t.now () in
  let r = stamp ~now ~run:t.run ~seq:t.seq kind in
  let file = segment t (utc_date now) in
  Eio.Flow.copy_string (to_string r ^ "\n") file;
  Eio.File.sync file;
  t.seq <- t.seq + 1;
  r

(* Reading. *)

type next = { next_seq : int; next_run : int }

exception Bad_record of { segment : string; line : int; msg : string }

let () =
  Printexc.register_printer (function
    | Bad_record { segment; line; msg } ->
        Some (Printf.sprintf "journal %s line %d: %s" segment line msg)
    | _ -> None)
  [@alert "-unsafe_multidomain"]

(* A segment is named for the UTC date it holds. Anything else in the
   directory belongs to somebody else and is left alone. *)
let is_segment name =
  String.length name = 16
  && String.ends_with ~suffix:".jsonl" name
  &&
  let digit i = match name.[i] with '0' .. '9' -> true | _ -> false in
  digit 0 && digit 1 && digit 2 && digit 3
  && name.[4] = '-'
  && digit 5 && digit 6
  && name.[7] = '-'
  && digit 8 && digit 9

let segments dir =
  if not (Eio.Path.is_directory dir) then []
  else List.sort String.compare (List.filter is_segment (Eio.Path.read_dir dir))

let iter_segment ?kinds dir name f =
  let keep =
    match kinds with
    | None -> fun _ -> true
    | Some kinds -> fun r -> List.mem (kind_name r.kind) kinds
  in
  Eio.Path.with_open_in Eio.Path.(dir / name) @@ fun flow ->
  let buf = Eio.Buf_read.of_flow ~max_size:Line.max_line flow in
  let n = ref 0 in
  Seq.iter
    (fun line ->
      incr n;
      if String.trim line <> "" then
        match of_string line with
        | Ok r -> if keep r then f r
        | Error msg -> raise (Bad_record { segment = name; line = !n; msg }))
    (Eio.Buf_read.lines buf)

let iter ?kinds dir f =
  List.iter (fun name -> iter_segment ?kinds dir name f) (segments dir)

let recover dir =
  let last name =
    let seen = ref None in
    iter_segment dir name (fun r -> seen := Some r);
    !seen
  in
  let rec newest = function
    | [] -> { next_seq = 1; next_run = 1 }
    | name :: older -> (
        match last name with
        | Some r -> { next_seq = r.seq + 1; next_run = r.run + 1 }
        | None -> newest older)
  in
  newest (List.rev (segments dir))

let open_ ~sw ~clock dir =
  let dir = (dir :> dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  let lock = take_lock (Eio.Path.native_exn Eio.Path.(dir / "lock")) in
  match recover dir with
  | next ->
      let t =
        create_with_lock ~sw ~clock ~run:next.next_run ~seq:next.next_seq dir
          (Some lock)
      in
      Eio.Switch.on_release sw (fun () -> close t);
      t
  | exception e ->
      let fd, path = lock in
      Hashtbl.remove held path;
      Unix.close fd;
      raise e
