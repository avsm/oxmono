(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Line = Agentkit.Line

type hello = { status : string; dune : bool; merlin : bool }

type op =
  | Build of { targets : string }
  | Test
  | Promote of { path : string }
  | Project of { module_ : string }
  | After_write of { path : string; verb : string }
  | Outline of { path : string; source : string }
  | Errors of { path : string; source : string }
  | Type_at of { path : string; source : string; line : int; col : int }
  | Locate of { path : string; source : string; line : int; col : int }
  | Occurrences of { path : string; source : string; line : int; col : int }
  | Search of { path : string; source : string; query : string; limit : int }
  | Complete of {
      path : string;
      source : string;
      line : int;
      col : int;
      prefix : string;
    }
  | Bash of { command : string }

type call = { id : int; op : op }
type trace = { id : int option; line : string }
type result = { id : int; output : string }
type to_server = Call of call | Shutdown
type to_client = Hello of hello | Trace of trace | Result of result

(* Encoding. Messages are built as generic jsont values and printed compactly,
   the idiom Merlin uses. Member order follows the order given here, which the
   exact-byte tests pin. *)

let jstring v = Jsont.Json.string (Line.utf_8 v)
let jbool v = Jsont.Json.bool v
let jint v = Jsont.Json.number (float_of_int v)
let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobj mems = Jsont.Json.object' mems

let envelope name mems =
  Dsml.Json.Value.to_string (jobj [ jmem name (jobj mems) ])

(* Every merlin operation carries the file it asks about and its text, and most
   of them a position in it, so those members are written once here. *)
let file_mems path source =
  [ jmem "path" (jstring path); jmem "source" (jstring source) ]

let position_mems path source line col =
  file_mems path source @ [ jmem "line" (jint line); jmem "col" (jint col) ]

(* [op_mems op] is the string naming [op] and the members carrying its
   arguments. The arguments sit beside [op] in the call object rather than in a
   nested one, so a call reads as one flat record. *)
let op_mems = function
  | Build { targets } -> ("build", [ jmem "targets" (jstring targets) ])
  | Test -> ("test", [])
  | Promote { path } -> ("promote", [ jmem "path" (jstring path) ])
  | Project { module_ } -> ("project", [ jmem "module" (jstring module_) ])
  | After_write { path; verb } ->
      ("after_write", [ jmem "path" (jstring path); jmem "verb" (jstring verb) ])
  | Outline { path; source } -> ("outline", file_mems path source)
  | Errors { path; source } -> ("errors", file_mems path source)
  | Type_at { path; source; line; col } ->
      ("type_at", position_mems path source line col)
  | Locate { path; source; line; col } ->
      ("locate", position_mems path source line col)
  | Occurrences { path; source; line; col } ->
      ("occurrences", position_mems path source line col)
  | Search { path; source; query; limit } ->
      ( "search",
        file_mems path source
        @ [ jmem "query" (jstring query); jmem "limit" (jint limit) ] )
  | Complete { path; source; line; col; prefix } ->
      ( "complete",
        position_mems path source line col @ [ jmem "prefix" (jstring prefix) ]
      )
  | Bash { command } -> ("bash", [ jmem "command" (jstring command) ])

let to_server_line = function
  | Call { id; op } ->
      let name, args = op_mems op in
      envelope "call" (jmem "id" (jint id) :: jmem "op" (jstring name) :: args)
  | Shutdown -> envelope "shutdown" []

let to_client_line = function
  | Hello { status; dune; merlin } ->
      envelope "hello"
        [
          jmem "status" (jstring status);
          jmem "dune" (jbool dune);
          jmem "merlin" (jbool merlin);
        ]
  | Trace { id; line } ->
      let id =
        match id with None -> [] | Some id -> [ jmem "id" (jint id) ]
      in
      envelope "trace" (id @ [ jmem "line" (jstring line) ])
  | Result { id; output } ->
      envelope "result" [ jmem "id" (jint id); jmem "output" (jstring output) ]

(* Decoding. A member of the wrong type is as absent, and an absent member
   fails the whole message, so every failure reaches the caller as [`Bad]. *)

let ( let* ) = Option.bind
let member k mems = Option.map snd (Jsont.Json.find_mem k mems)

let string_mem k mems =
  match member k mems with Some (Jsont.String (v, _)) -> Some v | _ -> None

let bool_mem k mems =
  match member k mems with Some (Jsont.Bool (v, _)) -> Some v | _ -> None

(* The integers on the wire count calls, lines and columns, so none of them
   reaches this bound. A fractional number, a number past the bound and a
   non-finite one are faults rather than numbers to round or to wrap, and
   [int_of_float] on them is unspecified. *)
let max_wire_int = 4_294_967_295.

let int_mem k mems =
  match member k mems with
  | Some (Jsont.Number (v, _))
    when Float.is_integer v && Float.abs v <= max_wire_int ->
      Some (int_of_float v)
  | _ -> None

let opt_int_mem k mems =
  match member k mems with
  | None -> Some None
  | Some _ -> Option.map Option.some (int_mem k mems)

(* [payload line] is the message name and the members of its object, when
   [line] is an object of exactly one member holding an object. *)
let payload line =
  match Dsml.Json.Value.of_string line with
  | Ok (Jsont.Object ([ (name, Jsont.Object (mems, _)) ], _)) ->
      Some (fst name, mems)
  | Ok _ | Error _ -> None

(* The counterparts of [file_mems] and [position_mems]. Each hands the members
   it read to the operation that wanted them, so an absent one fails the whole
   message wherever it was written. *)
let file_op mems f =
  let* path = string_mem "path" mems in
  let* source = string_mem "source" mems in
  f ~path ~source

let position_op mems f =
  file_op mems @@ fun ~path ~source ->
  let* line = int_mem "line" mems in
  let* col = int_mem "col" mems in
  f ~path ~source ~line ~col

let op_of mems =
  match string_mem "op" mems with
  | Some "build" ->
      let* targets = string_mem "targets" mems in
      Some (Build { targets })
  | Some "test" -> Some Test
  | Some "promote" ->
      let* path = string_mem "path" mems in
      Some (Promote { path })
  | Some "project" ->
      let* module_ = string_mem "module" mems in
      Some (Project { module_ })
  | Some "after_write" ->
      let* path = string_mem "path" mems in
      let* verb = string_mem "verb" mems in
      Some (After_write { path; verb })
  | Some "outline" ->
      file_op mems (fun ~path ~source -> Some (Outline { path; source }))
  | Some "errors" ->
      file_op mems (fun ~path ~source -> Some (Errors { path; source }))
  | Some "type_at" ->
      position_op mems (fun ~path ~source ~line ~col ->
          Some (Type_at { path; source; line; col }))
  | Some "locate" ->
      position_op mems (fun ~path ~source ~line ~col ->
          Some (Locate { path; source; line; col }))
  | Some "occurrences" ->
      position_op mems (fun ~path ~source ~line ~col ->
          Some (Occurrences { path; source; line; col }))
  | Some "search" ->
      file_op mems (fun ~path ~source ->
          let* query = string_mem "query" mems in
          let* limit = int_mem "limit" mems in
          Some (Search { path; source; query; limit }))
  | Some "complete" ->
      position_op mems (fun ~path ~source ~line ~col ->
          let* prefix = string_mem "prefix" mems in
          Some (Complete { path; source; line; col; prefix }))
  | Some "bash" ->
      let* command = string_mem "command" mems in
      Some (Bash { command })
  | Some _ | None -> None

let to_server_of line =
  match payload line with
  | Some ("call", mems) ->
      let* id = int_mem "id" mems in
      let* op = op_of mems in
      Some (Call { id; op })
  | Some ("shutdown", _) -> Some Shutdown
  | Some _ | None -> None

let to_client_of line =
  match payload line with
  | Some ("hello", mems) ->
      let* status = string_mem "status" mems in
      let* dune = bool_mem "dune" mems in
      let* merlin = bool_mem "merlin" mems in
      Some (Hello { status; dune; merlin })
  | Some ("trace", mems) ->
      let* id = opt_int_mem "id" mems in
      let* line = string_mem "line" mems in
      Some (Trace { id; line })
  | Some ("result", mems) ->
      let* id = int_mem "id" mems in
      let* output = string_mem "output" mems in
      Some (Result { id; output })
  | Some _ | None -> None

let write_to_server w msg = Line.write to_server_line w msg
let read_to_server r = Line.read to_server_of r
let write_to_client w msg = Line.write to_client_line w msg
let read_to_client r = Line.read to_client_of r
