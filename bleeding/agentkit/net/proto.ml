(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Line = Agentkit.Line

type hello = { status : string; curl : bool }
type render = Text | Raw

type op =
  | Fetch of { url : string; render : render; max_bytes : int option }
  | Head of { url : string }
  | Run of { program : string; args : string list }

type call = { id : int; op : op }
type trace = { id : int option; line : string }
type result = { id : int; output : string }
type to_server = Call of call | Shutdown
type to_client = Hello of hello | Trace of trace | Result of result

let name = function Fetch _ -> "fetch" | Head _ -> "head" | Run _ -> "run"

(* Encoding. Messages are built as generic jsont values and printed compactly,
   as [Okit.Proto] does, and member order follows the order given here. *)

let jstring v = Jsont.Json.string (Line.utf_8 v)
let jbool v = Jsont.Json.bool v
let jint v = Jsont.Json.number (float_of_int v)
let jstrings vs = Jsont.Json.list (List.map jstring vs)
let jmem k v = Jsont.Json.mem (Jsont.Json.name k) v
let jobj mems = Jsont.Json.object' mems

let envelope name mems =
  Dsml.Json.Value.to_string (jobj [ jmem name (jobj mems) ])

let render_word = function Text -> "text" | Raw -> "raw"

(* [op_mems op] is the string naming [op] and the members carrying its
   arguments. The arguments sit beside [op] in the call object rather than in a
   nested one, so a call reads as one flat record. *)
let op_mems = function
  | Fetch { url; render; max_bytes } ->
      ( "fetch",
        [
          jmem "url" (jstring url); jmem "render" (jstring (render_word render));
        ]
        @
        match max_bytes with
        | None -> []
        | Some n -> [ jmem "max_bytes" (jint n) ] )
  | Head { url } -> ("head", [ jmem "url" (jstring url) ])
  | Run { program; args } ->
      ("run", [ jmem "program" (jstring program); jmem "args" (jstrings args) ])

let to_server_line = function
  | Call { id; op } ->
      let name, args = op_mems op in
      envelope "call" (jmem "id" (jint id) :: jmem "op" (jstring name) :: args)
  | Shutdown -> envelope "shutdown" []

let to_client_line = function
  | Hello { status; curl } ->
      envelope "hello"
        [ jmem "status" (jstring status); jmem "curl" (jbool curl) ]
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

(* The integers on the wire count calls and bytes, so none of them reaches this
   bound. A fractional number, a number past the bound and a non-finite one are
   faults rather than numbers to round or to wrap, and [int_of_float] on them is
   unspecified. *)
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

(* An array of strings, refused whole if any element is not a string. A call
   whose arguments lost one element would run a different command. *)
let strings_mem k mems =
  match member k mems with
  | Some (Jsont.Array (elts, _)) ->
      List.fold_right
        (fun elt acc ->
          match (elt, acc) with
          | Jsont.String (v, _), Some vs -> Some (v :: vs)
          | _ -> None)
        elts (Some [])
  | _ -> None

(* [payload line] is the message name and the members of its object, when
   [line] is an object of exactly one member holding an object. *)
let payload line =
  match Dsml.Json.Value.of_string line with
  | Ok (Jsont.Object ([ (name, Jsont.Object (mems, _)) ], _)) ->
      Some (fst name, mems)
  | Ok _ | Error _ -> None

let render_of = function "text" -> Some Text | "raw" -> Some Raw | _ -> None

let op_of mems =
  match string_mem "op" mems with
  | Some "fetch" ->
      let* url = string_mem "url" mems in
      let* render = Option.bind (string_mem "render" mems) render_of in
      let* max_bytes = opt_int_mem "max_bytes" mems in
      Some (Fetch { url; render; max_bytes })
  | Some "head" ->
      let* url = string_mem "url" mems in
      Some (Head { url })
  | Some "run" ->
      let* program = string_mem "program" mems in
      let* args = strings_mem "args" mems in
      Some (Run { program; args })
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
      let* curl = bool_mem "curl" mems in
      Some (Hello { status; curl })
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
