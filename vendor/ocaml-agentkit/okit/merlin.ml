(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

type t = {
  proc : [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t;
  root : Eio.Fs.dir_ty Eio.Path.t;
  trace : string -> unit;
}

(* A trace line is shown in a narrow column, where only the first line of a
   message fits. The caller still gets the whole of it. *)
let first_line s =
  match String.index_opt s '\n' with None -> s | Some i -> String.sub s 0 i

(* The callback is the caller's, and a query must not fail because it raised.
   Cancellation is a fiber ending rather than a fault of the callback's, so it
   is passed on. *)
let guarded trace s =
  try trace s with Eio.Cancel.Cancelled _ as e -> raise e | _ -> ()

type pos = { line : int; col : int }

type outline_item = {
  name : string;
  kind : string;
  typ : string option;
  pos : pos;
  children : outline_item list;
}

(* Merlin asks dune for the configuration of a file, and that dune has to find
   the same workspace root and the same build directory as the server okit
   started. INSIDE_DUNE makes it take the directory it starts in for the root
   without looking for one, and DUNE_BUILD_DIR and DUNE_RPC point it at a build
   and a server that are not this workspace's, so all three are dropped here as
   they are for the server. *)
let environment () =
  let dropped = [ "INSIDE_DUNE="; "DUNE_BUILD_DIR="; "DUNE_RPC=" ] in
  let keep v =
    not (List.exists (fun prefix -> String.starts_with ~prefix v) dropped)
  in
  Array.of_list (List.filter keep (Array.to_list (Unix.environment ())))

let find ?(trace = ignore) ~proc ~root () =
  (* Merlin writes its version on standard output and nothing on standard
     error, but both are caught so that a binary of another name behind
     ocamlmerlin cannot write over the caller's own output. *)
  let said = Buffer.create 64 in
  let sink = Eio.Flow.buffer_sink said in
  match
    (* An empty standard input rather than the caller's own, which for an okitd
       is the pipe its protocol arrives on. A query passes the source the same
       way, so nothing merlin runs ever sees it. *)
    Eio.Process.run proc
      ~stdin:(Eio.Flow.string_source "")
      ~stdout:sink ~stderr:sink
      [ "ocamlmerlin"; "-version" ]
  with
  | () ->
      Some
        {
          proc :> [ `Generic ] Eio.Process.mgr_ty Eio.Resource.t;
          root;
          (* Wrapped once, so that every query this merlin runs traces
             safely. *)
          trace = guarded trace;
        }
  | exception Eio.Exn.Io _ -> None

let trace t s = t.trace s

(* ------------------------------------------------------------------ *)
(* Reading a reply                                                     *)
(* ------------------------------------------------------------------ *)

let member k = function
  | Jsont.Object (mems, _) -> Option.map snd (Jsont.Json.find_mem k mems)
  | _ -> None

let string_field j k =
  match member k j with Some (Jsont.String (s, _)) -> Some s | _ -> None

let text = function
  | Jsont.String (s, _) -> s
  | j -> Dsml.Json.Value.to_string j

let malformed ~what ~field =
  Error
    (Printf.sprintf
       "ocamlmerlin answered a %s with no %s. okit reads the JSON that \
        ocamlmerlin single writes, as of merlin 5.8."
       what field)

let rec collect f = function
  | [] -> Ok []
  | x :: xs ->
      let* y = f x in
      let* ys = collect f xs in
      Ok (y :: ys)

let pos_of j =
  match (member "line" j, member "col" j) with
  | Some (Jsont.Number (line, _)), Some (Jsont.Number (col, _)) ->
      Some { line = int_of_float line; col = int_of_float col }
  | _ -> None

(* Every reply is an object stating the class of the answer and its value. A
   class other than "return" means merlin declined the query, and the value is
   what it has to say about that. *)
let value_of out =
  match Dsml.Json.Value.of_string out with
  | Error e ->
      Error (Printf.sprintf "okit could not read what ocamlmerlin wrote: %s" e)
  | Ok reply -> (
      match (member "class" reply, member "value" reply) with
      | Some (Jsont.String ("return", _)), Some value -> Ok value
      | Some (Jsont.String (cls, _)), value ->
          Error
            (Printf.sprintf "ocamlmerlin answered %s: %s" cls
               (match value with Some v -> text v | None -> "no explanation"))
      | _ ->
          Error
            "ocamlmerlin answered without a class. okit reads the JSON that \
             ocamlmerlin single writes, as of merlin 5.8.")

let position p = Printf.sprintf "%d:%d" p.line p.col

(* The source goes on standard input, so a query answers about the text it is
   given rather than about whatever is on disk under [path]. *)
let query t ~path ~source args =
  let said = Buffer.create 256 in
  let name = match args with a :: _ -> a | [] -> "query" in
  t.trace (Printf.sprintf "merlin: %s %s" name path);
  (* Merlin has no Eio clock in reach, and the wall clock is what a person
     reading the column is comparing against anyway. *)
  let started = Unix.gettimeofday () in
  let r =
    match
      Eio.Process.parse_out t.proc Eio.Buf_read.take_all ~cwd:t.root
        ~stdin:(Eio.Flow.string_source source)
        ~stderr:(Eio.Flow.buffer_sink said)
        ~env:(environment ())
        (("ocamlmerlin" :: "single" :: args) @ [ "-filename"; path ])
    with
    | exception (Eio.Exn.Io _ as e) ->
        Error
          (Printf.sprintf "ocamlmerlin could not answer for %s: %s%s" path
             (Printexc.to_string e)
             (match String.trim (Buffer.contents said) with
             | "" -> ""
             | out -> "\nmerlin said:\n" ^ out))
    | out -> value_of out
  in
  (match r with
  | Ok _ ->
      t.trace
        (Printf.sprintf "merlin: reply %.1fs" (Unix.gettimeofday () -. started))
  | Error e -> t.trace ("merlin: error " ^ first_line e));
  r

let array_of ~what = function
  | Jsont.Array (items, _) -> Ok items
  | _ ->
      Error
        (Printf.sprintf
           "ocamlmerlin answered the %s query with something other than a list \
            of results."
           what)

(* ------------------------------------------------------------------ *)
(* Queries                                                             *)
(* ------------------------------------------------------------------ *)

(* Merlin 5.8 reports the outline in reverse source order, and other versions
   report it in source order, so the order is imposed here rather than left to
   the version that answers. *)
let by_position a b = compare (a.pos.line, a.pos.col) (b.pos.line, b.pos.col)

let rec outline_item_of j =
  match (string_field j "name", string_field j "kind") with
  | None, _ -> malformed ~what:"outline item" ~field:"name"
  | _, None -> malformed ~what:"outline item" ~field:"kind"
  | Some name, Some kind -> (
      match Option.bind (member "start" j) pos_of with
      | None -> malformed ~what:"outline item" ~field:"start"
      | Some pos ->
          let* children =
            match member "children" j with
            | Some (Jsont.Array (items, _)) -> collect outline_item_of items
            | _ -> Ok []
          in
          Ok
            {
              name;
              kind;
              typ = string_field j "type";
              pos;
              children = List.sort by_position children;
            })

let outline t ~path ~source =
  let* value = query t ~path ~source [ "outline" ] in
  let* items = array_of ~what:"outline" value in
  let* items = collect outline_item_of items in
  Ok (List.sort by_position items)

let type_at t ~path ~source p =
  let* value =
    query t ~path ~source [ "type-enclosing"; "-position"; position p ]
  in
  let* items = array_of ~what:"type-enclosing" value in
  match items with
  | [] ->
      Error
        (Printf.sprintf
           "nothing at line %d column %d of %s has a type. Ask about a \
            position inside an expression."
           p.line p.col path)
  (* The enclosings run from the smallest outwards, and the smallest is the
     one a reader pointing at a position means. *)
  | smallest :: _ -> (
      match string_field smallest "type" with
      | Some typ -> Ok typ
      | None -> malformed ~what:"type-enclosing result" ~field:"type")

let locate t ~path ~source p =
  let* value =
    query t ~path ~source
      [ "locate"; "-look-for"; "ml"; "-position"; position p ]
  in
  match value with
  (* Merlin answers a definition it could not reach with a bare string saying
     why, and one it reached with the file and the position. *)
  | Jsont.String (why, _) -> Ok (`Not_found why)
  | j -> (
      match (string_field j "file", Option.bind (member "pos" j) pos_of) with
      | Some file, Some pos -> Ok (`Found (file, pos))
      | None, _ -> malformed ~what:"locate result" ~field:"file"
      | _, None -> malformed ~what:"locate result" ~field:"pos")

type problem = { at : pos option; warning : bool; message : string }

let errors t ~path ~source =
  let* value = query t ~path ~source [ "errors" ] in
  let* items = array_of ~what:"errors" value in
  collect
    (fun j ->
      match string_field j "message" with
      | None -> malformed ~what:"error" ~field:"message"
      | Some message ->
          Ok
            {
              (* A syntax error the parser could not place has no start. *)
              at = Option.bind (member "start" j) pos_of;
              (* Merlin's other kinds, "typer", "parser" and "env", are all
                 things that stop a build, so the one distinction a reader
                 acts on is whether this is one of them. *)
              warning = string_field j "type" = Some "warning";
              message;
            })
    items

(* ------------------------------------------------------------------ *)
(* Queries whose results name a file of their own                      *)
(* ------------------------------------------------------------------ *)

(* The records below shadow the outline's field names. Everything the outline
   needs is above, so the shadowing reaches no code that reads an outline. *)

type place = { file : string; pos : pos }

(* Merlin states the file only where a result may lie in another one, which for
   occurrences is the project scope alone. *)
let place_of ~what ~path j =
  match Option.bind (member "start" j) pos_of with
  | None -> malformed ~what ~field:"start"
  | Some pos ->
      Ok { file = Option.value ~default:path (string_field j "file"); pos }

let occurrences t ~path ~source p =
  let* value =
    query t ~path ~source
      [ "occurrences"; "-identifier-at"; position p; "-scope"; "project" ]
  in
  let* items = array_of ~what:"occurrences" value in
  collect (place_of ~what:"occurrence" ~path) items

type hit = { name : string; typ : string; place : place }

(* Merlin also answers with a "constructible", the name with a hole for each
   argument. The name and the type state that between them, and a model reading
   both does not need it spelled out again. *)
let hit_of ~path j =
  match (string_field j "name", string_field j "type") with
  | None, _ -> malformed ~what:"search result" ~field:"name"
  | _, None -> malformed ~what:"search result" ~field:"type"
  | Some name, Some typ ->
      let* place = place_of ~what:"search result" ~path j in
      Ok { name; typ; place }

(* A position gives the query the environment to search from, and this query
   answers with every visible module's contents under its qualified name rather
   than with what the position has opened. So the start of the file serves for
   any of them, and a caller has nothing to choose. *)
let start_of_file = { line = 1; col = 0 }

let search t ~path ~source ~query:wanted ~limit =
  let* value =
    query t ~path ~source
      [
        "search-by-type";
        "-position";
        position start_of_file;
        "-query";
        wanted;
        "-limit";
        string_of_int limit;
      ]
  in
  let* items = array_of ~what:"search-by-type" value in
  collect (hit_of ~path) items

type completion = { name : string; kind : string; typ : string }

let completion_of j =
  match (string_field j "name", string_field j "kind") with
  | None, _ -> malformed ~what:"completion" ~field:"name"
  | _, None -> malformed ~what:"completion" ~field:"kind"
  (* Merlin calls the type "desc", and leaves it empty for a completion with
     none to state, such as a module. *)
  | Some name, Some kind ->
      Ok { name; kind; typ = Option.value ~default:"" (string_field j "desc") }

let complete t ~path ~source p ~prefix =
  let* value =
    query t ~path ~source
      [ "complete-prefix"; "-position"; position p; "-prefix"; prefix ]
  in
  match member "entries" value with
  | None -> malformed ~what:"complete-prefix result" ~field:"entries"
  | Some entries ->
      let* items = array_of ~what:"complete-prefix" entries in
      collect completion_of items
