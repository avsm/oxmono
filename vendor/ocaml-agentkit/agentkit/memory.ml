(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type kind = Fact | Open_item | Reference | Procedure

let kind_name = function
  | Fact -> "fact"
  | Open_item -> "open_item"
  | Reference -> "reference"
  | Procedure -> "procedure"

let kinds =
  [
    ("fact", Fact);
    ("open_item", Open_item);
    ("reference", Reference);
    ("procedure", Procedure);
  ]

let kind_of_name s = List.assoc_opt s kinds

type entry = {
  id : string;
  kind : kind;
  title : string;
  body : string;
  tags : string list;
  created : int;
  updated : int;
}

type snapshot = {
  version : int;
  parent : int;
  time : string;
  seq : int;
  cause : string;
  entries : entry list;
}

(* An entry's text is what a model wrote, so it is scrubbed on the way out for
   the reason the journal's is. *)
let string = Jsont.map ~dec:Fun.id ~enc:Fun.id Jsont.string
let kind_jsont = Jsont.enum ~kind:"memory entry kind" kinds

let entry_jsont =
  Jsont.Object.map ~kind:"memory entry"
    (fun id kind title body tags created updated : entry ->
      { id; kind; title; body; tags; created; updated })
  |> Jsont.Object.mem "id" string ~enc:(fun (e : entry) -> e.id)
  |> Jsont.Object.mem "kind" kind_jsont ~enc:(fun (e : entry) -> e.kind)
  |> Jsont.Object.mem "title" string ~enc:(fun (e : entry) -> e.title)
  |> Jsont.Object.mem "body" string ~enc:(fun (e : entry) -> e.body)
  |> Jsont.Object.mem "tags" (Jsont.list string) ~enc:(fun (e : entry) ->
      e.tags)
  |> Jsont.Object.mem "created" Jsont.int ~enc:(fun (e : entry) -> e.created)
  |> Jsont.Object.mem "updated" Jsont.int ~enc:(fun (e : entry) -> e.updated)
  |> Jsont.Object.finish

let jsont =
  Jsont.Object.map ~kind:"memory version"
    (fun version parent time seq cause entries : snapshot ->
      { version; parent; time; seq; cause; entries })
  |> Jsont.Object.mem "version" Jsont.int ~enc:(fun (s : snapshot) -> s.version)
  |> Jsont.Object.mem "parent" Jsont.int ~enc:(fun (s : snapshot) -> s.parent)
  |> Jsont.Object.mem "t" string ~enc:(fun (s : snapshot) -> s.time)
  |> Jsont.Object.mem "seq" Jsont.int ~enc:(fun (s : snapshot) -> s.seq)
  |> Jsont.Object.mem "cause" string ~enc:(fun (s : snapshot) -> s.cause)
  |> Jsont.Object.mem "entries" (Jsont.list entry_jsont)
       ~enc:(fun (s : snapshot) -> s.entries)
  |> Jsont.Object.finish

(* The store. *)

type dir = Eio.Fs.dir_ty Eio.Path.t
type t = { dir : dir; now : unit -> float }

exception Version_exists of int
exception No_version of int
exception No_entry of string
exception Corrupt of { path : string; msg : string }

let () =
  Printexc.register_printer (function
    | Version_exists v ->
        Some (Printf.sprintf "memory version %06d is already written" v)
    | No_version v -> Some (Printf.sprintf "no memory version %06d" v)
    | No_entry id -> Some (Printf.sprintf "no memory entry %S" id)
    | Corrupt { path; msg } -> Some (Printf.sprintf "memory %s: %s" path msg)
    | _ -> None)
  [@alert "-unsafe_multidomain"]

let current_name = "current"
let pending_name = "current.pending"
let snapshot_name v = Printf.sprintf "%06d.json" v

let create ~clock dir =
  let dir = (dir :> dir) in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 dir;
  { dir; now = (fun () -> Eio.Time.now clock) }

(* A version is on disk before anything says it is in force, so every write
   here is fsynced before the next step is allowed to happen. *)
let save_sync t name ~create data =
  Eio.Switch.run @@ fun sw ->
  let file = Eio.Path.open_out ~sw ~create Eio.Path.(t.dir / name) in
  Eio.Flow.copy_string data file;
  Eio.File.sync file

let version t =
  let path = Eio.Path.(t.dir / current_name) in
  if not (Eio.Path.is_file path) then 0
  else
    let text = String.trim (Eio.Path.load path) in
    match int_of_string_opt text with
    | Some v when v >= 1 -> v
    | Some _ | None ->
        raise
          (Corrupt
             {
               path = current_name;
               msg =
                 Printf.sprintf
                   "%S is not a version number, which counts from 1" text;
             })

(* [current] is renamed over rather than written in place, so a reader never
   meets it half written. *)
let set_version t v =
  save_sync t pending_name ~create:(`Or_truncate 0o600) (string_of_int v ^ "\n");
  Eio.Path.rename
    Eio.Path.(t.dir / pending_name)
    Eio.Path.(t.dir / current_name)

let is_snapshot name =
  String.length name = 11
  && String.ends_with ~suffix:".json" name
  &&
  let rec digits i =
    i = 6 || (name.[i] >= '0' && name.[i] <= '9' && digits (i + 1))
  in
  digits 0

let versions t =
  let names = List.filter is_snapshot (Eio.Path.read_dir t.dir) in
  List.sort compare
    (List.map (fun name -> int_of_string (String.sub name 0 6)) names)

let read t v =
  let name = snapshot_name v in
  let path = Eio.Path.(t.dir / name) in
  if not (Eio.Path.is_file path) then raise (No_version v);
  match Jsont_bytesrw.decode_string jsont (Eio.Path.load path) with
  | Ok s -> s
  | Error msg -> raise (Corrupt { path = name; msg })

let read_current t = match version t with 0 -> None | v -> Some (read t v)
let entries t = match read_current t with None -> [] | Some s -> s.entries

(* Every mutation goes through here, so the order the three writes happen in is
   stated once. *)
let mint t ~seq ~cause ~journal ~entry ~update =
  let from = version t in
  let to_ = from + 1 in
  let name = snapshot_name to_ in
  if Eio.Path.is_file Eio.Path.(t.dir / name) then raise (Version_exists to_);
  let before = if from = 0 then [] else (read t from).entries in
  let entries = update to_ before in
  let snapshot =
    {
      version = to_;
      parent = from;
      time = Journal.rfc3339 (t.now ());
      seq;
      cause;
      entries;
    }
  in
  let text =
    match Jsont_bytesrw.encode_string jsont snapshot with
    | Ok text -> text
    | Error msg -> raise (Corrupt { path = name; msg })
  in
  save_sync t name ~create:(`Exclusive 0o600) (text ^ "\n");
  journal { Journal.from; to_; entry; why = cause };
  set_version t to_;
  to_

let write t ~seq ~cause ~journal ~id ~kind ~title ~body ~tags =
  mint t ~seq ~cause ~journal ~entry:id ~update:(fun v before ->
      let created =
        match List.find_opt (fun (e : entry) -> e.id = id) before with
        | Some e -> e.created
        | None -> v
      in
      let e = { id; kind; title; body; tags; created; updated = v } in
      if List.exists (fun (o : entry) -> o.id = id) before then
        List.map (fun (o : entry) -> if o.id = id then e else o) before
      else before @ [ e ])

let forget t ~seq ~cause ~journal id =
  mint t ~seq ~cause ~journal ~entry:id ~update:(fun _ before ->
      if not (List.exists (fun (e : entry) -> e.id = id) before) then
        raise (No_entry id);
      List.filter (fun (e : entry) -> e.id <> id) before)

type recovered = { adopted : int option; removed : int list }

let recover t ~journalled =
  let current = version t in
  let above = List.filter (fun v -> v > current) (versions t) in
  let named, removed = List.partition (fun v -> v <= journalled) above in
  List.iter
    (fun v -> Eio.Path.unlink Eio.Path.(t.dir / snapshot_name v))
    removed;
  let adopted =
    match List.rev named with
    | [] -> None
    | highest :: _ ->
        set_version t highest;
        Some highest
  in
  { adopted; removed }
