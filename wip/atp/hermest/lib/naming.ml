(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

(* ocaml reserved keywords that need escaping *)
let reserved_keywords =
  [
    "and";
    "as";
    "assert";
    "asr";
    "begin";
    "class";
    "constraint";
    "do";
    "done";
    "downto";
    "else";
    "end";
    "exception";
    "external";
    "false";
    "for";
    "fun";
    "function";
    "functor";
    "if";
    "in";
    "include";
    "inherit";
    "initializer";
    "land";
    "lazy";
    "let";
    "lor";
    "lsl";
    "lsr";
    "lxor";
    "match";
    "method";
    "mod";
    "module";
    "mutable";
    "new";
    "nonrec";
    "object";
    "of";
    "open";
    "or";
    "private";
    "rec";
    "sig";
    "struct";
    "then";
    "to";
    "true";
    "try";
    "type";
    "val";
    "virtual";
    "when";
    "while";
    "with";
    "option";
    "list";
    "result";
    "unit";
    "int";
    "string";
    "bool";
    "float";
    "char";
    "bytes";
    "array";
    "ref";
  ]

let is_reserved name = List.mem (String.lowercase_ascii name) reserved_keywords

(* convert camelCase to snake_case *)
let camel_to_snake s =
  let buf = Buffer.create (String.length s * 2) in
  String.iteri
    (fun i c ->
      if Char.uppercase_ascii c = c && c <> Char.lowercase_ascii c then begin
        if i > 0 then Buffer.add_char buf '_';
        Buffer.add_char buf (Char.lowercase_ascii c)
      end
      else Buffer.add_char buf c)
    s;
  Buffer.contents buf

let escape_keyword name = if is_reserved name then name ^ "_" else name
let field_name name = escape_keyword (camel_to_snake name)

let module_name_of_segment segment =
  if String.length segment = 0 then segment else String.capitalize_ascii segment

let module_path_of_nsid nsid =
  String.split_on_char '.' nsid |> List.map module_name_of_segment

let type_name_of_nsid nsid =
  let segments = String.split_on_char '.' nsid in
  match List.rev segments with
  | last :: _ -> camel_to_snake last
  | [] -> "unknown"

let type_name name = escape_keyword (camel_to_snake name)
let def_module_name name = String.capitalize_ascii name

(* Parse ref string into components *)
type ref_parts = { nsid : string option; def : string option }

let parse_ref ref_str =
  if String.length ref_str > 0 && ref_str.[0] = '#' then
    (* local ref: "#localDef" *)
    {
      nsid = None;
      def = Some (String.sub ref_str 1 (String.length ref_str - 1));
    }
  else
    match String.split_on_char '#' ref_str with
    | [ nsid; def ] -> { nsid = Some nsid; def = Some def }
    | [ nsid ] -> { nsid = Some nsid; def = None }
    | _ -> { nsid = None; def = None }

let last_segment nsid =
  match List.rev (String.split_on_char '.' nsid) with
  | last :: _ -> last
  | [] -> "Unknown"

(* generate variant constructor name from ref *)
(* "#localDef" -> "LocalDef", "com.example.defs#someDef" -> "SomeDef" *)
let variant_name_of_ref ref_str =
  let parts = parse_ref ref_str in
  let name =
    match (parts.def, parts.nsid) with
    | Some def, _ -> def
    | None, Some nsid -> last_segment nsid
    | None, None -> "Unknown"
  in
  String.capitalize_ascii name

(* generate qualified variant name including last nsid segment to avoid conflicts *)
(* "app.bsky.embed.images#view" -> "ImagesView" *)
(* "app.bsky.embed.images" (no #) -> "Images" (refers to main) *)
(* "#localDef" -> "LocalDef" (no qualifier for local refs) *)
let qualified_variant_name_of_ref ref_str =
  let parts = parse_ref ref_str in
  match (parts.nsid, parts.def) with
  | Some nsid, Some def ->
      (* external ref with def: use last segment of nsid as qualifier *)
      String.capitalize_ascii (last_segment nsid) ^ String.capitalize_ascii def
  | Some nsid, None ->
      (* just nsid, refers to main def *)
      String.capitalize_ascii (last_segment nsid)
  | None, Some def ->
      (* local ref *)
      String.capitalize_ascii def
  | None, None -> String.capitalize_ascii ref_str

let union_type_name refs =
  match refs with
  | [] -> "unknown_union"
  | [ r ] -> type_name (variant_name_of_ref r)
  | _ -> (
      (* use first two refs to generate a name *)
      let names = List.map variant_name_of_ref refs in
      let sorted = List.sort String.compare names in
      match sorted with
      | a :: b :: _ -> camel_to_snake a ^ "_or_" ^ camel_to_snake b
      | [ a ] -> camel_to_snake a
      | [] -> "unknown_union")

(* convert nsid to flat file path and module name *)
let flat_name_of_nsid nsid = String.split_on_char '.' nsid |> String.concat "_"
let file_path_of_nsid nsid = flat_name_of_nsid nsid ^ ".ml"

let flat_module_name_of_nsid nsid =
  String.capitalize_ascii (flat_name_of_nsid nsid)

(** Convert nsid to hierarchical module path string. e.g.
    "app.bsky.notification.defs" -> "App.Bsky.Notification.Defs" *)
let module_path_string nsid = module_path_of_nsid nsid |> String.concat "."

(** Compute relative module path from one nsid to another. Both nsids share a
    common prefix, and we compute the path from the current location to the
    target.

    e.g. from "app.bsky.actor.defs" to "app.bsky.notification.defs" ->
    "Notification.Defs" (since we're inside App.Bsky)

    e.g. from "app.bsky.actor.defs" to "com.atproto.label.defs" ->
    "Com.Atproto.Label.Defs" (no common prefix beyond root) *)
let relative_module_path ~from_nsid ~to_nsid =
  let from_segs = String.split_on_char '.' from_nsid in
  let to_segs = String.split_on_char '.' to_nsid in
  (* Find common prefix length *)
  let rec common_prefix_len acc l1 l2 =
    match (l1, l2) with
    | h1 :: t1, h2 :: t2 when h1 = h2 -> common_prefix_len (acc + 1) t1 t2
    | _ -> acc
  in
  let prefix_len = common_prefix_len 0 from_segs to_segs in
  (* Drop the common prefix from the target path *)
  let rec drop n lst =
    if n <= 0 then lst else match lst with [] -> [] | _ :: t -> drop (n - 1) t
  in
  let target_suffix = drop prefix_len to_segs in
  target_suffix |> List.map String.capitalize_ascii |> String.concat "."

let needs_key_annotation original_name ocaml_name = original_name <> ocaml_name

let key_annotation original_name ocaml_name =
  if needs_key_annotation original_name ocaml_name then
    Printf.sprintf " [@key \"%s\"]" original_name
  else ""

(** find common prefix segments from a list of NSIDs e.g.
    ["app.bsky.actor.defs"; "app.bsky.feed.defs"; "app.bsky.graph.defs"] ->
    ["app"; "bsky"] *)
let common_prefix_of_nsids nsids =
  match nsids with
  | [] -> []
  | first :: rest ->
      let first_segments = String.split_on_char '.' first in
      List.fold_left
        (fun prefix nsid ->
          let segments = String.split_on_char '.' nsid in
          let rec common acc l1 l2 =
            match (l1, l2) with
            | h1 :: t1, h2 :: t2 when h1 = h2 -> common (h1 :: acc) t1 t2
            | _ -> List.rev acc
          in
          common [] prefix segments)
        first_segments rest

(** generate shared module file name from NSIDs e.g.
    ["app.bsky.actor.defs"; "app.bsky.feed.defs"] with index 1 ->
    "app_bsky_shared_1.ml" *)
let shared_file_name nsids index =
  let prefix = common_prefix_of_nsids nsids in
  let prefix_str = String.concat "_" prefix in
  prefix_str ^ "_shared_" ^ string_of_int index ^ ".ml"

(** generate shared module name from NSIDs e.g.
    ["app.bsky.actor.defs"; "app.bsky.feed.defs"] with index 1 ->
    "App_bsky_shared_1" *)
let shared_module_name nsids index =
  let prefix = common_prefix_of_nsids nsids in
  let prefix_str = String.concat "_" prefix in
  String.capitalize_ascii (prefix_str ^ "_shared_" ^ string_of_int index)

(** generate a short type name for use in shared modules uses the last segment
    of the nsid as context e.g. nsid="app.bsky.actor.defs",
    def_name="viewerState" -> "actor_viewer_state" *)
let shared_type_name nsid def_name =
  let segments = String.split_on_char '.' nsid in
  let context =
    match List.rev segments with
    (* use second-last segment if last is "defs" *)
    | "defs" :: second :: _ -> second
    | last :: _ -> last
    | [] -> "unknown"
  in
  type_name (context ^ "_" ^ def_name)

(** group NSIDs by shared prefixes e.g.
    ["app.bsky.actor.defs"; "app.bsky.actor.getProfile"; "app.bsky.graph.defs";
     "com.atproto.sync.getRepo"] ->
    [("app", Node [("bsky", Node [("actor", Node [("defs", Module
     "app.bsky.actor.defs"); ("getProfile", Module
     "app.bsky.actor.getProfile")]); ("graph", Node [("defs", Module
     "app.bsky.graph.defs")])])]); ("com", [("atproto", [("sync", [("getRepo",
     Module "com.atproto.sync.getRepo")])])])]

    Note: Supports NSIDs where a parent has both content AND children. e.g.
    ["sh.tangled.pipeline"; "sh.tangled.pipeline.status"] -> Pipeline has its
    own content AND a Status child. Uses ModuleWithChildren. *)
type trie =
  | Node of (string * trie) list  (** Intermediate node with no nsid *)
  | Module of string  (** Leaf node with nsid, no children *)
  | ModuleWithChildren of string * (string * trie) list
      (** Node with nsid AND children *)

let group_nsids_by_prefix nsids =
  let rec insert_segments trie nsid segments =
    match segments with
    | [] -> (
        (* We've reached the end of segments - this nsid belongs here *)
        match trie with
        | Node (_ :: _ as children) ->
            (* Already have children, convert to ModuleWithChildren *)
            ModuleWithChildren (nsid, children)
        | ModuleWithChildren (_, children) ->
            (* Already a module with children, update the nsid *)
            ModuleWithChildren (nsid, children)
        | Node [] | Module _ ->
            (* No children or was a leaf, just make it a Module *)
            Module nsid)
    | seg :: rest -> (
        let existing_children =
          match trie with
          | Node children -> children
          | Module _ -> []
          | ModuleWithChildren (_, children) -> children
        in
        let existing_nsid =
          match trie with
          | ModuleWithChildren (ns, _) -> Some ns
          | Module ns -> Some ns
          | Node _ -> None
        in
        let existing =
          match List.assoc_opt seg existing_children with
          | Some child -> child
          | None -> Node []
        in
        let updated = insert_segments existing nsid rest in
        let children_without_seg = List.remove_assoc seg existing_children in
        let new_children = (seg, updated) :: children_without_seg in
        match existing_nsid with
        | Some ns -> ModuleWithChildren (ns, new_children)
        | None -> Node new_children)
  in
  (* Sort children alphabetically to ensure deterministic output *)
  let rec sort_trie = function
    | Node children ->
        Node
          (children
          |> List.map (fun (k, v) -> (k, sort_trie v))
          |> List.sort (fun (a, _) (b, _) -> String.compare a b))
    | Module nsid -> Module nsid
    | ModuleWithChildren (nsid, children) ->
        ModuleWithChildren
          ( nsid,
            children
            |> List.map (fun (k, v) -> (k, sort_trie v))
            |> List.sort (fun (a, _) (b, _) -> String.compare a b) )
  in
  match
    List.fold_left
      (fun trie nsid ->
        let segments = String.split_on_char '.' nsid in
        insert_segments trie nsid segments)
      (Node []) nsids
    |> sort_trie
  with
  | Node result -> result
  | ModuleWithChildren (_, result) -> result
  | Module _ -> failwith "unexpected single module at root"
