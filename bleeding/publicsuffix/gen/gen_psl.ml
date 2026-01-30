(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(* gen_psl.ml - Generate OCaml code from public_suffix_list.dat

   This parser reads the Public Suffix List and generates OCaml source code
   containing a pre-parsed trie data structure for efficient lookups.

   PSL Format (from https://publicsuffix.org/list/):
   - One rule per line
   - Lines read only up to first whitespace
   - Comments start with double slash
   - Wildcard (asterisk) only in leftmost position
   - Exception rules prefixed with !
   - File uses UTF-8 encoding with Unicode (not Punycode)

   The generated code contains the trie as OCaml values that can be
   embedded directly into the library.
*)

(** Section markers in the PSL file *)
type section = ICANN | Private

(** Rule types *)
type rule_type = Normal | Wildcard | Exception

type rule = {
  labels : string list; (* Labels in reverse order: ["uk"; "co"] for co.uk *)
  rule_type : rule_type;
  section : section;
}
(** A parsed rule *)

type trie_node = {
  id : int;  (* Unique identifier for this node *)
  mutable rule : (rule_type * section) option;
  mutable children : (string * trie_node) list;
  mutable wildcard_child : trie_node option;
}
(** Trie node for efficient lookup *)

let node_id_counter = ref 0

let make_node () =
  let id = !node_id_counter in
  incr node_id_counter;
  { id; rule = None; children = []; wildcard_child = None }

(** Parse a single line from the PSL file *)
let parse_line section line =
  (* Strip comments (looking for //) *)
  let line =
    match String.index_opt line '/' with
    | Some i when i > 0 && line.[i - 1] = '/' -> String.sub line 0 (i - 1)
    | Some 0 -> ""
    | _ -> line
  in
  (* Take only up to first whitespace and trim *)
  let line =
    String.trim line |> fun s ->
    match (String.index_from_opt s 0 ' ', String.index_from_opt s 0 '\t') with
    | Some i, Some j -> String.sub s 0 (min i j)
    | Some i, None | None, Some i -> String.sub s 0 i
    | None, None -> s
  in
  let line = String.trim line in
  if line = "" then None
  else
    (* Determine rule type *)
    let rule_type, domain =
      if String.length line > 0 && line.[0] = '!' then
        (Exception, String.sub line 1 (String.length line - 1))
      else if String.length line > 2 && line.[0] = '*' && line.[1] = '.' then
        (Wildcard, String.sub line 2 (String.length line - 2))
      else (Normal, line)
    in
    (* Process labels: split, reverse, filter, and encode *)
    let labels =
      String.split_on_char '.' domain
      |> List.rev
      |> List.filter (fun s -> s <> "")
      |> List.map (fun label ->
          match Punycode.encode_label label with
          | Ok encoded -> String.lowercase_ascii encoded
          | Error _ -> String.lowercase_ascii label)
    in
    if labels = [] then None else Some { labels; rule_type; section }

(** Insert a rule into the trie *)
let insert_rule trie rule =
  let rec insert node labels =
    match labels with
    | [] ->
        (* We've reached the endpoint for this rule *)
        if rule.rule_type = Wildcard then begin
          (* For wildcard rules, store in wildcard_child *)
          let child =
            match node.wildcard_child with
            | Some c -> c
            | None ->
                let c = make_node () in
                node.wildcard_child <- Some c;
                c
          in
          child.rule <- Some (Wildcard, rule.section)
        end
        else node.rule <- Some (rule.rule_type, rule.section)
    | label :: rest ->
        (* Find or create child for this label *)
        let child =
          match List.assoc_opt label node.children with
          | Some c -> c
          | None ->
              let c = make_node () in
              node.children <- (label, c) :: node.children;
              c
        in
        insert child rest
  in
  insert trie rule.labels

(** Parse the entire PSL file *)
let parse_file filename =
  let ic = open_in filename in
  let trie = make_node () in
  let current_section = ref ICANN in
  let rule_count = ref 0 in
  let icann_count = ref 0 in
  let private_count = ref 0 in
  let version = ref None in
  let commit = ref None in
  (* Helper to check if string contains substring *)
  let contains_substring s sub =
    try
      let _ = Str.search_forward (Str.regexp_string sub) s 0 in
      true
    with Not_found -> false
  in
  (* Helper to extract value after "KEY: " pattern *)
  let extract_value line prefix =
    let prefix_len = String.length prefix in
    if String.length line > prefix_len && String.sub line 0 prefix_len = prefix
    then
      Some
        (String.trim
           (String.sub line prefix_len (String.length line - prefix_len)))
    else None
  in
  try
    while true do
      let line = input_line ic in
      (* Check for version and commit info *)
      if !version = None then version := extract_value line "// VERSION: ";
      if !commit = None then commit := extract_value line "// COMMIT: ";
      (* Check for section markers *)
      if contains_substring line "===BEGIN ICANN DOMAINS===" then
        current_section := ICANN
      else if contains_substring line "===BEGIN PRIVATE DOMAINS===" then
        current_section := Private
      else
        Option.iter
          (fun rule ->
            insert_rule trie rule;
            incr rule_count;
            if rule.section = ICANN then incr icann_count
            else incr private_count)
          (parse_line !current_section line)
    done;
    (trie, !rule_count, !icann_count, !private_count, !version, !commit)
  with End_of_file ->
    close_in ic;
    (trie, !rule_count, !icann_count, !private_count, !version, !commit)

(** Escape a string for OCaml source code *)
let escape_string s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | '\r' -> Buffer.add_string b "\\r"
      | '\t' -> Buffer.add_string b "\\t"
      | c when Char.code c < 32 || Char.code c > 126 ->
          (* For non-ASCII, we keep the UTF-8 bytes as-is since OCaml handles UTF-8 strings *)
          Buffer.add_char b c
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

(** Generate OCaml code for the trie *)
let generate_code trie rule_count icann_count private_count version commit =
  (* Print header *)
  print_string
    {|(* Auto-generated from public_suffix_list.dat - DO NOT EDIT *)
(* This file contains the parsed Public Suffix List as OCaml data structures *)

(** Section of the PSL where a rule originates *)
type section = ICANN | Private

(** Rule types in the PSL *)
type rule_type = Normal | Wildcard | Exception

(** A node in the suffix trie *)
type trie_node = {
  rule : (rule_type * section) option;
  children : (string * trie_node) list;
  wildcard_child : trie_node option;
}

|};
  Printf.printf "(* Statistics: %d total rules (%d ICANN, %d private) *)\n"
    rule_count icann_count private_count;
  Printf.printf "(* Version: %s *)\n" version;
  Printf.printf "(* Commit: %s *)\n" commit;
  print_string "\n";

  (* Generate the trie as nested let bindings using a depth-first traversal *)
  let node_counter = ref 0 in
  let node_names = Hashtbl.create 1000 in

  (* First pass: assign names to all nodes *)
  let rec assign_names node =
    let name = Printf.sprintf "n%d" !node_counter in
    incr node_counter;
    Hashtbl.add node_names node.id name;
    List.iter (fun (_, child) -> assign_names child) node.children;
    Option.iter assign_names node.wildcard_child
  in
  assign_names trie;

  (* Generate nodes in reverse order (leaves first) *)
  let generated = Hashtbl.create 1000 in
  let output_buffer = Buffer.create (1024 * 1024) in

  let rec generate_node node =
    let node_id = node.id in
    if Hashtbl.mem generated node_id then Hashtbl.find node_names node_id
    else begin
      (* First generate all children *)
      List.iter (fun (_, child) -> ignore (generate_node child)) node.children;
      Option.iter
        (fun child -> ignore (generate_node child))
        node.wildcard_child;

      let name = Hashtbl.find node_names node_id in

      (* Generate the node definition *)
      Buffer.add_string output_buffer (Printf.sprintf "let %s = {\n" name);

      (* Rule field *)
      (match node.rule with
      | None -> Buffer.add_string output_buffer "  rule = None;\n"
      | Some (rt, sec) ->
          let rt_str =
            match rt with
            | Normal -> "Normal"
            | Wildcard -> "Wildcard"
            | Exception -> "Exception"
          in
          let sec_str =
            match sec with ICANN -> "ICANN" | Private -> "Private"
          in
          Buffer.add_string output_buffer
            (Printf.sprintf "  rule = Some (%s, %s);\n" rt_str sec_str));

      (* Children field *)
      if node.children = [] then
        Buffer.add_string output_buffer "  children = [];\n"
      else begin
        Buffer.add_string output_buffer "  children = [\n";
        List.iter
          (fun (label, child) ->
            let child_name = Hashtbl.find node_names child.id in
            Buffer.add_string output_buffer
              (Printf.sprintf "    (\"%s\", %s);\n" (escape_string label)
                 child_name))
          node.children;
        Buffer.add_string output_buffer "  ];\n"
      end;

      (* Wildcard child field *)
      (match node.wildcard_child with
      | None -> Buffer.add_string output_buffer "  wildcard_child = None;\n"
      | Some child ->
          let child_name = Hashtbl.find node_names child.id in
          Buffer.add_string output_buffer
            (Printf.sprintf "  wildcard_child = Some %s;\n" child_name));

      Buffer.add_string output_buffer "}\n\n";

      Hashtbl.add generated node_id true;
      name
    end
  in

  let root_name = generate_node trie in
  print_string (Buffer.contents output_buffer);
  Printf.printf "let root = %s\n" root_name;

  (* Generate helper to get the root *)
  print_string
    {|
(** Get the root of the suffix trie *)
let get_root () = root

(** Total number of rules in the list *)
|};
  Printf.printf "let rule_count = %d\n\n" rule_count;
  Printf.printf "let icann_rule_count = %d\n\n" icann_count;
  Printf.printf "let private_rule_count = %d\n\n" private_count;
  (* Generate version and commit values *)
  Printf.printf "let version = %S\n\n" version;
  Printf.printf "let commit = %S\n" commit

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <public_suffix_list.dat>\n" Sys.argv.(0);
    exit 1
  end;
  let filename = Sys.argv.(1) in
  let trie, rule_count, icann_count, private_count, version, commit =
    parse_file filename
  in
  (* Ensure version and commit are present *)
  let version =
    match version with
    | Some v -> v
    | None ->
        Printf.eprintf "ERROR: VERSION not found in %s\n" filename;
        exit 1
  in
  let commit =
    match commit with
    | Some c -> c
    | None ->
        Printf.eprintf "ERROR: COMMIT not found in %s\n" filename;
        exit 1
  in
  generate_code trie rule_count icann_count private_count version commit
