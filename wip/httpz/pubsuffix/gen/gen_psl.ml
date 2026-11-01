type section =
  | ICANN
  | Private

type rule_type =
  | Normal
  | Wildcard
  | Exception

type rule =
  { labels : string list
  ; rule_type : rule_type
  ; section : section
  }

(* A wildcard rule is recorded in [wildcard], so only these two kinds ever
   reach [rule]. *)
type node_rule =
  | Rule_normal
  | Rule_exception

type trie_node =
  { mutable rule : (node_rule * section) option
  ; mutable children : (string * trie_node) list
  ; mutable wildcard : section option
  }

let make_node () = { rule = None; children = []; wildcard = None }

(* A comment runs from [//] to the end of the line and may be indented, so it
   is recognised after the leading whitespace is gone. What follows a rule is
   separated from it by whitespace and is dropped with the rest of the line. *)
let parse_line section line =
  let line = String.trim line in
  if String.starts_with ~prefix:"//" line
  then None
  else (
    let line =
      match String.index_opt line ' ', String.index_opt line '\t' with
      | Some i, Some j -> String.sub line 0 (min i j)
      | Some i, None | None, Some i -> String.sub line 0 i
      | None, None -> line
    in
    if line = ""
    then None
    else (
      let rule_type, domain =
        if String.length line > 0 && line.[0] = '!'
        then Exception, String.sub line 1 (String.length line - 1)
        else if String.length line > 2 && line.[0] = '*' && line.[1] = '.'
        then Wildcard, String.sub line 2 (String.length line - 2)
        else Normal, line
      in
      let labels =
        String.split_on_char '.' domain
        |> List.rev
        |> List.map (fun label ->
          if label = ""
          then failwith (Printf.sprintf "public-suffix rule %S has an empty label" domain);
          try String.lowercase_ascii (Punycode.encode_label label) with
          | Punycode.Error reason ->
            failwith
              (Format.asprintf
                 "public-suffix rule %S has an invalid label %S: %a"
                 domain label Punycode.pp_error_reason reason))
      in
      Some { labels; rule_type; section }))
;;

let insert_rule trie rule =
  let rec insert node labels =
    match labels with
    | [] ->
      (match rule.rule_type with
       | Wildcard -> node.wildcard <- Some rule.section
       | Normal -> node.rule <- Some (Rule_normal, rule.section)
       | Exception -> node.rule <- Some (Rule_exception, rule.section))
    | label :: rest ->
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
;;

let parse_file filename =
  In_channel.with_open_text filename
  @@ fun ic ->
  let trie = make_node () in
  let current_section = ref ICANN in
  let rule_count = ref 0 in
  let icann_count = ref 0 in
  let private_count = ref 0 in
  let version = ref None in
  let commit = ref None in
  let extract_value line prefix =
    let prefix_len = String.length prefix in
    if String.length line > prefix_len && String.sub line 0 prefix_len = prefix
    then Some (String.trim (String.sub line prefix_len (String.length line - prefix_len)))
    else None
  in
  let rec loop () =
    match In_channel.input_line ic with
    | None -> ()
    | Some line ->
      if !version = None then version := extract_value line "// VERSION: ";
      if !commit = None then commit := extract_value line "// COMMIT: ";
      if String.starts_with ~prefix:"// ===BEGIN ICANN DOMAINS===" line
      then current_section := ICANN
      else if String.starts_with ~prefix:"// ===BEGIN PRIVATE DOMAINS===" line
      then current_section := Private
      else
        Option.iter
          (fun rule ->
            insert_rule trie rule;
            incr rule_count;
            if rule.section = ICANN then incr icann_count else incr private_count)
          (parse_line !current_section line);
      loop ()
  in
  loop ();
  trie, !rule_count, !icann_count, !private_count, !version, !commit
;;

let generate_code trie rule_count icann_count private_count version commit =
  print_string
    {|(* Generated from public_suffix_list.dat. Do not edit. *)

type section = ICANN | Private

type rule_type = Normal | Exception

type trie_node = {
  rule : (rule_type * section) option;
  children : (string * trie_node) iarray;
  wildcard : section option;
}

|};
  let counter = ref 0 in
  let output_buffer = Buffer.create (1024 * 1024) in
  (* Emit each node after its children so that every reference in the generated module
     names an already-bound value. Children are emitted in label order so that a lookup
     can binary-search them; the root alone has about 1450. *)
  let rec generate_node node =
    let children =
      List.map (fun (label, child) -> label, generate_node child)
        (List.sort (fun (a, _) (b, _) -> String.compare a b) node.children)
    in
    let name = Printf.sprintf "n%d" !counter in
    incr counter;
    Buffer.add_string output_buffer (Printf.sprintf "let %s = {\n" name);
    (match node.rule with
     | None -> Buffer.add_string output_buffer "  rule = None;\n"
     | Some (rt, sec) ->
       let rt_str =
         match rt with
         | Rule_normal -> "Normal"
         | Rule_exception -> "Exception"
       in
       let sec_str =
         match sec with
         | ICANN -> "ICANN"
         | Private -> "Private"
       in
       Buffer.add_string
         output_buffer
         (Printf.sprintf "  rule = Some (%s, %s);\n" rt_str sec_str));
    if children = []
    then Buffer.add_string output_buffer "  children = [: :];\n"
    else (
      Buffer.add_string output_buffer "  children = [:\n";
      List.iter
        (fun (label, child_name) ->
          Buffer.add_string
            output_buffer
            (Printf.sprintf "    (%S, %s);\n" label child_name))
        children;
      Buffer.add_string output_buffer "  :];\n");
    (match node.wildcard with
     | None -> Buffer.add_string output_buffer "  wildcard = None;\n"
     | Some sec ->
       let sec_str =
         match sec with
         | ICANN -> "ICANN"
         | Private -> "Private"
       in
       Buffer.add_string output_buffer (Printf.sprintf "  wildcard = Some %s;\n" sec_str));
    Buffer.add_string output_buffer "}\n\n";
    name
  in
  let root_name = generate_node trie in
  print_string (Buffer.contents output_buffer);
  Printf.printf "let root = %s\n\n" root_name;
  Printf.printf "let rule_count = %d\n\n" rule_count;
  Printf.printf "let icann_rule_count = %d\n\n" icann_count;
  Printf.printf "let private_rule_count = %d\n\n" private_count;
  Printf.printf "let version = %S\n\n" version;
  Printf.printf "let commit = %S\n" commit
;;

let () =
  let allow_partial, filename =
    match Array.to_list Sys.argv with
    | [ _; filename ] -> (false, filename)
    | [ _; "--allow-partial"; filename ] -> (true, filename)
    | _ ->
      Printf.eprintf "Usage: %s [--allow-partial] <public_suffix_list.dat>\n"
        Sys.argv.(0);
      exit 1
  in
  let trie, rule_count, icann_count, private_count, version, commit =
    parse_file filename
  in
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
  (* VERSION and COMMIT sit at the top of the file, so a truncated download
     still passes the checks above while missing rules. Missing Private rules
     then fail open for cross-tenant cookies via the implicit wildcard. The
     2026-09 list has ~6950 ICANN and ~3300 Private rules; refuse a file that
     falls well short of both sections. *)
  if (not allow_partial) && (icann_count < 5000 || private_count < 2000)
  then (
    Printf.eprintf
      "ERROR: %s looks incomplete (%d ICANN, %d Private rules; expected at \
       least 5000 and 2000). Pass --allow-partial only for test fixtures.\n"
      filename icann_count private_count;
    exit 1);
  generate_code trie rule_count icann_count private_count version commit
;;
