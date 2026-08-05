(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

open Lexicon_types

(** returns SCCs in reverse topological order (dependencies first) each SCC is a
    list of nodes *)
let find_sccs (type node) (nodes : node list) ~(get_id : node -> string)
    ~(get_deps : node -> string list) : node list list =
  (* build node map: id -> node *)
  let node_map =
    List.fold_left (fun m node -> (get_id node, node) :: m) [] nodes
  in
  let node_ids = List.map get_id nodes in
  (* build dependency map *)
  let deps = List.map (fun node -> (get_id node, get_deps node)) nodes in
  (* Tarjan's algorithm state *)
  let index_counter = ref 0 in
  let indices = Hashtbl.create 64 in
  let lowlinks = Hashtbl.create 64 in
  let on_stack = Hashtbl.create 64 in
  let stack = ref [] in
  let sccs = ref [] in
  let rec strongconnect id =
    let index = !index_counter in
    incr index_counter;
    Hashtbl.add indices id index;
    Hashtbl.add lowlinks id index;
    Hashtbl.add on_stack id true;
    stack := id :: !stack;
    (* visit successors *)
    let successors =
      List.assoc_opt id deps |> Option.value ~default:[]
      |> List.filter (fun s -> List.mem s node_ids)
    in
    List.iter
      (fun succ ->
        if not (Hashtbl.mem indices succ) then begin
          (* successor not yet visited *)
          strongconnect succ;
          Hashtbl.replace lowlinks id
            (min (Hashtbl.find lowlinks id) (Hashtbl.find lowlinks succ))
        end
        else if Hashtbl.find_opt on_stack succ = Some true then
          (* successor is on stack, part of current SCC *)
          Hashtbl.replace lowlinks id
            (min (Hashtbl.find lowlinks id) (Hashtbl.find indices succ)))
      successors;
    (* if this is a root node, pop the SCC *)
    if Hashtbl.find lowlinks id = Hashtbl.find indices id then begin
      let rec pop_scc acc =
        match !stack with
        | [] -> acc
        | top :: rest ->
            stack := rest;
            Hashtbl.replace on_stack top false;
            if top = id then top :: acc else pop_scc (top :: acc)
      in
      let scc_ids = pop_scc [] in
      (* convert IDs to nodes, preserving original order *)
      let scc_nodes =
        List.filter_map
          (fun n -> List.assoc_opt n node_map)
          (List.filter (fun n -> List.mem n scc_ids) node_ids)
      in
      if scc_nodes <> [] then sccs := scc_nodes :: !sccs
    end
  in
  (* run on all nodes *)
  List.iter
    (fun id -> if not (Hashtbl.mem indices id) then strongconnect id)
    node_ids;
  (* SCCs are prepended, so reverse to get topological order *)
  List.rev !sccs

(* Helper: fold over properties collecting local refs *)
let fold_properties nsid f acc props =
  List.fold_left
    (fun a (_, (prop : property)) -> f nsid a prop.type_def)
    acc props

(* Helper: fold over optional body schema *)
let fold_body_schema nsid f acc body =
  Option.fold ~none:acc
    ~some:(fun b -> Option.fold ~none:acc ~some:(f nsid acc) b.schema)
    body

(* Helper: fold over optional parameters *)
let fold_params nsid f acc params =
  Option.fold ~none:acc
    ~some:(fun p -> fold_properties nsid f acc p.properties)
    params

(** returns list of definition names that this type depends on within the same
    nsid *)
let rec collect_local_refs nsid acc = function
  | Array { items; _ } -> collect_local_refs nsid acc items
  | Ref { ref_; _ } ->
      if String.length ref_ > 0 && ref_.[0] = '#' then
        (* local ref: #foo *)
        let def_name = String.sub ref_ 1 (String.length ref_ - 1) in
        def_name :: acc
      else begin
        (* check if it's a self-reference: nsid#foo *)
        match String.split_on_char '#' ref_ with
        | [ ext_nsid; def_name ] when ext_nsid = nsid -> def_name :: acc
        | _ -> acc
      end
  | Union { refs; _ } ->
      List.fold_left
        (fun a r ->
          if String.length r > 0 && r.[0] = '#' then
            let def_name = String.sub r 1 (String.length r - 1) in
            def_name :: a
          else
            match String.split_on_char '#' r with
            | [ ext_nsid; def_name ] when ext_nsid = nsid -> def_name :: a
            | _ -> a)
        acc refs
  | Object { properties; _ } ->
      fold_properties nsid collect_local_refs acc properties
  | Record { record; _ } ->
      fold_properties nsid collect_local_refs acc record.properties
  | Query { parameters; output; _ } ->
      let acc = fold_params nsid collect_local_refs acc parameters in
      fold_body_schema nsid collect_local_refs acc output
  | Procedure { parameters; input; output; _ } ->
      let acc = fold_params nsid collect_local_refs acc parameters in
      let acc = fold_body_schema nsid collect_local_refs acc input in
      fold_body_schema nsid collect_local_refs acc output
  | _ -> acc

(** find SCCs among definitions within a single lexicon returns SCCs in reverse
    topological order *)
let find_def_sccs nsid (defs : def_entry list) : def_entry list list =
  find_sccs defs
    ~get_id:(fun def -> def.name)
    ~get_deps:(fun def -> collect_local_refs nsid [] def.type_def)

(* Helper: iterate over properties *)
let iter_properties f props =
  List.iter (fun (_, (prop : property)) -> f prop.type_def) props

(* Helper: iterate over optional body schema *)
let iter_body_schema f body = Option.iter (fun b -> Option.iter f b.schema) body

(* Helper: iterate over optional parameters *)
let iter_params f params =
  Option.iter (fun p -> iter_properties f p.properties) params

(** get external nsid dependencies for a lexicon *)
let get_external_nsids (doc : lexicon_doc) : string list =
  let nsids = ref [] in
  let add_nsid s = if not (List.mem s !nsids) then nsids := s :: !nsids in
  let rec collect_from_type = function
    | Array { items; _ } -> collect_from_type items
    | Ref { ref_; _ } ->
        if String.length ref_ > 0 && ref_.[0] <> '#' then begin
          match String.split_on_char '#' ref_ with
          | ext_nsid :: _ -> add_nsid ext_nsid
          | [] -> ()
        end
    | Union { refs; _ } ->
        List.iter
          (fun r ->
            if String.length r > 0 && r.[0] <> '#' then
              match String.split_on_char '#' r with
              | ext_nsid :: _ -> add_nsid ext_nsid
              | [] -> ())
          refs
    | Object { properties; _ } -> iter_properties collect_from_type properties
    | Query { parameters; output; _ } ->
        iter_params collect_from_type parameters;
        iter_body_schema collect_from_type output
    | Procedure { parameters; input; output; _ } ->
        iter_params collect_from_type parameters;
        iter_body_schema collect_from_type input;
        iter_body_schema collect_from_type output
    | Record { record; _ } ->
        iter_properties collect_from_type record.properties
    | _ -> ()
  in
  List.iter (fun def -> collect_from_type def.type_def) doc.defs;
  !nsids

(** find SCCs between lexicon files, in reverse topological order *)
let find_file_sccs (lexicons : lexicon_doc list) : lexicon_doc list list =
  let nsids = List.map (fun doc -> doc.id) lexicons in
  find_sccs lexicons
    ~get_id:(fun doc -> doc.id)
    ~get_deps:(fun doc ->
      (* filter to only include nsids we have *)
      get_external_nsids doc |> List.filter (fun n -> List.mem n nsids))
