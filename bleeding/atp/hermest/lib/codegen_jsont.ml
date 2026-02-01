(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/.

   Portions copyright (c) 2025 Anil Madhavapeddy. *)

(** Jsont codec generation from AT Protocol lexicons.

    This module generates jsont declarative codecs. The
    output follows the pattern:

    {[
      type foo = { bar : string; baz : int option }

      let foo_jsont =
        Jsont.Object.map ~kind:"Foo" (fun bar baz -> { bar; baz })
        |> Jsont.Object.mem "bar" Jsont.string ~enc:(fun r -> r.bar)
        |> Jsont.Object.opt_mem "baz" Jsont.int ~enc:(fun r -> r.baz)
        |> Jsont.Object.finish
    ]} *)

open Lexicon_types

(* Internal helpers - re-export Emitter functions for convenience *)
let add_import = Emitter.add_import
let mark_union_generated = Emitter.mark_union_generated
let is_union_generated = Emitter.is_union_generated
let lookup_union_name = Emitter.lookup_union_name
let is_cyclic_nsid = Emitter.is_cyclic_nsid
let emitln = Emitter.emitln
let emit_newline = Emitter.emit_newline

(** Generate jsont type reference for a primitive/composite type *)
let rec gen_jsont_ref nsid out (type_def : type_def) : string =
  match type_def with
  | String _ -> "Jsont.string"
  | Integer { maximum; _ } -> (
      match maximum with
      | Some m when m > 1073741823 -> "Jsont.int64"
      | _ -> "Jsont.int")
  | Boolean _ -> "Jsont.bool"
  | Bytes _ -> "Jsont.binary_string"
  | Blob _ -> "Atp.Blob_ref.jsont"
  | CidLink _ -> "Atp.Cid.jsont"
  | Array { items; _ } ->
      let item_jsont = gen_jsont_ref nsid out items in
      Printf.sprintf "(Jsont.list %s)" item_jsont
  | Object _ -> "Jsont.json" (* inline objects handled separately *)
  | Ref { ref_; _ } -> gen_ref_jsont nsid out ref_
  | Union { refs; closed; _ } -> (
      match refs with
      | [] ->
          (* Empty open union - use Jsont.json *)
          "Jsont.json"
      | [ single_ref ] ->
          (* Single-ref union - treat as the ref directly *)
          gen_ref_jsont nsid out single_ref
      | _ when Option.value closed ~default:true = false -> (
          (* Open union with multiple refs - need union type *)
          match lookup_union_name out refs with
          | Some name -> name ^ "_jsont"
          | None -> gen_union_jsont_name refs)
      | _ -> (
          (* Closed union with multiple refs *)
          match lookup_union_name out refs with
          | Some name -> name ^ "_jsont"
          | None -> gen_union_jsont_name refs))
  | Token _ -> "Jsont.string"
  | Unknown _ -> "Jsont.json"
  | Query _ | Procedure _ | Subscription _ | Record _ | PermissionSet _ ->
      "Jsont.ignore"

(** Generate reference to another type's jsont codec *)
and gen_ref_jsont nsid out ref_str : string =
  if String.length ref_str > 0 && ref_str.[0] = '#' then begin
    (* local ref: #someDef -> someDef_jsont *)
    (* If this file is cyclic, use Jsont.json to avoid internal cycles *)
    if is_cyclic_nsid out nsid then "Jsont.json"
    else begin
      let def_name = String.sub ref_str 1 (String.length ref_str - 1) in
      Naming.type_name def_name ^ "_jsont"
    end
  end
  else begin
    (* external ref: com.example.defs#someDef *)
    match String.split_on_char '#' ref_str with
    | [ ext_nsid; def_name ] ->
        if ext_nsid = nsid then begin
          (* Self-qualified local ref *)
          if is_cyclic_nsid out nsid then "Jsont.json"
          else Naming.type_name def_name ^ "_jsont"
        end
        else if is_cyclic_nsid out ext_nsid then
          (* Cyclic reference - use Jsont.json to break the cycle *)
          "Jsont.json"
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ "." ^ Naming.type_name def_name ^ "_jsont"
        end
    | [ ext_nsid ] ->
        if ext_nsid = nsid then begin
          if is_cyclic_nsid out nsid then "Jsont.json" else "main_jsont"
        end
        else if is_cyclic_nsid out ext_nsid then
          (* Cyclic reference - use Jsont.json to break the cycle *)
          "Jsont.json"
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ ".main_jsont"
        end
    | _ -> "Jsont.json (* invalid_ref *)"
  end

and gen_union_jsont_name refs = Naming.union_type_name refs ^ "_jsont"

(** Generate OCaml type reference (for type definitions) *)
let rec gen_type_ref nsid out (type_def : type_def) : string =
  match type_def with
  | String _ -> "string"
  | Integer { maximum; _ } -> (
      match maximum with Some m when m > 1073741823 -> "int64" | _ -> "int")
  | Boolean _ -> "bool"
  | Bytes _ -> "string"
  | Blob _ -> "Atp.Blob_ref.t"
  | CidLink _ -> "Atp.Cid.t"
  | Array { items; _ } ->
      let item_type = gen_type_ref nsid out items in
      item_type ^ " list"
  | Object _ -> "Jsont.json"
  | Ref { ref_; _ } -> gen_ref_type nsid out ref_
  | Union { refs; closed; _ } -> (
      match refs with
      | [] ->
          (* Empty open union - use Jsont.json *)
          "Jsont.json"
      | [ single_ref ] ->
          (* Single-ref union - treat as the ref directly *)
          gen_ref_type nsid out single_ref
      | _ when Option.value closed ~default:true = false -> (
          (* Open union with multiple refs - need union type *)
          match lookup_union_name out refs with
          | Some name -> name
          | None -> Naming.union_type_name refs)
      | _ -> (
          (* Closed union with multiple refs *)
          match lookup_union_name out refs with
          | Some name -> name
          | None -> Naming.union_type_name refs))
  | Token _ -> "string"
  | Unknown _ -> "Jsont.json"
  | Query _ | Procedure _ | Subscription _ | Record _ | PermissionSet _ ->
      "unit"

and gen_ref_type nsid out ref_str : string =
  if String.length ref_str > 0 && ref_str.[0] = '#' then begin
    (* If this file is cyclic, use Jsont.json to avoid internal cycles *)
    if is_cyclic_nsid out nsid then "Jsont.json"
    else begin
      let def_name = String.sub ref_str 1 (String.length ref_str - 1) in
      Naming.type_name def_name
    end
  end
  else begin
    match String.split_on_char '#' ref_str with
    | [ ext_nsid; def_name ] ->
        if ext_nsid = nsid then begin
          if is_cyclic_nsid out nsid then "Jsont.json"
          else Naming.type_name def_name
        end
        else if is_cyclic_nsid out ext_nsid then
          (* Cyclic reference - use Jsont.json to break the cycle *)
          "Jsont.json"
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ "." ^ Naming.type_name def_name
        end
    | [ ext_nsid ] ->
        if ext_nsid = nsid then begin
          if is_cyclic_nsid out nsid then "Jsont.json"
          else Naming.type_name "main"
        end
        else if is_cyclic_nsid out ext_nsid then
          (* Cyclic reference - use Jsont.json to break the cycle *)
          "Jsont.json"
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ ".main"
        end
    | _ -> "invalid_ref"
  end

(** Collect inline union types from a type definition that need to be generated
*)
let rec collect_inline_unions acc (type_def : type_def) : union_spec list =
  match type_def with
  | Union spec when List.length spec.refs > 1 -> spec :: acc
  | Array { items; _ } -> collect_inline_unions acc items
  | Object { properties; _ } ->
      List.fold_left
        (fun acc (_, (prop : property)) ->
          collect_inline_unions acc prop.type_def)
        acc properties
  | _ -> acc

(** Generate inline union type definition and jsont codec. For now, we use
    Jsont.json as a fallback since proper discriminated union support with
    Jsont.Object.Case requires more work. *)
let gen_inline_union_type_and_jsont _nsid out (spec : union_spec) =
  let type_name = Naming.union_type_name spec.refs in
  (* Check if already generated using the type name *)
  if is_union_generated out type_name then ()
  else begin
    mark_union_generated out type_name;
    (* For now, use Jsont.json as a simple fallback for union types.
       Proper discriminated union support would require using Jsont.Object.Case. *)
    emitln out (Printf.sprintf "(* Union type: %s *)" type_name);
    emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
    emit_newline out;
    emitln out (Printf.sprintf "let %s_jsont = Jsont.json" type_name);
    emit_newline out
  end

(** Generate all inline unions from an object spec *)
let gen_inline_unions_for_object nsid out (spec : object_spec) =
  let unions =
    List.fold_left
      (fun acc (_, (prop : property)) ->
        collect_inline_unions acc prop.type_def)
      [] spec.properties
  in
  List.iter (gen_inline_union_type_and_jsont nsid out) unions

(** Generate object type definition and jsont codec with $type discriminator. *)
let gen_object_type_and_jsont ?(first = true) nsid out name (spec : object_spec)
    =
  (* First, generate any inline union types used in properties *)
  gen_inline_unions_for_object nsid out spec;

  let required = Option.value spec.required ~default:[] in
  let nullable = Option.value spec.nullable ~default:[] in
  let type_name = Naming.type_name name in
  let keyword = if first then "type" else "and" in
  (* For "main" definitions, use just the NSID; for others, use nsid#name *)
  let type_id = if name = "main" then nsid else nsid ^ "#" ^ name in

  (* Handle empty objects *)
  if spec.properties = [] then begin
    emitln out (Printf.sprintf "%s %s = unit" keyword type_name);
    emit_newline out;
    emitln out (Printf.sprintf "let %s_jsont = Jsont.ignore" type_name);
    emit_newline out
  end
  else begin
    (* Generate type definition *)
    emitln out (Printf.sprintf "%s %s = {" keyword type_name);
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_type_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let type_str =
          if is_required && not is_nullable then base_type
          else base_type ^ " option"
        in
        emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;

    (* Generate jsont codec with $type discriminator *)
    let kind_name = String.capitalize_ascii type_name in
    let param_names =
      List.map
        (fun (prop_name, _) -> Naming.field_name prop_name)
        spec.properties
    in
    let params_str = String.concat " " param_names in
    let record_str = String.concat "; " param_names in

    emitln out (Printf.sprintf "let %s_jsont =" type_name);
    emitln out (Printf.sprintf "  Jsont.Object.map ~kind:\"%s\"" kind_name);
    emitln out
      (Printf.sprintf "    (fun _typ %s -> { %s })" params_str record_str);
    emitln out
      (Printf.sprintf
         "  |> Jsont.Object.mem \"$type\" Jsont.string ~dec_absent:\"%s\" \
          ~enc:(fun _ -> \"%s\")"
         type_id type_id);

    (* Member declarations *)
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let jsont_ref = gen_jsont_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let is_optional = (not is_required) || is_nullable in

        if is_optional then
          emitln out
            (Printf.sprintf
               "  |> Jsont.Object.opt_mem \"%s\" %s ~enc:(fun r -> r.%s)"
               prop_name jsont_ref ocaml_name)
        else
          emitln out
            (Printf.sprintf
               "  |> Jsont.Object.mem \"%s\" %s ~enc:(fun r -> r.%s)" prop_name
               jsont_ref ocaml_name))
      spec.properties;

    emitln out "  |> Jsont.Object.finish";
    emit_newline out
  end

(** Generate union type definition and jsont codec. For now, we use Jsont.json
    as a fallback since proper discriminated union support with
    Jsont.Object.Case requires more work. *)
let gen_union_type_and_jsont _nsid out name (spec : union_spec) =
  let type_name = Naming.type_name name in
  let _ = spec in
  (* suppress unused warning *)
  (* For now, use Jsont.json as a simple fallback for named union types *)
  emitln out (Printf.sprintf "(* Union type: %s *)" type_name);
  emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
  emit_newline out;
  emitln out (Printf.sprintf "let %s_jsont = Jsont.json" type_name);
  emit_newline out

(** Generate inline unions for params spec *)
let gen_inline_unions_for_params nsid out (spec : params_spec) =
  let unions =
    List.fold_left
      (fun acc (_, (prop : property)) ->
        collect_inline_unions acc prop.type_def)
      [] spec.properties
  in
  List.iter (gen_inline_union_type_and_jsont nsid out) unions

(** Generate params type and jsont codec for query/procedure *)
let gen_params_type_and_jsont nsid out (spec : params_spec) =
  (* First, generate any inline union types used in properties *)
  gen_inline_unions_for_params nsid out spec;

  let required = Option.value spec.required ~default:[] in

  (* Handle empty params *)
  if spec.properties = [] then begin
    emitln out "type params = unit";
    emit_newline out;
    emitln out "let params_jsont = Jsont.ignore";
    emit_newline out
  end
  else begin
    (* Generate type definition *)
    emitln out "type params = {";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_type_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        let type_str =
          if is_required then base_type else base_type ^ " option"
        in
        emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;

    (* Generate jsont codec *)
    emitln out "let params_jsont =";
    let param_names =
      List.map
        (fun (prop_name, _) -> Naming.field_name prop_name)
        spec.properties
    in
    let params_str = String.concat " " param_names in
    emitln out "  Jsont.Object.map ~kind:\"Params\"";
    emitln out (Printf.sprintf "    (fun %s -> {" params_str);
    List.iter
      (fun name -> emitln out (Printf.sprintf "      %s;" name))
      param_names;
    emitln out "    })";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let jsont_ref = gen_jsont_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        if is_required then begin
          emitln out
            (Printf.sprintf "  |> Jsont.Object.mem \"%s\" %s" prop_name
               jsont_ref);
          emitln out (Printf.sprintf "       ~enc:(fun r -> r.%s)" ocaml_name)
        end
        else begin
          emitln out
            (Printf.sprintf "  |> Jsont.Object.opt_mem \"%s\" %s" prop_name
               jsont_ref);
          emitln out (Printf.sprintf "       ~enc:(fun r -> r.%s)" ocaml_name)
        end)
      spec.properties;
    emitln out "  |> Jsont.Object.finish";
    emit_newline out
  end

(** Generate output/input body type and jsont codec *)
let gen_body_type_and_jsont nsid out (body : body_def) suffix =
  match body.schema with
  | Some (Object spec) -> gen_object_type_and_jsont nsid out suffix spec
  | Some type_def ->
      (* Non-object schema - generate type alias *)
      let type_name = Naming.type_name suffix in
      let base_type = gen_type_ref nsid out type_def in
      emitln out (Printf.sprintf "type %s = %s" type_name base_type);
      emit_newline out;
      let jsont_ref = gen_jsont_ref nsid out type_def in
      emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
      emit_newline out
  | None ->
      (* No schema - use unit *)
      let type_name = Naming.type_name suffix in
      emitln out (Printf.sprintf "type %s = unit" type_name);
      emitln out (Printf.sprintf "let %s_jsont = Jsont.ignore" type_name);
      emit_newline out

(** Collect all local references from a type definition. nsid is the current
    document's NSID, used to detect fully-qualified local refs. *)
let rec collect_local_refs nsid acc (type_def : type_def) =
  match type_def with
  | Ref { ref_; _ } -> (
      (* Handle both #localRef and nsid#localRef forms *)
      let name =
        if String.length ref_ > 0 && ref_.[0] = '#' then
          Some (String.sub ref_ 1 (String.length ref_ - 1))
        else
          (* Check if it's a fully-qualified local ref *)
          match String.split_on_char '#' ref_ with
          | [ ext_nsid; def_name ] when ext_nsid = nsid -> Some def_name
          | _ -> None
      in
      match name with Some n -> n :: acc | None -> acc)
  | Array { items; _ } -> collect_local_refs nsid acc items
  | Object { properties; _ } ->
      List.fold_left
        (fun acc (_, (prop : property)) ->
          collect_local_refs nsid acc prop.type_def)
        acc properties
  | Record { record; _ } ->
      List.fold_left
        (fun acc (_, (prop : property)) ->
          collect_local_refs nsid acc prop.type_def)
        acc record.properties
  | Query { parameters; output; _ } -> (
      let acc =
        match parameters with
        | Some p ->
            List.fold_left
              (fun acc (_, (prop : property)) ->
                collect_local_refs nsid acc prop.type_def)
              acc p.properties
        | None -> acc
      in
      match output with
      | Some { schema = Some s; _ } -> collect_local_refs nsid acc s
      | _ -> acc)
  | Procedure { parameters; input; output; _ } -> (
      let acc =
        match parameters with
        | Some p ->
            List.fold_left
              (fun acc (_, (prop : property)) ->
                collect_local_refs nsid acc prop.type_def)
              acc p.properties
        | None -> acc
      in
      let acc =
        match input with
        | Some { schema = Some s; _ } -> collect_local_refs nsid acc s
        | _ -> acc
      in
      match output with
      | Some { schema = Some s; _ } -> collect_local_refs nsid acc s
      | _ -> acc)
  | _ -> acc

(** Check if a definition is a "primary" type (Query/Procedure/Record) that may
    generate inline types *)
let is_primary_def (entry : def_entry) =
  match entry.type_def with
  | Query _ | Procedure _ | Subscription _ | Record _ | PermissionSet _ -> true
  | _ -> false

(** Topologically sort definitions based on local references *)
let topo_sort_defs nsid (defs : def_entry list) =
  (* Separate primary definitions (Query/Procedure/Record) from supporting types *)
  (* Supporting types (Object, Union, etc.) should come first *)
  let primary_defs, support_defs = List.partition is_primary_def defs in

  (* Build dependency graph for support defs *)
  let deps =
    List.map
      (fun (entry : def_entry) ->
        (entry.name, collect_local_refs nsid [] entry.type_def))
      support_defs
  in

  (* Simple topological sort using Kahn's algorithm *)
  let rec sort sorted remaining =
    if remaining = [] then sorted (* No reversal - deps already come first *)
    else
      (* Find a definition with no unresolved dependencies *)
      let sorted_names = List.map (fun (e : def_entry) -> e.name) sorted in
      let ready, not_ready =
        List.partition
          (fun (entry : def_entry) ->
            let entry_deps =
              List.assoc_opt entry.name deps |> Option.value ~default:[]
            in
            List.for_all
              (fun dep ->
                List.mem dep sorted_names
                || not
                     (List.exists
                        (fun (e : def_entry) -> e.name = dep)
                        remaining))
              entry_deps)
          remaining
      in
      match ready with
      | [] ->
          (* Cycle detected or external deps - sort remaining alphabetically *)
          let remaining_sorted =
            List.sort
              (fun (a : def_entry) (b : def_entry) ->
                String.compare a.name b.name)
              remaining
          in
          sorted @ remaining_sorted
      | _ ->
          (* Sort ready elements alphabetically for deterministic output *)
          let ready_sorted =
            List.sort
              (fun (a : def_entry) (b : def_entry) ->
                String.compare a.name b.name)
              ready
          in
          sort (sorted @ ready_sorted) not_ready
  in
  (* Sort support defs first, then append primary defs *)
  sort [] support_defs @ primary_defs

(** Generate a complete lexicon module. cyclic_nsids is a list of NSIDs that
    form dependency cycles - references to these NSIDs will use Jsont.json to
    break the cycle. *)
let gen_lexicon_module ?(cyclic_nsids = []) (doc : lexicon_doc) =
  let out = Emitter.make ~cyclic_nsids () in
  let nsid = doc.id in

  (* File header *)
  emitln out (Printf.sprintf "(* Generated from %s *)" nsid);
  emitln out "";

  (* Topologically sort definitions so dependencies come first *)
  let sorted_defs = topo_sort_defs nsid doc.defs in

  (* Process each definition in sorted order *)
  List.iter
    (fun (entry : def_entry) ->
      match entry.type_def with
      | Record { record; _ } ->
          (* Record type - generate the record object *)
          gen_object_type_and_jsont nsid out entry.name record
      | Object spec -> gen_object_type_and_jsont nsid out entry.name spec
      | Union spec -> gen_union_type_and_jsont nsid out entry.name spec
      | Query { parameters; output; _ } ->
          (* Query - generate params and output types *)
          Option.iter (gen_params_type_and_jsont nsid out) parameters;
          Option.iter
            (fun body -> gen_body_type_and_jsont nsid out body "output")
            output
      | Procedure { parameters; input; output; _ } ->
          (* Procedure - generate params, input, and output types *)
          Option.iter (gen_params_type_and_jsont nsid out) parameters;
          Option.iter
            (fun body -> gen_body_type_and_jsont nsid out body "input")
            input;
          Option.iter
            (fun body -> gen_body_type_and_jsont nsid out body "output")
            output
      | Subscription { parameters; message; _ } ->
          Option.iter (gen_params_type_and_jsont nsid out) parameters;
          Option.iter
            (fun body -> gen_body_type_and_jsont nsid out body "message")
            message
      | String { enum = Some values; _ } ->
          (* String enum - generate string type alias *)
          let type_name = Naming.type_name entry.name in
          emitln out
            (Printf.sprintf "(* Enum: %s *)" (String.concat ", " values));
          emitln out (Printf.sprintf "type %s = string" type_name);
          emitln out (Printf.sprintf "let %s_jsont = Jsont.string" type_name);
          emit_newline out
      | Array _ ->
          (* Array type - generate any inline unions from items first *)
          let unions = collect_inline_unions [] entry.type_def in
          List.iter (gen_inline_union_type_and_jsont nsid out) unions;
          let type_name = Naming.type_name entry.name in
          let base_type = gen_type_ref nsid out entry.type_def in
          let jsont_ref = gen_jsont_ref nsid out entry.type_def in
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
          emit_newline out
      | _ ->
          (* Other types - generate simple alias *)
          let type_name = Naming.type_name entry.name in
          let base_type = gen_type_ref nsid out entry.type_def in
          let jsont_ref = gen_jsont_ref nsid out entry.type_def in
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
          emit_newline out)
    sorted_defs;

  Emitter.contents out

(** {1 Interface Generation}

    Generate .mli files with ocamldoc from lexicon descriptions. *)

(** Escape special characters for OCamldoc comments *)
let escape_ocamldoc s =
  (* Escape characters that have special meaning in OCamldoc *)
  let b = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '{' | '}' | '[' | ']' | '@' ->
          Buffer.add_char b '\\';
          Buffer.add_char b c
      | _ -> Buffer.add_char b c)
    s;
  Buffer.contents b

(** Format a description as an OCamldoc comment *)
let format_docstring ?(indent = "") desc =
  match desc with
  | None -> ""
  | Some s when String.trim s = "" -> ""
  | Some s ->
      let escaped = escape_ocamldoc (String.trim s) in
      Printf.sprintf "%s(** %s *)\n" indent escaped

(** Get description from a type_def *)
let get_type_description (type_def : type_def) =
  match type_def with
  | String s -> s.description
  | Integer s -> s.description
  | Boolean s -> s.description
  | Bytes s -> s.description
  | Blob s -> s.description
  | CidLink s -> s.description
  | Array s -> s.description
  | Object s -> s.description
  | Ref s -> s.description
  | Union s -> s.description
  | Token s -> s.description
  | Unknown s -> s.description
  | Query s -> s.description
  | Procedure s -> s.description
  | Subscription s -> s.description
  | Record s -> s.description
  | PermissionSet s -> s.description

(** Generate inline union type for interface file (type + val only, no let) *)
let gen_inline_union_interface out (spec : union_spec) =
  let type_name = Naming.union_type_name spec.refs in
  (* Check if already generated using the type name *)
  if is_union_generated out type_name then ()
  else begin
    mark_union_generated out type_name;
    emitln out (Printf.sprintf "(* Union type: %s *)" type_name);
    emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
    emit_newline out;
    emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
    emit_newline out
  end

(** Generate interface for object type *)
let gen_object_interface ?(first = true) nsid out name (spec : object_spec) desc
    =
  (* First, generate any inline union types used in properties *)
  let unions =
    List.fold_left
      (fun acc (_, (prop : property)) ->
        collect_inline_unions acc prop.type_def)
      [] spec.properties
  in
  List.iter (gen_inline_union_interface out) unions;

  let required = Option.value spec.required ~default:[] in
  let nullable = Option.value spec.nullable ~default:[] in
  let type_name = Naming.type_name name in
  let keyword = if first then "type" else "and" in

  (* Emit documentation *)
  emitln out (format_docstring desc);

  (* Handle empty objects *)
  if spec.properties = [] then begin
    emitln out (Printf.sprintf "%s %s = unit" keyword type_name);
    emit_newline out;
    emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
    emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
    emit_newline out
  end
  else begin
    (* Generate type definition with field docs *)
    emitln out (Printf.sprintf "%s %s = {" keyword type_name);
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_type_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let type_str =
          if is_required && not is_nullable then base_type
          else base_type ^ " option"
        in
        (* Emit field with inline doc *)
        match prop.description with
        | Some desc when String.trim desc <> "" ->
            emitln out
              (Printf.sprintf "  %s : %s;  (** %s *)" ocaml_name type_str
                 (escape_ocamldoc desc))
        | _ -> emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;

    (* Generate jsont val *)
    emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
    emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
    emit_newline out
  end

(** Generate interface for union type *)
let gen_union_interface _nsid out name (_spec : union_spec) desc =
  let type_name = Naming.type_name name in
  emitln out (format_docstring desc);
  emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
  emit_newline out;
  emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
  emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
  emit_newline out

(** Generate interface for params type *)
let gen_params_interface nsid out (spec : params_spec) =
  (* Generate inline union interfaces *)
  let unions =
    List.fold_left
      (fun acc (_, (prop : property)) ->
        collect_inline_unions acc prop.type_def)
      [] spec.properties
  in
  List.iter (gen_inline_union_interface out) unions;
  let _ = nsid in
  (* suppress unused warning *)
  let required = Option.value spec.required ~default:[] in

  if spec.properties = [] then begin
    emitln out "(** Query/procedure parameters. *)";
    emitln out "type params = unit";
    emit_newline out;
    emitln out "(** Jsont codec for {!type:params}. *)";
    emitln out "val params_jsont : params Jsont.t";
    emit_newline out
  end
  else begin
    emitln out "(** Query/procedure parameters. *)";
    emitln out "type params = {";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_type_ref nsid out prop.type_def in
        let is_required = List.mem prop_name required in
        let type_str =
          if is_required then base_type else base_type ^ " option"
        in
        match prop.description with
        | Some desc when String.trim desc <> "" ->
            emitln out
              (Printf.sprintf "  %s : %s;  (** %s *)" ocaml_name type_str
                 (escape_ocamldoc desc))
        | _ -> emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;
    emitln out "(** Jsont codec for {!type:params}. *)";
    emitln out "val params_jsont : params Jsont.t";
    emit_newline out
  end

(** Generate interface for body type (input/output/message) *)
let gen_body_interface nsid out (body : body_def) suffix =
  match body.schema with
  | Some (Object spec) ->
      gen_object_interface nsid out suffix spec body.description
  | Some type_def ->
      let type_name = Naming.type_name suffix in
      let base_type = gen_type_ref nsid out type_def in
      emitln out (format_docstring body.description);
      emitln out (Printf.sprintf "type %s = %s" type_name base_type);
      emit_newline out;
      emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
      emitln out
        (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
      emit_newline out
  | None ->
      let type_name = Naming.type_name suffix in
      emitln out (format_docstring body.description);
      emitln out (Printf.sprintf "type %s = unit" type_name);
      emitln out
        (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
      emit_newline out

(** Generate a complete lexicon interface (.mli file). cyclic_nsids is a list of
    NSIDs that form dependency cycles. *)
let gen_lexicon_interface ?(cyclic_nsids = []) (doc : lexicon_doc) =
  let out = Emitter.make ~cyclic_nsids () in
  let nsid = doc.id in

  (* File header with module documentation *)
  emitln out (Printf.sprintf "(* Generated from %s *)" nsid);
  emitln out "";

  (* Module-level documentation from lexicon description *)
  (match doc.description with
  | Some desc when String.trim desc <> "" ->
      emitln out (Printf.sprintf "(** %s" (escape_ocamldoc (String.trim desc)));
      emitln out "";
      emitln out
        (Printf.sprintf "    @see <%s> AT Protocol Lexicon"
           ("https://docs.bsky.app/docs/api/"
           ^ String.map (function '.' -> '-' | c -> c) nsid));
      emitln out "*)"
  | _ -> emitln out (Printf.sprintf "(** Types and codecs for [%s]. *)" nsid));
  emitln out "";

  (* Topologically sort definitions so dependencies come first *)
  let sorted_defs = topo_sort_defs nsid doc.defs in

  (* Process each definition in sorted order *)
  List.iter
    (fun (entry : def_entry) ->
      let desc = get_type_description entry.type_def in
      match entry.type_def with
      | Record { record; description; _ } ->
          gen_object_interface nsid out entry.name record description
      | Object spec -> gen_object_interface nsid out entry.name spec desc
      | Union spec -> gen_union_interface nsid out entry.name spec desc
      | Query { parameters; output; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_params_interface nsid out) parameters;
          Option.iter
            (fun body -> gen_body_interface nsid out body "output")
            output
      | Procedure { parameters; input; output; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_params_interface nsid out) parameters;
          Option.iter
            (fun body -> gen_body_interface nsid out body "input")
            input;
          Option.iter
            (fun body -> gen_body_interface nsid out body "output")
            output
      | Subscription { parameters; message; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_params_interface nsid out) parameters;
          Option.iter
            (fun body -> gen_body_interface nsid out body "message")
            message
      | String { enum = Some values; _ } ->
          let type_name = Naming.type_name entry.name in
          emitln out (format_docstring desc);
          emitln out
            (Printf.sprintf "(** Enum values: %s *)"
               (String.concat ", "
                  (List.map (fun v -> "[\"" ^ v ^ "\"]") values)));
          emitln out (Printf.sprintf "type %s = string" type_name);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out
      | Array _ ->
          let unions = collect_inline_unions [] entry.type_def in
          List.iter (gen_inline_union_interface out) unions;
          let type_name = Naming.type_name entry.name in
          let base_type = gen_type_ref nsid out entry.type_def in
          emitln out (format_docstring desc);
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out
      | _ ->
          let type_name = Naming.type_name entry.name in
          let base_type = gen_type_ref nsid out entry.type_def in
          emitln out (format_docstring desc);
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out)
    sorted_defs;

  Emitter.contents out

(** {1 Unified Module Generation}

    Generate a single file with nested module structure where all types are
    defined inline. This ensures cross-references use public module paths
    instead of internal flat module names, avoiding odoc "hidden fields"
    warnings. *)

type unified_ctx = {
  current_nsid : string;  (** NSID of the lexicon currently being generated *)
  all_nsids : string list;  (** All NSIDs in this unified module *)
  cyclic_nsids : string list;  (** NSIDs that form cycles *)
}
(** Context for unified generation - tracks current location in module hierarchy
*)

(** Generate type reference using hierarchical module path. From current_nsid,
    generates relative path to target. *)
let rec gen_unified_type_ref ctx out (type_def : type_def) : string =
  match type_def with
  | String _ -> "string"
  | Integer { maximum; _ } -> (
      match maximum with Some m when m > 1073741823 -> "int64" | _ -> "int")
  | Boolean _ -> "bool"
  | Bytes _ -> "string"
  | Blob _ -> "Atp.Blob_ref.t"
  | CidLink _ -> "Atp.Cid.t"
  | Array { items; _ } ->
      let item_type = gen_unified_type_ref ctx out items in
      item_type ^ " list"
  | Object _ -> "Jsont.json"
  | Ref { ref_; _ } -> gen_unified_ref_type ctx out ref_
  | Union { refs; _ } -> (
      match refs with
      | [] -> "Jsont.json"
      | [ single_ref ] -> gen_unified_ref_type ctx out single_ref
      | _ -> "Jsont.json" (* Multi-ref unions use Jsont.json in unified mode *))
  | Token _ -> "string"
  | Unknown _ -> "Jsont.json"
  | Query _ | Procedure _ | Subscription _ | Record _ | PermissionSet _ ->
      "unit"

and gen_unified_ref_type ctx out ref_str : string =
  if String.length ref_str > 0 && ref_str.[0] = '#' then begin
    (* local ref: #someDef -> someDef *)
    if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
    else begin
      let def_name = String.sub ref_str 1 (String.length ref_str - 1) in
      Naming.type_name def_name
    end
  end
  else begin
    match String.split_on_char '#' ref_str with
    | [ ext_nsid; def_name ] ->
        if ext_nsid = ctx.current_nsid then begin
          (* Self-qualified local ref *)
          if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
          else Naming.type_name def_name
        end
        else if List.mem ext_nsid ctx.cyclic_nsids then "Jsont.json"
        else if List.mem ext_nsid ctx.all_nsids then begin
          (* Internal reference - use relative module path *)
          let rel_path =
            Naming.relative_module_path ~from_nsid:ctx.current_nsid
              ~to_nsid:ext_nsid
          in
          rel_path ^ "." ^ Naming.type_name def_name
        end
        else begin
          (* External reference - use flat module name *)
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ "." ^ Naming.type_name def_name
        end
    | [ ext_nsid ] ->
        if ext_nsid = ctx.current_nsid then begin
          if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
          else "main"
        end
        else if List.mem ext_nsid ctx.cyclic_nsids then "Jsont.json"
        else if List.mem ext_nsid ctx.all_nsids then begin
          let rel_path =
            Naming.relative_module_path ~from_nsid:ctx.current_nsid
              ~to_nsid:ext_nsid
          in
          rel_path ^ ".main"
        end
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ ".main"
        end
    | _ -> "invalid_ref"
  end

(** Generate jsont reference using hierarchical module path *)
let rec gen_unified_jsont_ref ctx out (type_def : type_def) : string =
  match type_def with
  | String _ -> "Jsont.string"
  | Integer { maximum; _ } -> (
      match maximum with
      | Some m when m > 1073741823 -> "Jsont.int64"
      | _ -> "Jsont.int")
  | Boolean _ -> "Jsont.bool"
  | Bytes _ -> "Jsont.binary_string"
  | Blob _ -> "Atp.Blob_ref.jsont"
  | CidLink _ -> "Atp.Cid.jsont"
  | Array { items; _ } ->
      let item_jsont = gen_unified_jsont_ref ctx out items in
      Printf.sprintf "(Jsont.list %s)" item_jsont
  | Object _ -> "Jsont.json"
  | Ref { ref_; _ } -> gen_unified_ref_jsont ctx out ref_
  | Union { refs; _ } -> (
      match refs with
      | [] -> "Jsont.json"
      | [ single_ref ] -> gen_unified_ref_jsont ctx out single_ref
      | _ -> "Jsont.json" (* Multi-ref unions use Jsont.json in unified mode *))
  | Token _ -> "Jsont.string"
  | Unknown _ -> "Jsont.json"
  | Query _ | Procedure _ | Subscription _ | Record _ | PermissionSet _ ->
      "Jsont.ignore"

and gen_unified_ref_jsont ctx out ref_str : string =
  if String.length ref_str > 0 && ref_str.[0] = '#' then begin
    if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
    else begin
      let def_name = String.sub ref_str 1 (String.length ref_str - 1) in
      Naming.type_name def_name ^ "_jsont"
    end
  end
  else begin
    match String.split_on_char '#' ref_str with
    | [ ext_nsid; def_name ] ->
        if ext_nsid = ctx.current_nsid then begin
          if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
          else Naming.type_name def_name ^ "_jsont"
        end
        else if List.mem ext_nsid ctx.cyclic_nsids then "Jsont.json"
        else if List.mem ext_nsid ctx.all_nsids then begin
          let rel_path =
            Naming.relative_module_path ~from_nsid:ctx.current_nsid
              ~to_nsid:ext_nsid
          in
          rel_path ^ "." ^ Naming.type_name def_name ^ "_jsont"
        end
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ "." ^ Naming.type_name def_name ^ "_jsont"
        end
    | [ ext_nsid ] ->
        if ext_nsid = ctx.current_nsid then begin
          if List.mem ctx.current_nsid ctx.cyclic_nsids then "Jsont.json"
          else "main_jsont"
        end
        else if List.mem ext_nsid ctx.cyclic_nsids then "Jsont.json"
        else if List.mem ext_nsid ctx.all_nsids then begin
          let rel_path =
            Naming.relative_module_path ~from_nsid:ctx.current_nsid
              ~to_nsid:ext_nsid
          in
          rel_path ^ ".main_jsont"
        end
        else begin
          let flat_module = Naming.flat_module_name_of_nsid ext_nsid in
          add_import out flat_module;
          flat_module ^ ".main_jsont"
        end
    | _ -> "Jsont.json (* invalid_ref *)"
  end

(** Generate object type using unified context *)
let gen_unified_object_type ?(first = true) ctx out name (spec : object_spec) =
  (* Note: We don't generate inline union types for unified modules because
     union type names can collide across module boundaries. Instead, union
     fields use Jsont.json directly (handled in gen_unified_type_ref). *)
  let required = Option.value spec.required ~default:[] in
  let nullable = Option.value spec.nullable ~default:[] in
  let type_name = Naming.type_name name in
  let keyword = if first then "type" else "and" in

  (* Compute the type ID for $type discriminator.
     For "main" definitions, use just the NSID; for others, use nsid#name *)
  let type_id =
    if name = "main" then ctx.current_nsid else ctx.current_nsid ^ "#" ^ name
  in

  if spec.properties = [] then begin
    emitln out (Printf.sprintf "%s %s = unit" keyword type_name);
    emit_newline out;
    emitln out (Printf.sprintf "let %s_jsont = Jsont.ignore" type_name);
    emit_newline out
  end
  else begin
    emitln out (Printf.sprintf "%s %s = {" keyword type_name);
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_unified_type_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let type_str =
          if is_required && not is_nullable then base_type
          else base_type ^ " option"
        in
        emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;

    (* Generate jsont codec with $type discriminator *)
    let kind_name = String.capitalize_ascii type_name in
    let param_names =
      List.map
        (fun (prop_name, _) -> Naming.field_name prop_name)
        spec.properties
    in
    let params_str = String.concat " " param_names in
    let record_str = String.concat "; " param_names in

    emitln out (Printf.sprintf "let %s_jsont =" type_name);
    emitln out (Printf.sprintf "  Jsont.Object.map ~kind:\"%s\"" kind_name);
    emitln out
      (Printf.sprintf "    (fun _typ %s -> { %s })" params_str record_str);
    emitln out
      (Printf.sprintf
         "  |> Jsont.Object.mem \"$type\" Jsont.string ~dec_absent:\"%s\" \
          ~enc:(fun _ -> \"%s\")"
         type_id type_id);

    (* Member declarations *)
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let jsont_ref = gen_unified_jsont_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let is_optional = (not is_required) || is_nullable in
        if is_optional then
          emitln out
            (Printf.sprintf
               "  |> Jsont.Object.opt_mem \"%s\" %s ~enc:(fun r -> r.%s)"
               prop_name jsont_ref ocaml_name)
        else
          emitln out
            (Printf.sprintf
               "  |> Jsont.Object.mem \"%s\" %s ~enc:(fun r -> r.%s)" prop_name
               jsont_ref ocaml_name))
      spec.properties;

    emitln out "  |> Jsont.Object.finish";
    emit_newline out
  end

(** Generate union type using unified context *)
let gen_unified_union_type _ctx out name (_spec : union_spec) =
  let type_name = Naming.type_name name in
  emitln out (Printf.sprintf "(* Union type: %s *)" type_name);
  emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
  emit_newline out;
  emitln out (Printf.sprintf "let %s_jsont = Jsont.json" type_name);
  emit_newline out

(** Generate params type using unified context *)
let gen_unified_params_type ctx out (spec : params_spec) =
  let required = Option.value spec.required ~default:[] in

  if spec.properties = [] then begin
    emitln out "type params = unit";
    emit_newline out;
    emitln out "let params_jsont = Jsont.ignore";
    emit_newline out
  end
  else begin
    emitln out "type params = {";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_unified_type_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        let type_str =
          if is_required then base_type else base_type ^ " option"
        in
        emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;

    emitln out "let params_jsont =";
    let param_names =
      List.map
        (fun (prop_name, _) -> Naming.field_name prop_name)
        spec.properties
    in
    let params_str = String.concat " " param_names in
    emitln out "  Jsont.Object.map ~kind:\"Params\"";
    emitln out (Printf.sprintf "    (fun %s -> {" params_str);
    List.iter
      (fun name -> emitln out (Printf.sprintf "      %s;" name))
      param_names;
    emitln out "    })";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let jsont_ref = gen_unified_jsont_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        if is_required then begin
          emitln out
            (Printf.sprintf "  |> Jsont.Object.mem \"%s\" %s" prop_name
               jsont_ref);
          emitln out (Printf.sprintf "       ~enc:(fun r -> r.%s)" ocaml_name)
        end
        else begin
          emitln out
            (Printf.sprintf "  |> Jsont.Object.opt_mem \"%s\" %s" prop_name
               jsont_ref);
          emitln out (Printf.sprintf "       ~enc:(fun r -> r.%s)" ocaml_name)
        end)
      spec.properties;
    emitln out "  |> Jsont.Object.finish";
    emit_newline out
  end

(** Generate body type using unified context *)
let gen_unified_body_type ctx out (body : body_def) suffix =
  match body.schema with
  | Some (Object spec) -> gen_unified_object_type ctx out suffix spec
  | Some type_def ->
      let type_name = Naming.type_name suffix in
      let base_type = gen_unified_type_ref ctx out type_def in
      emitln out (Printf.sprintf "type %s = %s" type_name base_type);
      emit_newline out;
      let jsont_ref = gen_unified_jsont_ref ctx out type_def in
      emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
      emit_newline out
  | None ->
      let type_name = Naming.type_name suffix in
      emitln out (Printf.sprintf "type %s = unit" type_name);
      emitln out (Printf.sprintf "let %s_jsont = Jsont.ignore" type_name);
      emit_newline out

(** Generate code for a single lexicon's definitions using unified context *)
let gen_unified_lexicon_content ctx out (doc : lexicon_doc) =
  let nsid = doc.id in
  let ctx = { ctx with current_nsid = nsid } in
  let sorted_defs = topo_sort_defs nsid doc.defs in

  List.iter
    (fun (entry : def_entry) ->
      match entry.type_def with
      | Record { record; _ } ->
          gen_unified_object_type ctx out entry.name record
      | Object spec -> gen_unified_object_type ctx out entry.name spec
      | Union spec -> gen_unified_union_type ctx out entry.name spec
      | Query { parameters; output; _ } ->
          Option.iter (gen_unified_params_type ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_type ctx out body "output")
            output
      | Procedure { parameters; input; output; _ } ->
          Option.iter (gen_unified_params_type ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_type ctx out body "input")
            input;
          Option.iter
            (fun body -> gen_unified_body_type ctx out body "output")
            output
      | Subscription { parameters; message; _ } ->
          Option.iter (gen_unified_params_type ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_type ctx out body "message")
            message
      | String { enum = Some values; _ } ->
          let type_name = Naming.type_name entry.name in
          emitln out
            (Printf.sprintf "(* Enum: %s *)" (String.concat ", " values));
          emitln out (Printf.sprintf "type %s = string" type_name);
          emitln out (Printf.sprintf "let %s_jsont = Jsont.string" type_name);
          emit_newline out
      | Array _ ->
          let type_name = Naming.type_name entry.name in
          let base_type = gen_unified_type_ref ctx out entry.type_def in
          let jsont_ref = gen_unified_jsont_ref ctx out entry.type_def in
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
          emit_newline out
      | _ ->
          let type_name = Naming.type_name entry.name in
          let base_type = gen_unified_type_ref ctx out entry.type_def in
          let jsont_ref = gen_unified_jsont_ref ctx out entry.type_def in
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out (Printf.sprintf "let %s_jsont = %s" type_name jsont_ref);
          emit_newline out)
    sorted_defs

(** Build a map from NSID to lexicon document *)
let build_lexicon_map (lexicons : lexicon_doc list) =
  List.fold_left (fun m doc -> (doc.id, doc) :: m) [] lexicons

(** Sort lexicons by their dependencies (topological order). Lexicons with no
    dependencies come first. *)
let sort_lexicons_by_deps (lexicons : lexicon_doc list) =
  Scc.find_file_sccs lexicons |> List.concat

(** Collect all NSIDs under a trie node *)
let rec collect_nsids_in_trie (trie : Naming.trie) : string list =
  match trie with
  | Naming.Module nsid -> [ nsid ]
  | Naming.Node children ->
      List.concat_map (fun (_, child) -> collect_nsids_in_trie child) children
  | Naming.ModuleWithChildren (nsid, children) ->
      nsid
      :: List.concat_map
           (fun (_, child) -> collect_nsids_in_trie child)
           children

(** Get the first segment of an NSID after a given prefix. e.g.,
    prefix="app.bsky", nsid="app.bsky.feed.defs" -> Some "feed" *)
let get_segment_after_prefix ~prefix nsid =
  let prefix_segs = String.split_on_char '.' prefix in
  let nsid_segs = String.split_on_char '.' nsid in
  let rec drop_prefix p n =
    match (p, n) with
    | [], seg :: _ -> Some seg
    | ph :: pt, nh :: nt when ph = nh -> drop_prefix pt nt
    | _ -> None
  in
  drop_prefix prefix_segs nsid_segs

(** Detect cycles in a dependency graph and return list of nodes in cycles *)
let find_nodes_in_cycles (deps : (string * string list) list) : string list =
  (* Use Tarjan's algorithm to find SCCs *)
  let nodes = List.map fst deps in
  let index_counter = ref 0 in
  let indices = Hashtbl.create 64 in
  let lowlinks = Hashtbl.create 64 in
  let on_stack = Hashtbl.create 64 in
  let stack = ref [] in
  let sccs = ref [] in
  let rec strongconnect node =
    let index = !index_counter in
    incr index_counter;
    Hashtbl.add indices node index;
    Hashtbl.add lowlinks node index;
    Hashtbl.add on_stack node true;
    stack := node :: !stack;
    let successors = List.assoc_opt node deps |> Option.value ~default:[] in
    List.iter
      (fun succ ->
        if List.mem succ nodes then begin
          if not (Hashtbl.mem indices succ) then begin
            strongconnect succ;
            Hashtbl.replace lowlinks node
              (min (Hashtbl.find lowlinks node) (Hashtbl.find lowlinks succ))
          end
          else if Hashtbl.find_opt on_stack succ = Some true then
            Hashtbl.replace lowlinks node
              (min (Hashtbl.find lowlinks node) (Hashtbl.find indices succ))
        end)
      successors;
    if Hashtbl.find lowlinks node = Hashtbl.find indices node then begin
      let rec pop_scc acc =
        match !stack with
        | [] -> acc
        | top :: rest ->
            stack := rest;
            Hashtbl.replace on_stack top false;
            if top = node then top :: acc else pop_scc (top :: acc)
      in
      let scc = pop_scc [] in
      if List.length scc > 1 then sccs := scc :: !sccs
    end
  in
  List.iter
    (fun node -> if not (Hashtbl.mem indices node) then strongconnect node)
    nodes;
  List.concat !sccs

(** Sort trie children by their dependencies. children_prefix is the NSID prefix
    for the parent node (e.g., "app.bsky"). Uses lexicon external dependencies
    to determine ordering. Returns (sorted_children, cyclic_sibling_names) where
    cyclic_sibling_names contains names of children that are part of dependency
    cycles. *)
let sort_children_by_deps ~children_prefix
    (children : (string * Naming.trie) list) (lexicons : lexicon_doc list) =
  (* Build map from child name to list of NSIDs under it *)
  let child_nsids =
    List.map (fun (name, trie) -> (name, collect_nsids_in_trie trie)) children
  in
  (* Build map from NSID to its child name *)
  let nsid_to_child =
    List.concat_map
      (fun (name, nsids) -> List.map (fun nsid -> (nsid, name)) nsids)
      child_nsids
  in
  (* For each child, find what other children it depends on *)
  let child_deps =
    List.map
      (fun (name, nsids) ->
        let deps =
          List.concat_map
            (fun nsid ->
              match List.find_opt (fun doc -> doc.id = nsid) lexicons with
              | None -> []
              | Some doc ->
                  let ext_nsids = Scc.get_external_nsids doc in
                  (* For each external dep, find which child it belongs to *)
                  List.filter_map
                    (fun ext_nsid ->
                      (* Check if the external dep is within our parent prefix *)
                      match
                        get_segment_after_prefix ~prefix:children_prefix
                          ext_nsid
                      with
                      | Some seg when seg <> name ->
                          (* Depends on sibling child *)
                          if List.mem_assoc seg children then Some seg else None
                      | _ -> (
                          (* Also check direct child mapping *)
                          match List.assoc_opt ext_nsid nsid_to_child with
                          | Some dep_child when dep_child <> name ->
                              Some dep_child
                          | _ -> None))
                    ext_nsids)
            nsids
        in
        (name, List.sort_uniq String.compare deps))
      child_nsids
  in
  (* Find nodes that are part of cycles *)
  let cyclic_nodes = find_nodes_in_cycles child_deps in
  (* For topological sort, remove cyclic deps (treat cyclic siblings as having no deps on each other) *)
  let acyclic_deps =
    List.map
      (fun (name, deps) ->
        let filtered_deps =
          List.filter
            (fun dep ->
              not (List.mem name cyclic_nodes && List.mem dep cyclic_nodes))
            deps
        in
        (name, filtered_deps))
      child_deps
  in
  (* Topological sort using Kahn's algorithm *)
  let rec topo_sort sorted remaining =
    if remaining = [] then List.rev sorted
    else
      let sorted_names = List.map fst sorted in
      let ready, not_ready =
        List.partition
          (fun (name, _) ->
            let deps = List.assoc name acyclic_deps in
            List.for_all (fun dep -> List.mem dep sorted_names) deps)
          remaining
      in
      match ready with
      | [] ->
          (* Shouldn't happen after removing cyclic deps, but fall back to alphabetical *)
          List.rev sorted
          @ List.sort (fun (a, _) (b, _) -> String.compare a b) not_ready
      | _ ->
          (* Sort ready elements alphabetically for deterministic output *)
          let ready_sorted =
            List.sort (fun (a, _) (b, _) -> String.compare a b) ready
          in
          topo_sort (ready_sorted @ sorted) not_ready
  in
  (topo_sort [] children, cyclic_nodes)

(** Generate the unified module with nested structure. All types are defined
    inline using hierarchical module paths. *)
let gen_unified_module ~module_name (lexicons : lexicon_doc list) =
  let out = Emitter.make () in
  let all_nsids = List.map (fun doc -> doc.id) lexicons in

  (* Find file-level cyclic NSIDs *)
  let sccs = Scc.find_file_sccs lexicons in
  let file_cyclic_nsids =
    List.concat_map
      (fun scc ->
        if List.length scc > 1 then List.map (fun doc -> doc.id) scc else [])
      sccs
  in

  (* Header *)
  emitln out
    (Printf.sprintf "(* %s - generated from atproto lexicons *)" module_name);
  emit_newline out;

  (* Generate utility module for filtering list parsing *)
  emitln out "(** Utility functions for resilient parsing. *)";
  emitln out "module Filter = struct";
  emitln out
    "  (** [filter_list jsont json_list] parses each element with [jsont],";
  emitln out
    "      returning only successfully parsed elements. Non-compliant records";
  emitln out "      are silently skipped. *)";
  emitln out
    "  let filter_list (type a) (jsont : a Jsont.t) (json_list : Jsont.json \
     list) : a list =";
  emitln out "    List.filter_map (fun json ->";
  emitln out "      match Jsont.Json.decode jsont json with";
  emitln out "      | Ok v -> Some v";
  emitln out "      | Error _ -> None";
  emitln out "    ) json_list";
  emitln out "end";
  emit_newline out;

  (* Sort lexicons so dependencies come first *)
  let sorted_lexicons = sort_lexicons_by_deps lexicons in

  (* Build the module trie structure *)
  let trie = Naming.group_nsids_by_prefix all_nsids in

  (* Build lexicon map for lookup *)
  let lexicon_map = build_lexicon_map sorted_lexicons in

  (* Collect sibling-level cyclic NSIDs as we traverse the trie *)
  let sibling_cyclic_nsids = ref [] in

  (* Generate nested module structure *)
  let rec gen_trie indent prefix (children : (string * Naming.trie) list) =
    (* Sort children by dependencies, get cyclic siblings *)
    let sorted_children, cyclic_siblings =
      sort_children_by_deps ~children_prefix:prefix children lexicons
    in
    (* Add NSIDs under cyclic siblings to the cyclic list *)
    List.iter
      (fun sibling_name ->
        match List.assoc_opt sibling_name children with
        | Some trie ->
            let nsids = collect_nsids_in_trie trie in
            sibling_cyclic_nsids := nsids @ !sibling_cyclic_nsids
        | None -> ())
      cyclic_siblings;
    List.iter
      (fun (key, child) ->
        let child_prefix = if prefix = "" then key else prefix ^ "." ^ key in
        match child with
        | Naming.Module nsid ->
            (* Generate module with lexicon content only *)
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s = struct" indent mod_name);
            let ctx =
              {
                current_nsid = nsid;
                all_nsids;
                cyclic_nsids = file_cyclic_nsids @ !sibling_cyclic_nsids;
              }
            in
            (match List.assoc_opt nsid lexicon_map with
            | Some doc -> gen_unified_lexicon_content ctx out doc
            | None -> ());
            emitln out (Printf.sprintf "%send" indent)
        | Naming.Node subchildren ->
            (* Generate module with children only (no content at this level) *)
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s = struct" indent mod_name);
            gen_trie (indent ^ "  ") child_prefix subchildren;
            emitln out (Printf.sprintf "%send" indent)
        | Naming.ModuleWithChildren (nsid, subchildren) ->
            (* Generate module with BOTH lexicon content AND children *)
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s = struct" indent mod_name);
            let ctx =
              {
                current_nsid = nsid;
                all_nsids;
                cyclic_nsids = file_cyclic_nsids @ !sibling_cyclic_nsids;
              }
            in
            (match List.assoc_opt nsid lexicon_map with
            | Some doc -> gen_unified_lexicon_content ctx out doc
            | None -> ());
            (* Also generate child modules *)
            gen_trie (indent ^ "  ") child_prefix subchildren;
            emitln out (Printf.sprintf "%send" indent))
      sorted_children
  in

  gen_trie "" "" trie;

  Emitter.contents out

(** {1 Unified Interface Generation}

    Generate .mli files with nested module signatures matching the unified
    module structure. *)

(** Generate interface for object type using unified context *)
let gen_unified_object_interface ?(first = true) ctx out name
    (spec : object_spec) desc =
  let required = Option.value spec.required ~default:[] in
  let nullable = Option.value spec.nullable ~default:[] in
  let type_name = Naming.type_name name in
  let keyword = if first then "type" else "and" in

  emitln out (format_docstring desc);

  if spec.properties = [] then begin
    emitln out (Printf.sprintf "%s %s = unit" keyword type_name);
    emit_newline out;
    emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
    emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
    emit_newline out
  end
  else begin
    let _ = ctx in
    (* suppress unused warning *)
    emitln out (Printf.sprintf "%s %s = {" keyword type_name);
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_unified_type_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        let is_nullable = List.mem prop_name nullable in
        let type_str =
          if is_required && not is_nullable then base_type
          else base_type ^ " option"
        in
        match prop.description with
        | Some desc when String.trim desc <> "" ->
            emitln out
              (Printf.sprintf "  %s : %s;  (** %s *)" ocaml_name type_str
                 (escape_ocamldoc desc))
        | _ -> emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;
    emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
    emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
    emit_newline out
  end

(** Generate interface for union type using unified context *)
let gen_unified_union_interface _ctx out name (_spec : union_spec) desc =
  let type_name = Naming.type_name name in
  emitln out (format_docstring desc);
  emitln out (Printf.sprintf "type %s = Jsont.json" type_name);
  emit_newline out;
  emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
  emitln out (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
  emit_newline out

(** Generate interface for params type using unified context *)
let gen_unified_params_interface ctx out (spec : params_spec) =
  let required = Option.value spec.required ~default:[] in

  if spec.properties = [] then begin
    emitln out "(** Query/procedure parameters. *)";
    emitln out "type params = unit";
    emit_newline out;
    emitln out "(** Jsont codec for {!type:params}. *)";
    emitln out "val params_jsont : params Jsont.t";
    emit_newline out
  end
  else begin
    emitln out "(** Query/procedure parameters. *)";
    emitln out "type params = {";
    List.iter
      (fun (prop_name, (prop : property)) ->
        let ocaml_name = Naming.field_name prop_name in
        let base_type = gen_unified_type_ref ctx out prop.type_def in
        let is_required = List.mem prop_name required in
        let type_str =
          if is_required then base_type else base_type ^ " option"
        in
        match prop.description with
        | Some desc when String.trim desc <> "" ->
            emitln out
              (Printf.sprintf "  %s : %s;  (** %s *)" ocaml_name type_str
                 (escape_ocamldoc desc))
        | _ -> emitln out (Printf.sprintf "  %s : %s;" ocaml_name type_str))
      spec.properties;
    emitln out "}";
    emit_newline out;
    emitln out "(** Jsont codec for {!type:params}. *)";
    emitln out "val params_jsont : params Jsont.t";
    emit_newline out
  end

(** Generate interface for body type using unified context *)
let gen_unified_body_interface ctx out (body : body_def) suffix =
  match body.schema with
  | Some (Object spec) ->
      gen_unified_object_interface ctx out suffix spec body.description
  | Some type_def ->
      let type_name = Naming.type_name suffix in
      let base_type = gen_unified_type_ref ctx out type_def in
      emitln out (format_docstring body.description);
      emitln out (Printf.sprintf "type %s = %s" type_name base_type);
      emit_newline out;
      emitln out (Printf.sprintf "(** Jsont codec for {!type:%s}. *)" type_name);
      emitln out
        (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
      emit_newline out
  | None ->
      let type_name = Naming.type_name suffix in
      emitln out (format_docstring body.description);
      emitln out (Printf.sprintf "type %s = unit" type_name);
      emitln out
        (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
      emit_newline out

(** Generate interface for a single lexicon's definitions using unified context
*)
let gen_unified_lexicon_interface_content ctx out (doc : lexicon_doc) =
  let nsid = doc.id in
  let ctx = { ctx with current_nsid = nsid } in
  let sorted_defs = topo_sort_defs nsid doc.defs in

  List.iter
    (fun (entry : def_entry) ->
      let desc = get_type_description entry.type_def in
      match entry.type_def with
      | Record { record; description; _ } ->
          gen_unified_object_interface ctx out entry.name record description
      | Object spec -> gen_unified_object_interface ctx out entry.name spec desc
      | Union spec -> gen_unified_union_interface ctx out entry.name spec desc
      | Query { parameters; output; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_unified_params_interface ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_interface ctx out body "output")
            output
      | Procedure { parameters; input; output; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_unified_params_interface ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_interface ctx out body "input")
            input;
          Option.iter
            (fun body -> gen_unified_body_interface ctx out body "output")
            output
      | Subscription { parameters; message; description; _ } ->
          emitln out (format_docstring description);
          Option.iter (gen_unified_params_interface ctx out) parameters;
          Option.iter
            (fun body -> gen_unified_body_interface ctx out body "message")
            message
      | String { enum = Some values; _ } ->
          let type_name = Naming.type_name entry.name in
          emitln out (format_docstring desc);
          emitln out
            (Printf.sprintf "(** Enum values: %s *)"
               (String.concat ", "
                  (List.map (fun v -> "[\"" ^ v ^ "\"]") values)));
          emitln out (Printf.sprintf "type %s = string" type_name);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out
      | Array _ ->
          let type_name = Naming.type_name entry.name in
          let base_type = gen_unified_type_ref ctx out entry.type_def in
          emitln out (format_docstring desc);
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out
      | _ ->
          let type_name = Naming.type_name entry.name in
          let base_type = gen_unified_type_ref ctx out entry.type_def in
          emitln out (format_docstring desc);
          emitln out (Printf.sprintf "type %s = %s" type_name base_type);
          emitln out
            (Printf.sprintf "val %s_jsont : %s Jsont.t" type_name type_name);
          emit_newline out)
    sorted_defs

(** Generate the unified interface (.mli) with nested module signatures. Matches
    the structure of {!gen_unified_module}. *)
let gen_unified_interface ~module_name (lexicons : lexicon_doc list) =
  let out = Emitter.make () in
  let all_nsids = List.map (fun doc -> doc.id) lexicons in

  (* Find file-level cyclic NSIDs *)
  let sccs = Scc.find_file_sccs lexicons in
  let file_cyclic_nsids =
    List.concat_map
      (fun scc ->
        if List.length scc > 1 then List.map (fun doc -> doc.id) scc else [])
      sccs
  in

  (* Header *)
  emitln out
    (Printf.sprintf "(* %s - generated from atproto lexicons *)" module_name);
  emit_newline out;
  emitln out
    (Printf.sprintf "(** AT Protocol lexicon types and Jsont codecs for %s. *)"
       module_name);
  emit_newline out;

  (* Generate utility module signature for filtering list parsing *)
  emitln out "(** Utility functions for resilient parsing. *)";
  emitln out "module Filter : sig";
  emitln out "  val filter_list : 'a Jsont.t -> Jsont.json list -> 'a list";
  emitln out
    "  (** [filter_list jsont json_list] parses each element with [jsont],";
  emitln out
    "      returning only successfully parsed elements. Non-compliant records";
  emitln out "      are silently skipped. *)";
  emitln out "end";
  emit_newline out;

  (* Sort lexicons so dependencies come first *)
  let sorted_lexicons = sort_lexicons_by_deps lexicons in

  (* Build the module trie structure *)
  let trie = Naming.group_nsids_by_prefix all_nsids in

  (* Build lexicon map for lookup *)
  let lexicon_map = build_lexicon_map sorted_lexicons in

  (* Collect sibling-level cyclic NSIDs as we traverse the trie *)
  let sibling_cyclic_nsids = ref [] in

  (* Generate nested module signature structure *)
  let rec gen_trie_sig indent prefix (children : (string * Naming.trie) list) =
    let sorted_children, cyclic_siblings =
      sort_children_by_deps ~children_prefix:prefix children lexicons
    in
    List.iter
      (fun sibling_name ->
        match List.assoc_opt sibling_name children with
        | Some trie ->
            let nsids = collect_nsids_in_trie trie in
            sibling_cyclic_nsids := nsids @ !sibling_cyclic_nsids
        | None -> ())
      cyclic_siblings;
    List.iter
      (fun (key, child) ->
        let child_prefix = if prefix = "" then key else prefix ^ "." ^ key in
        match child with
        | Naming.Module nsid ->
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s : sig" indent mod_name);
            let ctx =
              {
                current_nsid = nsid;
                all_nsids;
                cyclic_nsids = file_cyclic_nsids @ !sibling_cyclic_nsids;
              }
            in
            (match List.assoc_opt nsid lexicon_map with
            | Some doc -> gen_unified_lexicon_interface_content ctx out doc
            | None -> ());
            emitln out (Printf.sprintf "%send" indent)
        | Naming.Node subchildren ->
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s : sig" indent mod_name);
            gen_trie_sig (indent ^ "  ") child_prefix subchildren;
            emitln out (Printf.sprintf "%send" indent)
        | Naming.ModuleWithChildren (nsid, subchildren) ->
            (* Generate module signature with BOTH content AND children *)
            let mod_name = String.capitalize_ascii key in
            emitln out (Printf.sprintf "%smodule %s : sig" indent mod_name);
            let ctx =
              {
                current_nsid = nsid;
                all_nsids;
                cyclic_nsids = file_cyclic_nsids @ !sibling_cyclic_nsids;
              }
            in
            (match List.assoc_opt nsid lexicon_map with
            | Some doc -> gen_unified_lexicon_interface_content ctx out doc
            | None -> ());
            (* Also generate child module signatures *)
            gen_trie_sig (indent ^ "  ") child_prefix subchildren;
            emitln out (Printf.sprintf "%send" indent))
      sorted_children
  in

  gen_trie_sig "" "" trie;

  Emitter.contents out
