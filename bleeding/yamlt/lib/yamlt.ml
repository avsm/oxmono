(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Jsont.Repr
open Yamlrw

(* YAML format *)

type yaml_format = Block | Flow | Layout

(* Decoder *)

type decoder = {
  parser : Parser.t;
  file : string;
  locs : bool;
  _layout : bool; (* For future layout preservation *)
  max_depth : int;
  max_nodes : int;
  mutable node_count : int;
  mutable current : Event.spanned option;
  _anchors : (string, Jsont.json) Hashtbl.t; (* For future anchor resolution *)
  meta_none : Jsont.Meta.t;
}

let make_decoder ?(locs = false) ?(layout = false) ?(file = "-")
    ?(max_depth = 100) ?(max_nodes = 10_000_000) parser =
  let meta_none = Jsont.Meta.make (Jsont.Textloc.(set_file none) file) in
  {
    parser;
    file;
    locs;
    _layout = layout;
    max_depth;
    max_nodes;
    node_count = 0;
    current = None;
    _anchors = Hashtbl.create 16;
    meta_none;
  }

(* Decoder helpers *)

(* Local helper to reduce Jsont.Error.msgf boilerplate *)
let err_msg meta fmt = Jsont.Error.msgf meta fmt
let err_msg_none fmt = Jsont.Error.msgf Jsont.Meta.none fmt

let check_depth d ~nest =
  if nest > d.max_depth then
    err_msg_none "Maximum nesting depth %d exceeded" d.max_depth

let check_nodes d =
  d.node_count <- d.node_count + 1;
  if d.node_count > d.max_nodes then
    err_msg_none "Maximum node count %d exceeded" d.max_nodes

let meta_of_span d span =
  if not d.locs then d.meta_none
  else
    let start = span.Span.start and stop = span.Span.stop in
    let first_byte = start.Position.index in
    let last_byte = max first_byte (stop.Position.index - 1) in
    (* line_pos is (line_number, byte_position_of_line_start) *)
    let first_line =
      (start.Position.line, start.Position.index - start.Position.column + 1)
    in
    (* Handle case where stop is at the start of a new line (column 1)
       This happens when the span includes a trailing newline.
       The last_byte is on the previous line, so we need to calculate
       the line start position based on last_byte, not stop. *)
    let last_line =
      if stop.Position.column = 1 && stop.Position.line > start.Position.line
      then
        (* last_byte is on the previous line (stop.line - 1)
           We need to estimate where that line starts. Since we don't have
           the full text, we can't calculate it exactly, but we can use:
           last_byte - (estimated_column - 1)
           For now, we'll use the same line as start if they're close,
           or just report it as the previous line. *)
        let last_line_num = stop.Position.line - 1 in
        (* Estimate: assume last_byte is somewhere on the previous line.
           We'll use the byte position minus a reasonable offset.
           This is approximate but better than wrapping to the next line. *)
        if last_line_num = start.Position.line then
          (* Same line as start - use start's line position *)
          first_line
        else
          (* Different line - estimate line start as last_byte minus some offset
             Since we subtracted 1 from stop.index to get last_byte, and stop.column was 1,
             last_byte should be the newline character on the previous line.
             The line likely started much earlier, but we'll estimate conservatively. *)
          (last_line_num, last_byte)
      else (stop.Position.line, stop.Position.index - stop.Position.column + 1)
    in
    let textloc =
      Jsont.Textloc.make ~file:d.file ~first_byte ~last_byte ~first_line
        ~last_line
    in
    Jsont.Meta.make textloc

let next_event d =
  d.current <- Parser.next d.parser;
  d.current

let peek_event d =
  match d.current with Some _ -> d.current | None -> next_event d

let skip_event d = d.current <- None

let _expect_event d pred name =
  match peek_event d with
  | Some ev when pred ev.Event.event ->
      skip_event d;
      ev
  | Some ev ->
      let span = ev.Event.span in
      let meta = meta_of_span d span in
      err_msg meta "Expected %s but found %a" name Event.pp ev.Event.event
  | None -> err_msg_none "Expected %s but reached end of stream" name

(* Error helpers *)

let _err_expected_scalar d ev =
  let meta = meta_of_span d ev.Event.span in
  err_msg meta "Expected scalar but found %a" Event.pp ev.Event.event

let err_type_mismatch d span t ~fnd =
  let open Jsont.Repr in
  let meta = meta_of_span d span in
  err_msg meta "Expected %s but found %s" (kinded_sort t) fnd

(* YAML scalar resolution *)

let is_null_scalar s =
  s = "" || s = "~" || s = "null" || s = "Null" || s = "NULL"

let bool_of_scalar_opt s =
  match s with
  | "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on" | "On" | "ON" ->
      Some true
  | "false" | "False" | "FALSE" | "no" | "No" | "NO" | "off" | "Off" | "OFF" ->
      Some false
  | _ -> None

let float_of_scalar_opt s =
  (* Handle YAML special floats *)
  match s with
  | ".inf" | ".Inf" | ".INF" -> Some Float.infinity
  | "+.inf" | "+.Inf" | "+.INF" -> Some Float.infinity
  | "-.inf" | "-.Inf" | "-.INF" -> Some Float.neg_infinity
  | ".nan" | ".NaN" | ".NAN" -> Some Float.nan
  | _ -> (
      (* Try parsing as number, allowing underscores *)
      let s' = String.concat "" (String.split_on_char '_' s) in
      (* Try int first (supports 0o, 0x, 0b) then float *)
      match int_of_string_opt s' with
      | Some i -> Some (float_of_int i)
      | None -> float_of_string_opt s')

let _int_of_scalar_opt s =
  (* Handle hex, octal, and regular integers with underscores *)
  let s' = String.concat "" (String.split_on_char '_' s) in
  int_of_string_opt s'

(* Decode a scalar value according to expected type *)
let rec decode_scalar_as : type a.
    decoder -> Event.spanned -> string -> Scalar_style.t -> a t -> a =
 fun d ev value style t ->
  check_nodes d;
  let meta = meta_of_span d ev.Event.span in
  match t with
  | Null map ->
      if is_null_scalar value then map.dec meta ()
      else err_type_mismatch d ev.span t ~fnd:("scalar " ^ value)
  | Bool map -> (
      match bool_of_scalar_opt value with
      | Some b -> map.dec meta b
      | None ->
          (* For explicitly quoted strings, fail *)
          if style <> `Plain then
            err_type_mismatch d ev.span t ~fnd:("string " ^ value)
          else err_type_mismatch d ev.span t ~fnd:("scalar " ^ value))
  | Number map -> (
      if
        (* Handle null -> nan mapping like jsont *)
        is_null_scalar value
      then map.dec meta Float.nan
      else
        match float_of_scalar_opt value with
        | Some f -> map.dec meta f
        | None -> err_type_mismatch d ev.span t ~fnd:("scalar " ^ value))
  | String map ->
      (* Don't decode null values as strings - they should fail so outer combinators
         like 'option' or 'any' can handle them properly.
         BUT: quoted strings should always be treated as strings, even if they
         look like null (e.g., "" or "null") *)
      if style = `Plain && is_null_scalar value then
        err_type_mismatch d ev.span t ~fnd:"null"
      else
        (* Strings accept quoted scalars or non-null plain scalars *)
        map.dec meta value
  | Array map ->
      (* Treat null as an empty array for convenience *)
      if is_null_scalar value then
        let end_meta = meta_of_span d ev.Event.span in
        map.dec_finish end_meta 0 (map.dec_empty ())
      else err_type_mismatch d ev.span t ~fnd:"scalar"
  | Object map ->
      (* Treat null as an empty object for convenience *)
      if is_null_scalar value then
        (* Build a dict with all default values from absent members *)
        let add_default _ (Mem_dec mem_map) dict =
          match mem_map.dec_absent with
          | Some v -> Dict.add mem_map.id v dict
          | None ->
              (* Required field without default - error *)
              let exp = String_map.singleton mem_map.name (Mem_dec mem_map) in
              missing_mems_error meta map ~exp ~fnd:[]
        in
        let dict = String_map.fold add_default map.mem_decs Dict.empty in
        let dict = Dict.add object_meta_arg meta dict in
        apply_dict map.dec dict
      else err_type_mismatch d ev.span t ~fnd:"scalar"
  | Map m ->
      (* Handle Map combinators (e.g., from Jsont.option) *)
      m.dec (decode_scalar_as d ev value style m.dom)
  | Rec lazy_t ->
      (* Handle recursive types *)
      decode_scalar_as d ev value style (Lazy.force lazy_t)
  | _ -> err_type_mismatch d ev.span t ~fnd:"scalar"

(* Forward declaration for mutual recursion *)
let rec decode : type a. decoder -> nest:int -> a t -> a =
 fun d ~nest t ->
  check_depth d ~nest;
  match peek_event d with
  | None -> err_msg_none "Unexpected end of YAML stream"
  | Some ev -> (
      match (ev.Event.event, t) with
      (* Scalar events *)
      | Event.Scalar { value; style; anchor; _ }, _ ->
          skip_event d;
          let result = decode_scalar d ~nest ev value style t in
          (* Store anchor if present - TODO: implement anchor storage *)
          (match anchor with
          | Some _name ->
              (* We need generic JSON for anchors - decode as json and convert back *)
              ()
          | None -> ());
          result
      (* Alias *)
      | Event.Alias { anchor }, _ ->
          skip_event d;
          decode_alias d ev anchor t
      (* Map combinator - must come before specific event matches *)
      | _, Map m -> m.dec (decode d ~nest m.dom)
      (* Recursive types - must come before specific event matches *)
      | _, Rec lazy_t -> decode d ~nest (Lazy.force lazy_t)
      (* Sequence -> Array *)
      | Event.Sequence_start _, Array map -> decode_array d ~nest ev map
      | Event.Sequence_start _, Any map -> decode_any_sequence d ~nest ev t map
      | Event.Sequence_start _, _ ->
          err_type_mismatch d ev.span t ~fnd:"sequence"
      (* Mapping -> Object *)
      | Event.Mapping_start _, Object map -> decode_object d ~nest ev map
      | Event.Mapping_start _, Any map -> decode_any_mapping d ~nest ev t map
      | Event.Mapping_start _, _ -> err_type_mismatch d ev.span t ~fnd:"mapping"
      (* Unexpected events *)
      | Event.Sequence_end, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected sequence end"
      | Event.Mapping_end, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected mapping end"
      | Event.Document_start _, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected document start"
      | Event.Document_end _, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected document end"
      | Event.Stream_start _, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected stream start"
      | Event.Stream_end, _ ->
          err_msg (meta_of_span d ev.span) "Unexpected stream end")

and decode_scalar : type a.
    decoder -> nest:int -> Event.spanned -> string -> Scalar_style.t -> a t -> a
    =
 fun d ~nest ev value style t ->
  match t with
  | Any map -> decode_any_scalar d ev value style t map
  | Map m -> m.dec (decode_scalar d ~nest ev value style m.dom)
  | Rec lazy_t -> decode_scalar d ~nest ev value style (Lazy.force lazy_t)
  | _ -> decode_scalar_as d ev value style t

and decode_any_scalar : type a.
    decoder ->
    Event.spanned ->
    string ->
    Scalar_style.t ->
    a t ->
    a any_map ->
    a =
 fun d ev value style t map ->
  check_nodes d;
  let meta = meta_of_span d ev.span in
  let type_err fnd = Jsont.Repr.type_error meta t ~fnd in
  (* Determine which decoder to use based on scalar content.
     IMPORTANT: Quoted scalars that look like null (e.g., "" or "null")
     should be treated as strings, not null. Only plain scalars
     should be resolved as null. *)
  if style = `Plain && is_null_scalar value then
    match map.dec_null with
    | Some t' -> decode_scalar_as d ev value style t'
    | None -> type_err Jsont.Sort.Null
  else if style = `Plain then
    (* Try bool, then number, then string *)
    match bool_of_scalar_opt value with
    | Some _ -> (
        match map.dec_bool with
        | Some t' -> decode_scalar_as d ev value style t'
        | None -> (
            match map.dec_string with
            | Some t' -> decode_scalar_as d ev value style t'
            | None -> type_err Jsont.Sort.Bool))
    | None -> (
        match float_of_scalar_opt value with
        | Some _ -> (
            match map.dec_number with
            | Some t' -> decode_scalar_as d ev value style t'
            | None -> (
                match map.dec_string with
                | Some t' -> decode_scalar_as d ev value style t'
                | None -> type_err Jsont.Sort.Number))
        | None -> (
            (* Plain scalar that's not bool/number -> string *)
            match map.dec_string with
            | Some t' -> decode_scalar_as d ev value style t'
            | None -> type_err Jsont.Sort.String))
  else
    (* Quoted scalars are strings *)
    match map.dec_string with
    | Some t' -> decode_scalar_as d ev value style t'
    | None -> type_err Jsont.Sort.String

and decode_alias : type a. decoder -> Event.spanned -> string -> a t -> a =
 fun d ev anchor t ->
  check_nodes d;
  match Hashtbl.find_opt d._anchors anchor with
  | None ->
      let meta = meta_of_span d ev.span in
      err_msg meta "Unknown anchor: %s" anchor
  | Some json_value -> (
      (* Decode the stored JSON value through the type *)
      let t' = Jsont.Repr.unsafe_to_t t in
      match Jsont.Json.decode' t' json_value with
      | Ok v -> v
      | Error e -> raise (Jsont.Error e))

and decode_array : type a elt b.
    decoder -> nest:int -> Event.spanned -> (a, elt, b) array_map -> a =
 fun d ~nest start_ev array_map ->
  skip_event d;
  (* consume Sequence_start *)
  check_nodes d;
  let meta = meta_of_span d start_ev.span in
  let builder = ref (array_map.dec_empty ()) in
  let idx = ref 0 in
  let rec loop () =
    match peek_event d with
    | Some { Event.event = Event.Sequence_end; span } ->
        skip_event d;
        let end_meta = meta_of_span d span in
        array_map.dec_finish end_meta !idx !builder
    | Some _ ->
        let i = !idx in
        (try
           if array_map.dec_skip i !builder then begin
             (* Skip this element by decoding as ignore *)
             let _ : unit =
               decode d ~nest:(nest + 1) (Jsont.Repr.of_t Jsont.ignore)
             in
             ()
           end
           else begin
             let elt = decode d ~nest:(nest + 1) array_map.elt in
             builder := array_map.dec_add i elt !builder
           end
         with Jsont.Error e ->
           Jsont.Repr.error_push_array meta array_map (i, Jsont.Meta.none) e);
        incr idx;
        loop ()
    | None -> err_msg meta "Unclosed sequence"
  in
  loop ()

and decode_any_sequence : type a.
    decoder -> nest:int -> Event.spanned -> a t -> a any_map -> a =
 fun d ~nest ev t map ->
  match map.dec_array with
  | Some t' -> (
      (* The t' decoder might be wrapped (e.g., Map for option types)
         Directly decode the array and let the wrapper handle it *)
      match t' with
      | Array array_map -> decode_array d ~nest ev array_map
      | _ ->
          (* For wrapped types like Map (Array ...), use full decode *)
          decode d ~nest t')
  | None ->
      Jsont.Repr.type_error (meta_of_span d ev.span) t ~fnd:Jsont.Sort.Array

and decode_object : type o.
    decoder -> nest:int -> Event.spanned -> (o, o) object_map -> o =
 fun d ~nest start_ev map ->
  skip_event d;
  (* consume Mapping_start *)
  check_nodes d;
  let meta = meta_of_span d start_ev.span in
  let dict =
    decode_object_members d ~nest meta map String_map.empty Dict.empty
  in
  let dict = Dict.add object_meta_arg meta dict in
  apply_dict map.dec dict

and decode_object_members : type o.
    decoder ->
    nest:int ->
    Jsont.Meta.t ->
    (o, o) object_map ->
    mem_dec String_map.t ->
    Dict.t ->
    Dict.t =
 fun d ~nest obj_meta map mem_miss dict ->
  (* Merge expected member decoders *)
  let u _ _ _ = assert false in
  let mem_miss = String_map.union u mem_miss map.mem_decs in
  match map.shape with
  | Object_basic umems ->
      decode_object_basic d ~nest obj_meta map umems mem_miss dict
  | Object_cases (umems_opt, cases) ->
      (* Wrap umems_opt to hide existential types *)
      let umems = Unknown_mems umems_opt in
      decode_object_cases d ~nest obj_meta map umems cases mem_miss [] dict

and init_unknown_builder : type o mems builder.
    (o, mems, builder) unknown_mems -> builder =
  function
  | Unknown_skip -> ()
  | Unknown_error -> ()
  | Unknown_keep (mmap, _) -> mmap.dec_empty ()

and decode_object_basic : type o mems builder.
    decoder ->
    nest:int ->
    Jsont.Meta.t ->
    (o, o) object_map ->
    (o, mems, builder) unknown_mems ->
    mem_dec String_map.t ->
    Dict.t ->
    Dict.t =
 fun d ~nest obj_meta object_map umems mem_miss dict ->
  let ubuilder = ref (init_unknown_builder umems) in
  let mem_miss = ref mem_miss in
  let dict = ref dict in
  let rec loop () =
    match peek_event d with
    | Some { Event.event = Event.Mapping_end; _ } ->
        skip_event d;
        (* Finalize *)
        finish_object obj_meta object_map umems !ubuilder !mem_miss !dict
    | Some ev ->
        (* Expect a scalar key *)
        let name, name_meta = decode_mapping_key d ev in
        (* Look up member decoder *)
        (match String_map.find_opt name object_map.mem_decs with
        | Some (Mem_dec mem) -> (
            mem_miss := String_map.remove name !mem_miss;
            try
              let v = decode d ~nest:(nest + 1) mem.type' in
              dict := Dict.add mem.id v !dict
            with Jsont.Error e ->
              Jsont.Repr.error_push_object obj_meta object_map (name, name_meta)
                e)
        | None -> (
            (* Unknown member *)
            match umems with
            | Unknown_skip ->
                let _ : unit =
                  decode d ~nest:(nest + 1) (Jsont.Repr.of_t Jsont.ignore)
                in
                ()
            | Unknown_error ->
                Jsont.Repr.unexpected_mems_error obj_meta object_map
                  ~fnd:[ (name, name_meta) ]
            | Unknown_keep (mmap, _) -> (
                try
                  let v = decode d ~nest:(nest + 1) mmap.mems_type in
                  ubuilder := mmap.dec_add name_meta name v !ubuilder
                with Jsont.Error e ->
                  Jsont.Repr.error_push_object obj_meta object_map
                    (name, name_meta) e)));
        loop ()
    | None -> err_msg obj_meta "Unclosed mapping"
  in
  loop ()

and finish_object : type o mems builder.
    Jsont.Meta.t ->
    (o, o) object_map ->
    (o, mems, builder) unknown_mems ->
    builder ->
    mem_dec String_map.t ->
    Dict.t ->
    Dict.t =
 fun meta map umems ubuilder mem_miss dict ->
  let open Jsont.Repr in
  let dict = Dict.add object_meta_arg meta dict in
  let dict =
    match umems with
    | Unknown_skip | Unknown_error -> dict
    | Unknown_keep (mmap, _) ->
        Dict.add mmap.id (mmap.dec_finish meta ubuilder) dict
  in
  (* Check for missing required members *)
  let add_default _ (Mem_dec mem_map) dict =
    match mem_map.dec_absent with
    | Some v -> Dict.add mem_map.id v dict
    | None -> raise Exit
  in
  try String_map.fold add_default mem_miss dict
  with Exit ->
    let no_default _ (Mem_dec mm) = Option.is_none mm.dec_absent in
    let exp = String_map.filter no_default mem_miss in
    missing_mems_error meta map ~exp ~fnd:[]

and decode_object_cases : type o cases tag.
    decoder ->
    nest:int ->
    Jsont.Meta.t ->
    (o, o) object_map ->
    unknown_mems_option ->
    (o, cases, tag) object_cases ->
    mem_dec String_map.t ->
    (Jsont.name * Jsont.json) list ->
    Dict.t ->
    Dict.t =
 fun d ~nest obj_meta object_map umems cases mem_miss delayed dict ->
  match peek_event d with
  | Some { Event.event = Event.Mapping_end; _ } -> (
      skip_event d;
      (* No tag found - use dec_absent if available *)
      match cases.tag.dec_absent with
      | Some tag ->
          decode_with_case_tag d ~nest obj_meta object_map umems cases tag
            mem_miss delayed dict
      | None ->
          (* Missing required case tag *)
          let exp = String_map.singleton cases.tag.name (Mem_dec cases.tag) in
          let fnd = List.map (fun ((n, _), _) -> n) delayed in
          Jsont.Repr.missing_mems_error obj_meta object_map ~exp ~fnd)
  | Some ev ->
      let name, name_meta = decode_mapping_key d ev in
      if String.equal name cases.tag.name then begin
        (* Found the case tag *)
        let tag = decode d ~nest:(nest + 1) cases.tag.type' in
        decode_with_case_tag d ~nest obj_meta object_map umems cases tag
          mem_miss delayed dict
      end
      else begin
        (* Not the case tag - check if known member or delay *)
        match String_map.find_opt name object_map.mem_decs with
        | Some (Mem_dec mem) -> (
            let mem_miss = String_map.remove name mem_miss in
            try
              let v = decode d ~nest:(nest + 1) mem.type' in
              let dict = Dict.add mem.id v dict in
              decode_object_cases d ~nest obj_meta object_map umems cases
                mem_miss delayed dict
            with Jsont.Error e ->
              Jsont.Repr.error_push_object obj_meta object_map (name, name_meta)
                e)
        | None ->
            (* Unknown member - decode as generic JSON and delay *)
            let v = decode d ~nest:(nest + 1) (Jsont.Repr.of_t Jsont.json) in
            let delayed = ((name, name_meta), v) :: delayed in
            decode_object_cases d ~nest obj_meta object_map umems cases mem_miss
              delayed dict
      end
  | None -> err_msg obj_meta "Unclosed mapping"

and decode_with_case_tag : type o cases tag.
    decoder ->
    nest:int ->
    Jsont.Meta.t ->
    (o, o) object_map ->
    unknown_mems_option ->
    (o, cases, tag) object_cases ->
    tag ->
    mem_dec String_map.t ->
    (Jsont.name * Jsont.json) list ->
    Dict.t ->
    Dict.t =
 fun d ~nest obj_meta map umems cases tag mem_miss delayed dict ->
  let open Jsont.Repr in
  let eq_tag (Case c) = cases.tag_compare c.tag tag = 0 in
  match List.find_opt eq_tag cases.cases with
  | None -> unexpected_case_tag_error obj_meta map cases tag
  | Some (Case case) ->
      (* Continue decoding with the case's object map *)
      let case_dict =
        decode_case_remaining d ~nest obj_meta case.object_map umems mem_miss
          delayed dict
      in
      let case_value = apply_dict case.object_map.dec case_dict in
      Dict.add cases.id (case.dec case_value) dict

and decode_case_remaining : type o.
    decoder ->
    nest:int ->
    Jsont.Meta.t ->
    (o, o) object_map ->
    unknown_mems_option ->
    mem_dec String_map.t ->
    (Jsont.name * Jsont.json) list ->
    Dict.t ->
    Dict.t =
 fun d ~nest obj_meta case_map _umems mem_miss delayed dict ->
  (* First, process delayed members against the case map *)
  let u _ _ _ = assert false in
  let mem_miss = String_map.union u mem_miss case_map.mem_decs in
  let dict, mem_miss =
    List.fold_left
      (fun (dict, mem_miss) ((name, meta), json_value) ->
        match String_map.find_opt name case_map.mem_decs with
        | Some (Mem_dec mem) -> (
            let t' = Jsont.Repr.unsafe_to_t mem.type' in
            match Jsont.Json.decode' t' json_value with
            | Ok v ->
                let dict = Dict.add mem.id v dict in
                let mem_miss = String_map.remove name mem_miss in
                (dict, mem_miss)
            | Error e ->
                Jsont.Repr.error_push_object obj_meta case_map (name, meta) e)
        | None ->
            (* Unknown for case too - skip them *)
            (dict, mem_miss))
      (dict, mem_miss) delayed
  in
  (* Then continue reading remaining members using case's own unknown handling *)
  match case_map.shape with
  | Object_basic case_umems ->
      decode_object_basic d ~nest obj_meta case_map case_umems mem_miss dict
  | Object_cases _ ->
      (* Nested cases shouldn't happen - use skip for safety *)
      decode_object_basic d ~nest obj_meta case_map Unknown_skip mem_miss dict

and decode_any_mapping : type a.
    decoder -> nest:int -> Event.spanned -> a t -> a any_map -> a =
 fun d ~nest ev t map ->
  match map.dec_object with
  | Some t' -> decode d ~nest t'
  | None ->
      Jsont.Repr.type_error (meta_of_span d ev.span) t ~fnd:Jsont.Sort.Object

and decode_mapping_key : decoder -> Event.spanned -> string * Jsont.Meta.t =
 fun d ev ->
  match ev.Event.event with
  | Event.Scalar { value; _ } ->
      skip_event d;
      let meta = meta_of_span d ev.span in
      (value, meta)
  | _ ->
      let meta = meta_of_span d ev.span in
      err_msg meta "Mapping keys must be scalars (strings), found %a" Event.pp
        ev.event

(* Skip stream/document wrappers *)
let skip_to_content d =
  let rec loop () =
    match peek_event d with
    | Some { Event.event = Event.Stream_start _; _ } ->
        skip_event d;
        loop ()
    | Some { Event.event = Event.Document_start _; _ } ->
        skip_event d;
        loop ()
    | _ -> ()
  in
  loop ()

let skip_end_wrappers d =
  let rec loop () =
    match peek_event d with
    | Some { Event.event = Event.Document_end _; _ } ->
        skip_event d;
        loop ()
    | Some { Event.event = Event.Stream_end; _ } ->
        skip_event d;
        loop ()
    | None -> ()
    | Some ev ->
        let meta = meta_of_span d ev.span in
        err_msg meta "Expected end of document but found %a" Event.pp ev.event
  in
  loop ()

(* Skip to the end of the current document after an error *)
let skip_to_document_end d =
  let rec loop depth =
    match peek_event d with
    | None -> ()
    | Some { Event.event = Event.Stream_end; _ } -> ()
    | Some { Event.event = Event.Document_end _; _ } ->
        skip_event d;
        if depth = 0 then () else loop (depth - 1)
    | Some { Event.event = Event.Document_start _; _ } ->
        skip_event d;
        loop (depth + 1)
    | Some _ ->
        skip_event d;
        loop depth
  in
  loop 0

(* Public decode API *)

(* Decode all documents from a multi-document YAML stream *)
let decode_all' ?(layout = false) ?(locs = false) ?(file = "-")
    ?(max_depth = 100) ?(max_nodes = 10_000_000) t reader =
  let parser = Parser.of_reader reader in
  let d = make_decoder ~layout ~locs ~file ~max_depth ~max_nodes parser in
  let t' = Jsont.Repr.of_t t in
  let rec next_doc () =
    match peek_event d with
    | None -> Seq.Nil
    | Some { Event.event = Event.Stream_end; _ } ->
        skip_event d;
        Seq.Nil
    | Some _ -> (
        try
          skip_to_content d;
          (* Reset node count for each document *)
          d.node_count <- 0;
          let v = decode d ~nest:0 t' in
          (* Skip document end marker if present *)
          (match peek_event d with
          | Some { Event.event = Event.Document_end _; _ } -> skip_event d
          | _ -> ());
          Seq.Cons (Ok v, next_doc)
        with
        | Jsont.Error e ->
            skip_to_document_end d;
            Seq.Cons (Error e, next_doc)
        | Error.Yamlrw_error err ->
            skip_to_document_end d;
            let msg = Error.to_string err in
            let e = Jsont.(Error.make_msg Error.Context.empty Meta.none msg) in
            Seq.Cons (Error e, next_doc))
  in
  next_doc

let decode_all ?layout ?locs ?file ?max_depth ?max_nodes t reader =
  decode_all' ?layout ?locs ?file ?max_depth ?max_nodes t reader
  |> Seq.map (Result.map_error Jsont.Error.to_string)

let decode' ?layout ?locs ?file ?max_depth ?max_nodes t reader =
  let parser = Parser.of_reader reader in
  let d = make_decoder ?layout ?locs ?file ?max_depth ?max_nodes parser in
  try
    skip_to_content d;
    let t' = Jsont.Repr.of_t t in
    let v = decode d ~nest:0 t' in
    skip_end_wrappers d;
    Ok v
  with
  | Jsont.Error e -> Error e
  | Error.Yamlrw_error err ->
      let msg = Error.to_string err in
      Error Jsont.(Error.make_msg Error.Context.empty Meta.none msg)

let decode ?layout ?locs ?file ?max_depth ?max_nodes t reader =
  Result.map_error Jsont.Error.to_string
    (decode' ?layout ?locs ?file ?max_depth ?max_nodes t reader)

let decode_string ?layout ?locs ?file ?max_depth ?max_nodes t s =
  let reader = Bytesrw.Bytes.Reader.of_string s in
  decode ?layout ?locs ?file ?max_depth ?max_nodes t reader

(* Convert Yamlrw.value to Jsont.json for type-driven decoding *)
let rec value_to_json (v : Yamlrw.value) : Jsont.json =
  let meta = Jsont.Meta.none in
  match v with
  | `Null -> Jsont.Null ((), meta)
  | `Bool b -> Jsont.Bool (b, meta)
  | `Float f -> Jsont.Number (f, meta)
  | `String s -> Jsont.String (s, meta)
  | `A items -> Jsont.Array (List.map value_to_json items, meta)
  | `O fields ->
      let mems = List.map (fun (k, v) -> ((k, meta), value_to_json v)) fields in
      Jsont.Object (mems, meta)

let decode_value' t v =
  let json = value_to_json v in
  Jsont.Json.decode' t json

let decode_value t v =
  Result.map_error Jsont.Error.to_string (decode_value' t v)

(* Encoder *)

type encoder = {
  emitter : Emitter.t;
  format : yaml_format;
  _indent : int; (* Stored for future use in custom formatting *)
  explicit_doc : bool;
  scalar_style : Scalar_style.t;
}

let make_encoder ?(format = Block) ?(indent = 2) ?(explicit_doc = false)
    ?(scalar_style = `Any) emitter =
  { emitter; format; _indent = indent; explicit_doc; scalar_style }

let layout_style_of_format = function
  | Block -> `Block
  | Flow -> `Flow
  | Layout -> `Any

(* Choose appropriate scalar style for a string *)
let choose_scalar_style ~preferred s =
  if preferred <> `Any then preferred
  else if String.contains s '\n' then `Literal
  else if String.length s > 80 then `Folded
  else `Plain

(* Helper to create scalar events with common defaults *)
let scalar_event ?(anchor = None) ?(tag = None) ~value ~style () =
  Event.Scalar
    { anchor; tag; value; plain_implicit = true; quoted_implicit = true; style }

(* Helper to emit events *)
let emit e = Emitter.emit e.emitter

(* Encode null *)
let encode_null e _meta = emit e (scalar_event ~value:"null" ~style:`Plain ())

(* Encode boolean *)
let encode_bool e _meta b =
  emit e (scalar_event ~value:(if b then "true" else "false") ~style:`Plain ())

(* Encode number *)
let encode_number e _meta f =
  let value =
    match Float.classify_float f with
    | FP_nan -> ".nan"
    | FP_infinite -> if f > 0.0 then ".inf" else "-.inf"
    | _ ->
        if Float.is_integer f && Float.abs f < 1e15 then Printf.sprintf "%.0f" f
        else Printf.sprintf "%g" f
  in
  emit e (scalar_event ~value ~style:`Plain ())

(* Encode string *)
let encode_string e _meta s =
  let style = choose_scalar_style ~preferred:e.scalar_style s in
  emit e (scalar_event ~value:s ~style ())

let rec encode : type a. encoder -> a t -> a -> unit =
 fun e t v ->
  match t with
  | Null map ->
      let meta = map.enc_meta v in
      let () = map.enc v in
      encode_null e meta
  | Bool map ->
      let meta = map.enc_meta v in
      let b = map.enc v in
      encode_bool e meta b
  | Number map ->
      let meta = map.enc_meta v in
      let f = map.enc v in
      encode_number e meta f
  | String map ->
      let meta = map.enc_meta v in
      let s = map.enc v in
      encode_string e meta s
  | Array map -> encode_array e map v
  | Object map -> encode_object e map v
  | Any map ->
      let t' = map.enc v in
      encode e t' v
  | Map m -> encode e m.dom (m.enc v)
  | Rec lazy_t -> encode e (Lazy.force lazy_t) v

and encode_array : type a elt b. encoder -> (a, elt, b) array_map -> a -> unit =
 fun e map v ->
  let style = layout_style_of_format e.format in
  emit e
    (Event.Sequence_start { anchor = None; tag = None; implicit = true; style });
  let _ =
    map.enc
      (fun () _idx elt ->
        encode e map.elt elt;
        ())
      () v
  in
  emit e Event.Sequence_end

and encode_object : type o. encoder -> (o, o) object_map -> o -> unit =
 fun e map v ->
  let style = layout_style_of_format e.format in
  emit e
    (Event.Mapping_start { anchor = None; tag = None; implicit = true; style });
  (* Encode each member *)
  List.iter
    (fun (Mem_enc mem) ->
      let mem_v = mem.enc v in
      if not (mem.enc_omit mem_v) then begin
        (* Emit key *)
        emit e (scalar_event ~value:mem.name ~style:`Plain ());
        (* Emit value *)
        encode e mem.type' mem_v
      end)
    map.mem_encs;
  (* Handle unknown members (for as_string_map objects) *)
  (match map.shape with
  | Object_basic (Unknown_keep (mmap, enc_fn)) ->
      let mems = enc_fn v in
      let _acc : unit =
        mmap.enc
          (fun _meta name mem_v () ->
            (* Emit key *)
            emit e (scalar_event ~value:name ~style:`Plain ());
            (* Emit value *)
            encode e mmap.mems_type mem_v)
          mems ()
      in
      ()
  | Object_basic (Unknown_skip | Unknown_error) -> ()
  | Object_cases (_, cases) ->
      let (Case_value (case_map, case_v)) = cases.enc_case (cases.enc v) in
      (* Emit case tag *)
      if not (cases.tag.enc_omit case_map.tag) then begin
        emit e (scalar_event ~value:cases.tag.name ~style:`Plain ());
        encode e cases.tag.type' case_map.tag
      end;
      (* Emit case members *)
      List.iter
        (fun (Mem_enc mem) ->
          let mem_v = mem.enc case_v in
          if not (mem.enc_omit mem_v) then begin
            emit e (scalar_event ~value:mem.name ~style:`Plain ());
            encode e mem.type' mem_v
          end)
        case_map.object_map.mem_encs);
  emit e Event.Mapping_end

(* Public encode API *)

let encode' ?buf:_ ?format ?indent ?explicit_doc ?scalar_style t v ~eod writer =
  let config =
    {
      Emitter.default_config with
      indent = Option.value ~default:2 indent;
      layout_style = (match format with Some Flow -> `Flow | _ -> `Block);
    }
  in
  let emitter = Emitter.of_writer ~config writer in
  let e = make_encoder ?format ?indent ?explicit_doc ?scalar_style emitter in
  try
    emit e (Event.Stream_start { encoding = `Utf8 });
    emit e
      (Event.Document_start { version = None; implicit = not e.explicit_doc });
    let t' = Jsont.Repr.of_t t in
    encode e t' v;
    emit e (Event.Document_end { implicit = not e.explicit_doc });
    emit e Event.Stream_end;
    if eod then Emitter.flush e.emitter;
    Ok ()
  with
  | Jsont.Error err -> Error err
  | Error.Yamlrw_error err ->
      let msg = Error.to_string err in
      Error Jsont.(Error.make_msg Error.Context.empty Meta.none msg)

let encode ?buf ?format ?indent ?explicit_doc ?scalar_style t v ~eod writer =
  Result.map_error Jsont.Error.to_string
    (encode' ?buf ?format ?indent ?explicit_doc ?scalar_style t v ~eod writer)

(* Recode *)

let recode ?layout ?locs ?file ?max_depth ?max_nodes ?buf ?format ?indent
    ?explicit_doc ?scalar_style t reader writer ~eod =
  let format =
    match (layout, format) with Some true, None -> Some Layout | _, f -> f
  in
  let layout =
    match (layout, format) with None, Some Layout -> Some true | l, _ -> l
  in
  match decode' ?layout ?locs ?file ?max_depth ?max_nodes t reader with
  | Ok v ->
      encode ?buf ?format ?indent ?explicit_doc ?scalar_style t v ~eod writer
  | Error e -> Error (Jsont.Error.to_string e)
