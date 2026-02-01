(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

type string_spec = {
  format : string option;
  min_length : int option;
  max_length : int option;
  min_graphemes : int option;
  max_graphemes : int option;
  known_values : string list option;
  enum : string list option;
  const : string option;
  default : string option;
  description : string option;
}

type integer_spec = {
  minimum : int option;
  maximum : int option;
  enum : int list option;
  const : int option;
  default : int option;
  description : string option;
}

type boolean_spec = {
  const : bool option;
  default : bool option;
  description : string option;
}

type bytes_spec = {
  min_length : int option;
  max_length : int option;
  description : string option;
}

type blob_spec = {
  accept : string list option;
  max_size : int option;
  description : string option;
}

type cid_link_spec = { description : string option }

type array_spec = {
  items : type_def;
  min_length : int option;
  max_length : int option;
  description : string option;
}

and property = { type_def : type_def; description : string option }

and object_spec = {
  properties : (string * property) list;
  required : string list option;
  nullable : string list option;
  description : string option;
}

and ref_spec = {
  ref_ : string; (* e.g., "#localDef" or "com.example.defs#someDef" *)
  description : string option;
}

and union_spec = {
  refs : string list;
  closed : bool option;
  description : string option;
}

and token_spec = { description : string option }
and unknown_spec = { description : string option }

and params_spec = {
  properties : (string * property) list;
  required : string list option;
  description : string option;
}

and body_def = {
  encoding : string;
  schema : type_def option;
  description : string option;
}

and error_def = { name : string; description : string option }

and query_spec = {
  parameters : params_spec option;
  output : body_def option;
  errors : error_def list option;
  description : string option;
}

and procedure_spec = {
  parameters : params_spec option;
  input : body_def option;
  output : body_def option;
  errors : error_def list option;
  description : string option;
}

and subscription_spec = {
  parameters : params_spec option;
  message : body_def option;
  errors : error_def list option;
  description : string option;
}

and record_spec = {
  key : string; (* "tid", "nsid", etc. *)
  record : object_spec;
  description : string option;
}

and permission_spec = {
  resource : string; (* "rpc" or "repo" *)
  inherit_aud : bool option;
  lxm : string list option (* for rpc permissions *);
  action :
    string list option (* for repo permissions: "create", "update", "delete" *);
  collection : string list option (* for repo permissions *);
}

and permission_set_spec = {
  title : string;
  detail : string option;
  permissions : permission_spec list;
  description : string option;
}

and type_def =
  | String of string_spec
  | Integer of integer_spec
  | Boolean of boolean_spec
  | Bytes of bytes_spec
  | Blob of blob_spec
  | CidLink of cid_link_spec
  | Array of array_spec
  | Object of object_spec
  | Ref of ref_spec
  | Union of union_spec
  | Token of token_spec
  | Unknown of unknown_spec
  | Query of query_spec
  | Procedure of procedure_spec
  | Subscription of subscription_spec
  | Record of record_spec
  | PermissionSet of permission_set_spec

type def_entry = { name : string; type_def : type_def }

type lexicon_doc = {
  lexicon : int; (* always 1 *)
  id : string; (* nsid *)
  revision : int option;
  description : string option;
  defs : def_entry list;
}

type parse_result = (lexicon_doc, string) result

(* Jsont codecs for all lexicon types *)

let string_spec_jsont : string_spec Jsont.t =
  let make format min_length max_length min_graphemes max_graphemes known_values
      enum const default description : string_spec =
    {
      format;
      min_length;
      max_length;
      min_graphemes;
      max_graphemes;
      known_values;
      enum;
      const;
      default;
      description;
    }
  in
  let map =
    Jsont.Object.map ~kind:"string_spec" make
    |> Jsont.Object.opt_mem "format" Jsont.string
         ~enc:(fun (s : string_spec) -> s.format)
    |> Jsont.Object.opt_mem "minLength" Jsont.int
         ~enc:(fun (s : string_spec) -> s.min_length)
    |> Jsont.Object.opt_mem "maxLength" Jsont.int
         ~enc:(fun (s : string_spec) -> s.max_length)
    |> Jsont.Object.opt_mem "minGraphemes" Jsont.int
         ~enc:(fun (s : string_spec) -> s.min_graphemes)
    |> Jsont.Object.opt_mem "maxGraphemes" Jsont.int
         ~enc:(fun (s : string_spec) -> s.max_graphemes)
    |> Jsont.Object.opt_mem "knownValues" Jsont.(list string)
         ~enc:(fun (s : string_spec) -> s.known_values)
    |> Jsont.Object.opt_mem "enum" Jsont.(list string)
         ~enc:(fun (s : string_spec) -> s.enum)
    |> Jsont.Object.opt_mem "const" Jsont.string
         ~enc:(fun (s : string_spec) -> s.const)
    |> Jsont.Object.opt_mem "default" Jsont.string
         ~enc:(fun (s : string_spec) -> s.default)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : string_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let integer_spec_jsont : integer_spec Jsont.t =
  let make minimum maximum enum const default description : integer_spec =
    { minimum; maximum; enum; const; default; description }
  in
  let map =
    Jsont.Object.map ~kind:"integer_spec" make
    |> Jsont.Object.opt_mem "minimum" Jsont.int
         ~enc:(fun (s : integer_spec) -> s.minimum)
    |> Jsont.Object.opt_mem "maximum" Jsont.int
         ~enc:(fun (s : integer_spec) -> s.maximum)
    |> Jsont.Object.opt_mem "enum" Jsont.(list int)
         ~enc:(fun (s : integer_spec) -> s.enum)
    |> Jsont.Object.opt_mem "const" Jsont.int
         ~enc:(fun (s : integer_spec) -> s.const)
    |> Jsont.Object.opt_mem "default" Jsont.int
         ~enc:(fun (s : integer_spec) -> s.default)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : integer_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let boolean_spec_jsont : boolean_spec Jsont.t =
  let make const default description : boolean_spec =
    { const; default; description }
  in
  let map =
    Jsont.Object.map ~kind:"boolean_spec" make
    |> Jsont.Object.opt_mem "const" Jsont.bool
         ~enc:(fun (s : boolean_spec) -> s.const)
    |> Jsont.Object.opt_mem "default" Jsont.bool
         ~enc:(fun (s : boolean_spec) -> s.default)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : boolean_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let bytes_spec_jsont : bytes_spec Jsont.t =
  let make min_length max_length description : bytes_spec =
    { min_length; max_length; description }
  in
  let map =
    Jsont.Object.map ~kind:"bytes_spec" make
    |> Jsont.Object.opt_mem "minLength" Jsont.int
         ~enc:(fun (s : bytes_spec) -> s.min_length)
    |> Jsont.Object.opt_mem "maxLength" Jsont.int
         ~enc:(fun (s : bytes_spec) -> s.max_length)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : bytes_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let blob_spec_jsont : blob_spec Jsont.t =
  let make accept max_size description : blob_spec =
    { accept; max_size; description }
  in
  let map =
    Jsont.Object.map ~kind:"blob_spec" make
    |> Jsont.Object.opt_mem "accept" Jsont.(list string)
         ~enc:(fun (s : blob_spec) -> s.accept)
    |> Jsont.Object.opt_mem "maxSize" Jsont.int
         ~enc:(fun (s : blob_spec) -> s.max_size)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : blob_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let cid_link_spec_jsont : cid_link_spec Jsont.t =
  let make description : cid_link_spec = { description } in
  let map =
    Jsont.Object.map ~kind:"cid_link_spec" make
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : cid_link_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let ref_spec_jsont : ref_spec Jsont.t =
  let make ref_ description : ref_spec = { ref_; description } in
  let map =
    Jsont.Object.map ~kind:"ref_spec" make
    |> Jsont.Object.mem "ref" Jsont.string
         ~enc:(fun (s : ref_spec) -> s.ref_)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : ref_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let union_spec_jsont : union_spec Jsont.t =
  let make refs closed description : union_spec = { refs; closed; description } in
  let map =
    Jsont.Object.map ~kind:"union_spec" make
    |> Jsont.Object.mem "refs" Jsont.(list string)
         ~enc:(fun (s : union_spec) -> s.refs)
    |> Jsont.Object.opt_mem "closed" Jsont.bool
         ~enc:(fun (s : union_spec) -> s.closed)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : union_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let token_spec_jsont : token_spec Jsont.t =
  let make description : token_spec = { description } in
  let map =
    Jsont.Object.map ~kind:"token_spec" make
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : token_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let unknown_spec_jsont : unknown_spec Jsont.t =
  let make description : unknown_spec = { description } in
  let map =
    Jsont.Object.map ~kind:"unknown_spec" make
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : unknown_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

let error_def_jsont : error_def Jsont.t =
  let make name description : error_def = { name; description } in
  let map =
    Jsont.Object.map ~kind:"error_def" make
    |> Jsont.Object.mem "name" Jsont.string
         ~enc:(fun (s : error_def) -> s.name)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : error_def) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map

(* Recursive types using Jsont.rec' with lazy values *)

module StringMap = Map.Make(String)

(* Forward declarations for mutually recursive types *)
let rec type_def_jsont_lazy : type_def Jsont.t Lazy.t = lazy (
  (* Create case maps for each variant *)
  let string_case = Jsont.Object.Case.map "string" string_spec_jsont
      ~dec:(fun s -> String s) in
  let integer_case = Jsont.Object.Case.map "integer" integer_spec_jsont
      ~dec:(fun s -> Integer s) in
  let boolean_case = Jsont.Object.Case.map "boolean" boolean_spec_jsont
      ~dec:(fun s -> Boolean s) in
  let bytes_case = Jsont.Object.Case.map "bytes" bytes_spec_jsont
      ~dec:(fun s -> Bytes s) in
  let blob_case = Jsont.Object.Case.map "blob" blob_spec_jsont
      ~dec:(fun s -> Blob s) in
  let cid_link_case = Jsont.Object.Case.map "cid-link" cid_link_spec_jsont
      ~dec:(fun s -> CidLink s) in
  let array_case = Jsont.Object.Case.map "array" (Lazy.force array_spec_jsont_lazy)
      ~dec:(fun s -> Array s) in
  let object_case = Jsont.Object.Case.map "object" (Lazy.force object_spec_jsont_lazy)
      ~dec:(fun s -> Object s) in
  let ref_case = Jsont.Object.Case.map "ref" ref_spec_jsont
      ~dec:(fun s -> Ref s) in
  let union_case = Jsont.Object.Case.map "union" union_spec_jsont
      ~dec:(fun s -> Union s) in
  let token_case = Jsont.Object.Case.map "token" token_spec_jsont
      ~dec:(fun s -> Token s) in
  let unknown_case = Jsont.Object.Case.map "unknown" unknown_spec_jsont
      ~dec:(fun s -> Unknown s) in
  let query_case = Jsont.Object.Case.map "query" (Lazy.force query_spec_jsont_lazy)
      ~dec:(fun s -> Query s) in
  let procedure_case = Jsont.Object.Case.map "procedure" (Lazy.force procedure_spec_jsont_lazy)
      ~dec:(fun s -> Procedure s) in
  let subscription_case = Jsont.Object.Case.map "subscription" (Lazy.force subscription_spec_jsont_lazy)
      ~dec:(fun s -> Subscription s) in
  let record_case = Jsont.Object.Case.map "record" (Lazy.force record_spec_jsont_lazy)
      ~dec:(fun s -> Record s) in
  let permission_set_case = Jsont.Object.Case.map "permission-set" (Lazy.force permission_set_spec_jsont_lazy)
      ~dec:(fun s -> PermissionSet s) in
  (* Create enc_case function for encoding *)
  let enc_case = function
    | String s -> Jsont.Object.Case.value string_case s
    | Integer s -> Jsont.Object.Case.value integer_case s
    | Boolean s -> Jsont.Object.Case.value boolean_case s
    | Bytes s -> Jsont.Object.Case.value bytes_case s
    | Blob s -> Jsont.Object.Case.value blob_case s
    | CidLink s -> Jsont.Object.Case.value cid_link_case s
    | Array s -> Jsont.Object.Case.value array_case s
    | Object s -> Jsont.Object.Case.value object_case s
    | Ref s -> Jsont.Object.Case.value ref_case s
    | Union s -> Jsont.Object.Case.value union_case s
    | Token s -> Jsont.Object.Case.value token_case s
    | Unknown s -> Jsont.Object.Case.value unknown_case s
    | Query s -> Jsont.Object.Case.value query_case s
    | Procedure s -> Jsont.Object.Case.value procedure_case s
    | Subscription s -> Jsont.Object.Case.value subscription_case s
    | Record s -> Jsont.Object.Case.value record_case s
    | PermissionSet s -> Jsont.Object.Case.value permission_set_case s
  in
  (* List of all cases *)
  let cases = Jsont.Object.Case.[
    make string_case; make integer_case; make boolean_case; make bytes_case;
    make blob_case; make cid_link_case; make array_case; make object_case;
    make ref_case; make union_case; make token_case; make unknown_case;
    make query_case; make procedure_case; make subscription_case;
    make record_case; make permission_set_case
  ] in
  (* Build the type_def codec *)
  Jsont.Object.map ~kind:"type_def" Fun.id
  |> Jsont.Object.case_mem "type" Jsont.string ~enc:Fun.id ~enc_case cases
  |> Jsont.Object.finish
)

and property_jsont_lazy : property Jsont.t Lazy.t = lazy (
  let make type_def description : property = { type_def; description } in
  let map =
    Jsont.Object.map ~kind:"property" make
    |> Jsont.Object.mem "type_def" (Jsont.rec' type_def_jsont_lazy)
         ~enc:(fun (p : property) -> p.type_def)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (p : property) -> p.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

(* Extract description from a type_def. Each variant's spec contains the description. *)
and get_type_def_description (td : type_def) : string option =
  match td with
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

(* Helper to decode properties object as (string * property) list.
   Each property value contains type_def fields including description.
   We decode as type_def and extract the description from it.
   Note: Field order is alphabetical since JSON object keys have no defined order. *)
and properties_jsont_lazy : (string * property) list Jsont.t Lazy.t = lazy (
  let map_jsont = Jsont.Object.as_string_map (Jsont.rec' type_def_jsont_lazy) in
  Jsont.map
    ~kind:"properties"
    ~dec:(fun m ->
      StringMap.bindings m |> List.map (fun (name, type_def) ->
        let description = get_type_def_description type_def in
        (name, { type_def; description })))
    ~enc:(fun (props : (string * property) list) ->
      List.fold_left (fun m (name, (prop : property)) ->
        StringMap.add name prop.type_def m) StringMap.empty props)
    map_jsont
)

and object_spec_jsont_lazy : object_spec Jsont.t Lazy.t = lazy (
  let make properties required nullable description : object_spec =
    { properties; required; nullable; description }
  in
  let map =
    Jsont.Object.map ~kind:"object_spec" make
    |> Jsont.Object.mem "properties" (Jsont.rec' properties_jsont_lazy)
         ~dec_absent:[] ~enc:(fun (s : object_spec) -> s.properties)
    |> Jsont.Object.opt_mem "required" Jsont.(list string)
         ~enc:(fun (s : object_spec) -> s.required)
    |> Jsont.Object.opt_mem "nullable" Jsont.(list string)
         ~enc:(fun (s : object_spec) -> s.nullable)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : object_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and array_spec_jsont_lazy : array_spec Jsont.t Lazy.t = lazy (
  let make items min_length max_length description : array_spec =
    { items; min_length; max_length; description }
  in
  let map =
    Jsont.Object.map ~kind:"array_spec" make
    |> Jsont.Object.mem "items" (Jsont.rec' type_def_jsont_lazy)
         ~enc:(fun (s : array_spec) -> s.items)
    |> Jsont.Object.opt_mem "minLength" Jsont.int
         ~enc:(fun (s : array_spec) -> s.min_length)
    |> Jsont.Object.opt_mem "maxLength" Jsont.int
         ~enc:(fun (s : array_spec) -> s.max_length)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : array_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and params_spec_jsont_lazy : params_spec Jsont.t Lazy.t = lazy (
  let make properties required description : params_spec =
    { properties; required; description }
  in
  let map =
    Jsont.Object.map ~kind:"params_spec" make
    |> Jsont.Object.mem "properties" (Jsont.rec' properties_jsont_lazy)
         ~dec_absent:[] ~enc:(fun (s : params_spec) -> s.properties)
    |> Jsont.Object.opt_mem "required" Jsont.(list string)
         ~enc:(fun (s : params_spec) -> s.required)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : params_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and body_def_jsont_lazy : body_def Jsont.t Lazy.t = lazy (
  let make encoding schema description : body_def =
    { encoding; schema; description }
  in
  let map =
    Jsont.Object.map ~kind:"body_def" make
    |> Jsont.Object.mem "encoding" Jsont.string
         ~enc:(fun (s : body_def) -> s.encoding)
    |> Jsont.Object.opt_mem "schema" (Jsont.rec' type_def_jsont_lazy)
         ~enc:(fun (s : body_def) -> s.schema)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : body_def) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and query_spec_jsont_lazy : query_spec Jsont.t Lazy.t = lazy (
  let make parameters output errors description : query_spec =
    { parameters; output; errors; description }
  in
  let map =
    Jsont.Object.map ~kind:"query_spec" make
    |> Jsont.Object.opt_mem "parameters" (Jsont.rec' params_spec_jsont_lazy)
         ~enc:(fun (s : query_spec) -> s.parameters)
    |> Jsont.Object.opt_mem "output" (Jsont.rec' body_def_jsont_lazy)
         ~enc:(fun (s : query_spec) -> s.output)
    |> Jsont.Object.opt_mem "errors" Jsont.(list error_def_jsont)
         ~enc:(fun (s : query_spec) -> s.errors)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : query_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and procedure_spec_jsont_lazy : procedure_spec Jsont.t Lazy.t = lazy (
  let make parameters input output errors description : procedure_spec =
    { parameters; input; output; errors; description }
  in
  let map =
    Jsont.Object.map ~kind:"procedure_spec" make
    |> Jsont.Object.opt_mem "parameters" (Jsont.rec' params_spec_jsont_lazy)
         ~enc:(fun (s : procedure_spec) -> s.parameters)
    |> Jsont.Object.opt_mem "input" (Jsont.rec' body_def_jsont_lazy)
         ~enc:(fun (s : procedure_spec) -> s.input)
    |> Jsont.Object.opt_mem "output" (Jsont.rec' body_def_jsont_lazy)
         ~enc:(fun (s : procedure_spec) -> s.output)
    |> Jsont.Object.opt_mem "errors" Jsont.(list error_def_jsont)
         ~enc:(fun (s : procedure_spec) -> s.errors)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : procedure_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and subscription_spec_jsont_lazy : subscription_spec Jsont.t Lazy.t = lazy (
  let make parameters message errors description : subscription_spec =
    { parameters; message; errors; description }
  in
  let map =
    Jsont.Object.map ~kind:"subscription_spec" make
    |> Jsont.Object.opt_mem "parameters" (Jsont.rec' params_spec_jsont_lazy)
         ~enc:(fun (s : subscription_spec) -> s.parameters)
    |> Jsont.Object.opt_mem "message" (Jsont.rec' body_def_jsont_lazy)
         ~enc:(fun (s : subscription_spec) -> s.message)
    |> Jsont.Object.opt_mem "errors" Jsont.(list error_def_jsont)
         ~enc:(fun (s : subscription_spec) -> s.errors)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : subscription_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and record_spec_jsont_lazy : record_spec Jsont.t Lazy.t = lazy (
  let make key record description : record_spec = { key; record; description } in
  let map =
    Jsont.Object.map ~kind:"record_spec" make
    |> Jsont.Object.mem "key" Jsont.string
         ~enc:(fun (s : record_spec) -> s.key)
    |> Jsont.Object.mem "record" (Jsont.rec' object_spec_jsont_lazy)
         ~enc:(fun (s : record_spec) -> s.record)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : record_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and permission_spec_jsont_lazy : permission_spec Jsont.t Lazy.t = lazy (
  let make resource inherit_aud lxm action collection : permission_spec =
    { resource; inherit_aud; lxm; action; collection }
  in
  let map =
    Jsont.Object.map ~kind:"permission_spec" make
    |> Jsont.Object.mem "resource" Jsont.string
         ~enc:(fun (s : permission_spec) -> s.resource)
    |> Jsont.Object.opt_mem "inherit_aud" Jsont.bool
         ~enc:(fun (s : permission_spec) -> s.inherit_aud)
    |> Jsont.Object.opt_mem "lxm" Jsont.(list string)
         ~enc:(fun (s : permission_spec) -> s.lxm)
    |> Jsont.Object.opt_mem "action" Jsont.(list string)
         ~enc:(fun (s : permission_spec) -> s.action)
    |> Jsont.Object.opt_mem "collection" Jsont.(list string)
         ~enc:(fun (s : permission_spec) -> s.collection)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

and permission_set_spec_jsont_lazy : permission_set_spec Jsont.t Lazy.t = lazy (
  let make title detail permissions description : permission_set_spec =
    { title; detail; permissions; description }
  in
  let map =
    Jsont.Object.map ~kind:"permission_set_spec" make
    |> Jsont.Object.mem "title" Jsont.string
         ~enc:(fun (s : permission_set_spec) -> s.title)
    |> Jsont.Object.opt_mem "detail" Jsont.string
         ~enc:(fun (s : permission_set_spec) -> s.detail)
    |> Jsont.Object.mem "permissions" Jsont.(list (Jsont.rec' permission_spec_jsont_lazy))
         ~enc:(fun (s : permission_set_spec) -> s.permissions)
    |> Jsont.Object.opt_mem "description" Jsont.string
         ~enc:(fun (s : permission_set_spec) -> s.description)
    |> Jsont.Object.skip_unknown
  in
  Jsont.Object.finish map
)

(* Force lazy values to get the actual codecs *)
let type_def_jsont = Lazy.force type_def_jsont_lazy
let property_jsont = Lazy.force property_jsont_lazy
let object_spec_jsont = Lazy.force object_spec_jsont_lazy
let array_spec_jsont = Lazy.force array_spec_jsont_lazy
let params_spec_jsont = Lazy.force params_spec_jsont_lazy
let body_def_jsont = Lazy.force body_def_jsont_lazy
let query_spec_jsont = Lazy.force query_spec_jsont_lazy
let procedure_spec_jsont = Lazy.force procedure_spec_jsont_lazy
let subscription_spec_jsont = Lazy.force subscription_spec_jsont_lazy
let record_spec_jsont = Lazy.force record_spec_jsont_lazy

(* defs_jsont decodes an object {"name": type_def, ...} as a def_entry list *)
let defs_jsont : def_entry list Jsont.t =
  (* Decode as a string map of type_def values *)
  let map_jsont = Jsont.Object.as_string_map type_def_jsont in
  (* Wrap with conversion functions *)
  Jsont.map
    ~kind:"defs"
    ~dec:(fun m -> StringMap.fold (fun name type_def acc -> { name; type_def } :: acc) m [])
    ~enc:(fun defs -> List.fold_left (fun m d -> StringMap.add d.name d.type_def m) StringMap.empty defs)
    map_jsont

let def_entry_jsont : def_entry Jsont.t =
  let make name type_def = { name; type_def } in
  Jsont.Object.map ~kind:"def_entry" make
  |> Jsont.Object.mem "name" Jsont.string ~enc:(fun s -> s.name)
  |> Jsont.Object.mem "type_def" type_def_jsont ~enc:(fun s -> s.type_def)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish

let lexicon_doc_jsont : lexicon_doc Jsont.t =
  let make lexicon id revision description defs =
    { lexicon; id; revision; description; defs }
  in
  Jsont.Object.map ~kind:"lexicon_doc" make
  |> Jsont.Object.mem "lexicon" Jsont.int ~enc:(fun s -> s.lexicon)
  |> Jsont.Object.mem "id" Jsont.string ~enc:(fun s -> s.id)
  |> Jsont.Object.opt_mem "revision" Jsont.int ~enc:(fun s -> s.revision)
  |> Jsont.Object.opt_mem "description" Jsont.string ~enc:(fun s -> s.description)
  |> Jsont.Object.mem "defs" defs_jsont ~enc:(fun s -> s.defs)
  |> Jsont.Object.skip_unknown
  |> Jsont.Object.finish
