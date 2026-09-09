(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Codec combinators for TOML values. *)

module Meta = Loc.Meta

(* ---- Error helpers ---- *)

let type_name = function
  | Value.String _ -> "string"
  | Value.Int _ -> "integer"
  | Value.Float _ -> "float"
  | Value.Bool _ -> "boolean"
  | Value.Datetime _ -> "datetime"
  | Value.Datetime_local _ -> "datetime-local"
  | Value.Date_local _ -> "date-local"
  | Value.Time_local _ -> "time-local"
  | Value.Array _ -> "array"
  | Value.Table _ -> "table"

(* The value is what says where. Every node a codec is handed came out of the
   document and carries the bytes it was read from, so taking the place off the
   value itself is all it takes to put a line and a column under a decode
   error: the reader of "expected integer, got string" is then told which
   string, the way a parse error tells them which byte. Passing the value
   rather than a [?meta] is so that no raiser can be written without one. *)
let err_expected_got expected v =
  Error.failf (Value.meta v) "expected %s, got %s" expected (type_name v)

(* A mapping codec runs a function over what the inner codec decoded, and that
   function is handed the value and not the node it came from: the overflow
   check under {!int} and {!int32} is one, and every [dec] a caller passes to
   {!map} is another. An error out of one of them that names no place is given
   the place of the node it was decoding, so no error leaves this module
   without one. *)
let locate_at v f =
  try f ()
  with Loc.Error e when Loc.is_none (Meta.loc e.Loc.Error.meta) ->
    Error.raise ~ctx:e.Loc.Error.ctx ~meta:(Value.meta v) e.Loc.Error.kind

let push_mem_ctx kind_node name_node f =
  try f () with Loc.Error e -> Error.push_object kind_node name_node e

let push_nth_ctx kind_node idx_node f =
  try f () with Loc.Error e -> Error.push_array kind_node idx_node e

let table_kind_node kind =
  let k = Sort.kinded ~kind Sort.Table in
  (k, Meta.none)

let array_kind_node kind =
  let k = Sort.kinded ~kind Sort.Array in
  (k, Meta.none)

(* Sort label for error messages when a map-layer codec has no decoder or
   encoder installed. Users see e.g. "No decoder for <user-kind> map" after
   [Sort.kinded_string ~kind] prepends the user's [~kind] label. Mirrors json's
   [base_map_sort]. *)
let base_map_sort = "map"

(* ---- Resource limits ---- *)

type limits = {
  max_depth : int;
  max_nodes : int;
  mutable depth : int;
  mutable nodes : int;
}

let limits ?(max_depth = 100) ?(max_nodes = 10_000_000) () =
  { max_depth; max_nodes; depth = 0; nodes = 0 }

(* [v] is the node being decoded when the limit was reached, and so where the
   document stopped being one this decoder will read. *)
let limits_bump_node lim v =
  lim.nodes <- lim.nodes + 1;
  if lim.nodes > lim.max_nodes then
    Error.failf (Value.meta v) "max nodes exceeded (limit: %d)" lim.max_nodes

let limits_enter lim v =
  lim.depth <- lim.depth + 1;
  if lim.depth > lim.max_depth then
    Error.failf (Value.meta v) "max depth exceeded (limit: %d)" lim.max_depth

let limits_leave lim = lim.depth <- lim.depth - 1

(* ---- Codec type ----

   Pure GADT: every combinator is a native constructor. The only closures inside
   constructor payloads are user-supplied functions (e.g. [Map.dec],
   [Enum.cmp]), which are a legitimate part of the user's contribution to the
   codec. *)

type _ t =
  (* Scalars *)
  | Bool : bool t
  | Int64 : int64 t
  | Float : float t
  | String : string t
  (* List / isomorphism / labeling *)
  | List : 'a t -> 'a list t
  | Map : {
      inner : 'a t;
      dec : 'a -> 'b;
      enc : 'b -> 'a;
      kind : string;
      doc : string;
    }
      -> 'b t
  | Labeled : { kind : string; doc : string; inner : 'a t } -> 'a t
  (* Type-coercing scalars *)
  | Number : float t
  | Int_as_string : int t
  | Int64_as_string : int64 t
  (* Ptime datetime codecs *)
  | Ptime_any : {
      tz : unit -> int;
      now : (unit -> Ptime.t) option;
      frac_s : int;
    }
      -> Ptime.t t
  | Ptime_offset : { tz_offset_s : int; frac_s : int } -> Ptime.t t
  | Ptime_span : Ptime.Span.t t
  | Ptime_date : Ptime.date t
  | Ptime_full : { tz_offset_s : int option } -> Value.ptime_datetime t
  (* Degenerate / constant / recursive *)
  | Const : { value : 'a; kind : string; doc : string } -> 'a t
  | Enum : {
      cases : (string * 'a) list;
      cmp : 'a -> 'a -> int;
      kind : string;
      doc : string;
    }
      -> 'a t
  | Option : { inner : 'a t; kind : string; doc : string } -> 'a option t
  | Result : { ok : 'a t; error : 'b t } -> ('a, 'b) result t
  | Fix : 'a t Lazy.t -> 'a t
  | Iter : {
      inner : 'a t;
      dec : ('a -> unit) option;
      enc : ('a -> unit) option;
      kind : string;
      doc : string;
    }
      -> 'a t
  | Recode : { dec : 'a t; f : 'a -> 'b; enc : 'b t } -> 'b t
  | Uni_map : {
      inner : 'a t;
      dec : ('a -> 'b) option;
      enc : ('b -> 'a) option;
      kind : string;
      doc : string;
    }
      -> 'b t
  (* Query / update *)
  | Key : { name : string; inner : 'a t } -> 'a t
  | Update_key : { name : string; inner : 'a t } -> Value.t t
  | Delete_key : { name : string } -> Value.t t
  | Nth : { index : int; absent : 'a option; inner : 'a t } -> 'a t
  | Mem : { name : string; absent : 'a option; inner : 'a t } -> 'a t
  (* Folds *)
  | Fold_array : {
      inner : 'a t;
      f : int -> 'a -> 'acc -> 'acc;
      init : 'acc;
    }
      -> 'acc t
  | Fold_table : {
      inner : 'a t;
      f : string -> 'a -> 'acc -> 'acc;
      init : 'acc;
    }
      -> 'acc t
  (* Degenerate leaves *)
  | Ignore : unit t
  | Zero : unit t
  (* Raw value codecs *)
  | Identity : Value.t t
  | Value_mems : (string * Value.t) list t
  | Any : 'a any_spec -> 'a t
  (* Array builders *)
  | Array_builder : ('array, 'elt, 'builder) array_spec -> 'array t
  | Array_of_tables : { inner : 'a t; kind : string; doc : string } -> 'a list t
  (* Table builders *)
  | Table_obj : { spec : 'o table_spec; inline : bool } -> 'o t

and 'a any_spec = {
  any_kind : string;
  any_doc : string;
  dec_string : 'a t option;
  dec_int : 'a t option;
  dec_float : 'a t option;
  dec_bool : 'a t option;
  dec_datetime : 'a t option;
  dec_array : 'a t option;
  dec_table : 'a t option;
  any_enc : ('a -> 'a t) option;
}

and ('array, 'elt, 'builder) array_spec = {
  kind : string;
  doc : string;
  elt : 'elt t;
  dec_empty : unit -> 'builder;
  dec_add : 'elt -> 'builder -> 'builder;
  dec_finish : 'builder -> 'array;
  enc_fold : ('array, 'elt) array_fold;
}

and ('array, 'elt) array_fold = {
  fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc;
}

(* The table builder stores each member's value as a raw Value.t while decoding.
   The dec chain then decodes each of those values in turn, threading the
   appropriate context. *)
and ('o, 'dec) table_spec_raw = {
  kind : string;
  doc : string;
  members : ('o, Value.t) mem_spec list; (* reverse declaration order *)
  dec : limits -> table_input -> 'dec;
  unknown : unknown_handling;
  keep_unknown_enc : ('o -> (string * Value.t) list) option;
}

and 'o table_spec = ('o, 'o) table_spec_raw

(* Everything one decode of one table gives its dec chain: the members of that
   table, the subset of them no member spec claims, and where the table itself
   was. All three are built by the decode that is running and passed down the
   chain, so nothing about a decode in progress is reachable from the codec
   value. The place is there for the errors that are about the table rather
   than about a member of it: a member the caller required and the document
   does not have has no node of its own to be pointed at. *)
and table_input = {
  all_pairs : (string * Value.t) list;
  unknown_pairs : (string * Value.t) list;
  table_meta : Meta.t;
}

and unknown_handling = Skip | Error_on_unknown | Keep
and 'o mem_encoder = { enc : 'o -> Value.t; should_omit : 'o -> bool }

and ('o, 'a) mem_spec = {
  name : string;
  mem_doc : string;
  mem_codec : 'a t;
  dec_absent : 'a option;
  enc_typed : 'o mem_encoder option;
}
[@@warning "-37-69"]

(* ---- Interpreters ---- *)

let rec kind : type a. a t -> string = function
  | Bool -> "boolean"
  | Int64 -> "integer"
  | Float -> "float"
  | String -> "string"
  | List inner -> "list of " ^ kind inner
  | Map { kind = k; _ } -> k
  | Labeled { kind = k; _ } -> k
  | Number -> "number"
  | Int_as_string -> "integer (as string)"
  | Int64_as_string -> "int64 (as string)"
  | Ptime_any _ -> "datetime (ptime)"
  | Ptime_offset _ -> "datetime (ptime offset only)"
  | Ptime_span -> "time-local (ptime span)"
  | Ptime_date -> "date-local (ptime)"
  | Ptime_full _ -> "datetime (unified ptime)"
  | Const { kind = k; _ } -> k
  | Enum { kind = k; _ } -> k
  | Option { kind = k; _ } -> k
  | Result { ok; error } -> kind ok ^ " or " ^ kind error
  | Fix _ -> "recursive"
  | Iter { kind = k; _ } -> k
  | Recode { dec; _ } -> kind dec
  | Uni_map { kind = k; _ } -> k
  | Key { inner; _ } -> kind inner
  | Update_key _ -> "table"
  | Delete_key _ -> "table"
  | Nth { inner; _ } -> kind inner
  | Mem { inner; _ } -> kind inner
  | Fold_array _ -> "array"
  | Fold_table _ -> "table"
  | Ignore -> "ignored"
  | Zero -> "zero"
  | Identity -> "value"
  | Value_mems -> "value members"
  | Any { any_kind; _ } -> any_kind
  | Array_builder { kind; _ } -> kind
  | Array_of_tables { kind = k; _ } -> k
  | Table_obj { spec; _ } -> spec.kind

let rec doc : type a. a t -> string = function
  | Bool | Int64 | Float | String | Number | Int_as_string | Int64_as_string
  | Ptime_any _ | Ptime_offset _ | Ptime_span | Ptime_date | Ptime_full _
  | Ignore | Zero | Identity | Value_mems | Delete_key _ | Update_key _ ->
      ""
  | List inner -> doc inner
  | Map { doc = d; _ } -> d
  | Labeled { doc = d; _ } -> d
  | Const { doc = d; _ } -> d
  | Enum { doc = d; _ } -> d
  | Option { doc = d; _ } -> d
  | Result _ -> ""
  | Fix _ -> ""
  | Iter { doc = d; _ } -> d
  | Recode { dec; _ } -> doc dec
  | Uni_map { doc = d; _ } -> d
  | Key { inner; _ } -> doc inner
  | Nth { inner; _ } -> doc inner
  | Mem { inner; _ } -> doc inner
  | Fold_array _ -> ""
  | Fold_table _ -> ""
  | Any { any_doc; _ } -> any_doc
  | Array_builder { doc; _ } -> doc
  | Array_of_tables { doc = d; _ } -> d
  | Table_obj { spec; _ } -> spec.doc

let pp fmt c = Fmt.string fmt (kind c)

let with_doc ?kind:k ?doc:d c =
  let new_kind = Option.value ~default:(kind c) k in
  let new_doc = Option.value ~default:(doc c) d in
  Labeled { kind = new_kind; doc = new_doc; inner = c }

(* ---- Ptime helpers (used in Ptime_any decoder) ---- *)

let today_date ?now tz_offset_s =
  let t = Option.fold ~none:Ptime.epoch ~some:(fun f -> f ()) now in
  Ptime.to_date ~tz_offset_s t

let ptime_of_date ?(tz_offset_s = 0) (year, month, day) =
  match Ptime.of_date_time ((year, month, day), ((0, 0, 0), tz_offset_s)) with
  | Some t -> t
  | None -> Ptime.epoch

let ptime_of_time ?now ~tz_offset_s ~hour ~minute ~second ~ns () =
  let frac = Float.of_int ns /. 1_000_000_000.0 in
  let date = today_date ?now tz_offset_s in
  let time = ((hour, minute, second), tz_offset_s) in
  match Ptime.of_date_time (date, time) with
  | Some t -> (
      match Ptime.Span.of_float_s frac with
      | Some span -> Option.value ~default:t (Ptime.add_span t span)
      | None -> t)
  | None -> Ptime.epoch

(* ---- Ptime decoders (Ptime_any / Ptime_offset / Ptime_span / Ptime_full)
   ---- *)

let decode_ptime_any ~tz ~now (v : Value.t) : Ptime.t =
  let tz_s = tz () in
  match v with
  | Value.Datetime _ -> (
      match Value.to_ptime_opt v with
      | Some t -> t
      | None -> Error.fail (Value.meta v) "cannot parse offset datetime")
  | Value.Datetime_local _ -> (
      match Value.to_ptime_datetime ~tz_offset_s:tz_s v with
      | Some (`Datetime_local t) -> t
      | _ -> Error.fail (Value.meta v) "cannot parse local datetime")
  | Value.Date_local _ -> (
      match Value.to_date_opt v with
      | Some date -> ptime_of_date ~tz_offset_s:tz_s date
      | None -> Error.fail (Value.meta v) "cannot parse local date")
  | Value.Time_local _ -> (
      match Value.to_ptime_datetime ~tz_offset_s:tz_s v with
      | Some (`Time (h, m, s, ns)) ->
          ptime_of_time ?now ~tz_offset_s:tz_s ~hour:h ~minute:m ~second:s ~ns
            ()
      | _ -> Error.fail (Value.meta v) "cannot parse local time")
  | v -> err_expected_got "datetime" v

let decode_ptime_offset (v : Value.t) : Ptime.t =
  match v with
  | Value.Datetime _ -> (
      match Value.to_ptime_opt v with
      | Some t -> t
      | None -> Error.fail (Value.meta v) "cannot parse offset datetime")
  | Value.Datetime_local _ ->
      Error.fail (Value.meta v)
        "local datetime requires timezone; use ptime() instead"
  | Value.Date_local _ ->
      Error.fail (Value.meta v)
        "local date requires timezone; use ptime() instead"
  | Value.Time_local _ ->
      Error.fail (Value.meta v)
        "local time requires timezone; use ptime() instead"
  | v -> err_expected_got "datetime" v

let decode_ptime_span (v : Value.t) : Ptime.span =
  match v with
  | Value.Time_local _ -> (
      match Value.to_ptime_datetime v with
      | Some (`Time (h, m, s, ns)) -> (
          let total_secs = (h * 3600) + (m * 60) + s in
          let frac = Float.of_int ns /. 1_000_000_000.0 in
          match Ptime.Span.of_float_s (Float.of_int total_secs +. frac) with
          | Some span -> span
          | None -> Error.fail (Value.meta v) "cannot create span from time")
      | _ -> Error.fail (Value.meta v) "cannot parse local time")
  | v -> err_expected_got "time-local" v

let decode_ptime_date (v : Value.t) : Ptime.date =
  match v with
  | Value.Date_local _ -> (
      match Value.to_date_opt v with
      | Some d -> d
      | None -> Error.fail (Value.meta v) "cannot parse local date")
  | v -> err_expected_got "date-local" v

let decode_ptime_full ~tz_offset_s (v : Value.t) : Value.ptime_datetime =
  match Value.to_ptime_datetime ?tz_offset_s v with
  | Some pdt -> pdt
  | None -> (
      match v with
      | Value.Datetime _ | Value.Datetime_local _ | Value.Date_local _
      | Value.Time_local _ ->
          Error.fail (Value.meta v) "cannot parse datetime"
      | v -> err_expected_got "datetime" v)

let decode_string_as ~kind ~of_string lim (v : Value.t) =
  match v with
  | Value.String (s, _) -> (
      limits_bump_node lim v;
      match of_string s with
      | Some i -> i
      | None -> Error.failf (Value.meta v) "cannot parse %s: %s" kind s)
  | v -> err_expected_got "string" v

let decode_bool lim v =
  match v with
  | Value.Bool (b, _) ->
      limits_bump_node lim v;
      b
  | v -> err_expected_got "boolean" v

let decode_int64 lim v =
  match v with
  | Value.Int (i, _) ->
      limits_bump_node lim v;
      i
  | v -> err_expected_got "integer" v

let decode_float lim v =
  match v with
  | Value.Float (f, _) ->
      limits_bump_node lim v;
      f
  | v -> err_expected_got "float" v

let decode_string lim v =
  match v with
  | Value.String (s, _) ->
      limits_bump_node lim v;
      s
  | v -> err_expected_got "string" v

let decode_number lim v =
  match v with
  | Value.Float (f, _) ->
      limits_bump_node lim v;
      f
  | Value.Int (i, _) ->
      limits_bump_node lim v;
      Int64.to_float i
  | v -> err_expected_got "number" v

let decode_enum : type a. (string * a) list -> limits -> Value.t -> a =
 fun cases lim v ->
  match v with
  | Value.String (s, _) -> (
      limits_bump_node lim v;
      match List.assoc_opt s cases with
      | Some x -> x
      | None -> Error.failf (Value.meta v) "unknown enum value: %s" s)
  | v -> err_expected_got "string" v

(* The members of [plain_pairs] no member spec claims, as [spec.unknown] asks
   for them: [Skip] wants none, [Error_on_unknown] wants the first to fail the
   decode, [Keep] wants them all in document order. *)
let unknown_members : type o.
    o table_spec ->
    (string * Value.t) list ->
    string list ->
    (string * Value.t) list =
 fun spec plain_pairs known_names ->
  let is_unknown (name, _) = not (List.mem name known_names) in
  match spec.unknown with
  | Skip -> []
  | Error_on_unknown ->
      let tkn = (spec.kind, Meta.none) in
      List.iter
        (fun ((name, mv) as pair) ->
          if is_unknown pair then
            push_mem_ctx tkn (name, Meta.none) (fun () ->
                Error.failf (Value.meta mv) "unknown member: %s" name))
        plain_pairs;
      []
  | Keep -> List.filter is_unknown plain_pairs

let decode_table : type o. o table_spec -> limits -> Value.t -> o =
 fun spec lim v ->
  match v with
  | Value.Table (pairs, _) ->
      limits_bump_node lim v;
      limits_enter lim v;
      let plain_pairs = List.map (fun ((k, _), v) -> (k, v)) pairs in
      let members_ordered = List.rev spec.members in
      let known_names =
        List.filter_map
          (fun m -> if m.name = "" then None else Some m.name)
          members_ordered
      in
      let unknown_pairs = unknown_members spec plain_pairs known_names in
      let result =
        spec.dec lim
          { all_pairs = plain_pairs; unknown_pairs; table_meta = Value.meta v }
      in
      limits_leave lim;
      result
  | v -> err_expected_got "table" v

let encode_table_member : type o.
    o -> (o, Value.t) mem_spec -> (string * Value.t) option =
 fun o m ->
  match (m.name, m.enc_typed) with
  | "", _ | _, None -> None
  | name, Some enc_info ->
      if enc_info.should_omit o then None else Some (name, enc_info.enc o)

let encode_table : type o. o table_spec -> o -> Value.t =
 fun spec o ->
  let members_ordered = List.rev spec.members in
  let pairs = List.filter_map (encode_table_member o) members_ordered in
  let pairs =
    match spec.keep_unknown_enc with
    | None -> pairs
    | Some get_unknown -> pairs @ get_unknown o
  in
  Value.table pairs

let assoc_name_opt : type v.
    string -> ((string * Loc.Meta.t) * v) list -> v option =
 fun name pairs ->
  List.find_map (fun ((k, _), v) -> if k = name then Some v else None) pairs

let decode_int_as_string : limits -> Value.t -> int =
 fun lim v ->
  decode_string_as ~kind:"integer" ~of_string:int_of_string_opt lim v

let decode_int64_as_string : limits -> Value.t -> int64 =
 fun lim v ->
  decode_string_as ~kind:"int64" ~of_string:Int64.of_string_opt lim v

let decode_delete_key : name:string -> limits -> Value.t -> Value.t =
 fun ~name lim v ->
  match v with
  | Value.Table (pairs, m) ->
      limits_bump_node lim v;
      let pairs' = List.filter (fun ((k, _), _) -> k <> name) pairs in
      Value.Table (pairs', m)
  | v -> err_expected_got "table" v

let encode_ptime_span x =
  let secs = Ptime.Span.to_float_s x in
  let secs = Float.max 0.0 (Float.min secs 86399.999999999) in
  let total_secs = Float.to_int secs in
  let frac = secs -. Float.of_int total_secs in
  let h = total_secs / 3600 in
  let m = total_secs mod 3600 / 60 in
  let s = total_secs mod 60 in
  if frac > 0.0 then
    Fmt.kstr Value.time_local "%02d:%02d:%02d%s" h m s
      (String.sub (Fmt.str "%.9f" frac) 1 10)
  else Fmt.kstr Value.time_local "%02d:%02d:%02d" h m s

let encode_enum cases cmp x =
  let rev_cases = List.map (fun (s, v) -> (v, s)) cases in
  match List.find_opt (fun (v', _) -> cmp x v' = 0) rev_cases with
  | Some (_, s) -> Value.string s
  | None -> failwith "enum value not in association list"

let rec encode : type a. a t -> a -> Value.t =
 fun c x ->
  match c with
  | Bool -> Value.bool x
  | Int64 -> Value.int x
  | Float -> Value.float x
  | String -> Value.string x
  | List inner -> Value.array (List.map (encode inner) x)
  | Map { inner; enc; _ } -> encode inner (enc x)
  | Labeled { inner; _ } -> encode inner x
  | Number -> Value.float x
  | Int_as_string -> Value.string (Int.to_string x)
  | Int64_as_string -> Value.string (Int64.to_string x)
  | Ptime_any { tz; frac_s; _ } ->
      Value.datetime_of_ptime ~tz_offset_s:(tz ()) ~frac_s x
  | Ptime_offset { tz_offset_s; frac_s } ->
      Value.datetime_of_ptime ~tz_offset_s ~frac_s x
  | Ptime_span -> encode_ptime_span x
  | Ptime_date ->
      let year, month, day = x in
      Fmt.kstr Value.date_local "%04d-%02d-%02d" year month day
  | Ptime_full _ -> Value.toml_of_ptime_datetime x
  | Const _ -> Value.table []
  | Enum { cases; cmp; _ } -> encode_enum cases cmp x
  | Option { inner; _ } -> (
      match x with Some v -> encode inner v | None -> Value.table [])
  | Result { ok; error } -> (
      match x with Ok v -> encode ok v | Error v -> encode error v)
  | Fix lazy_c -> encode (Lazy.force lazy_c) x
  | Iter { inner; enc; _ } ->
      (match enc with Some f -> f x | None -> ());
      encode inner x
  | Recode { f = _; enc; _ } -> encode enc x
  | Uni_map { inner; enc; kind = k; _ } -> (
      match enc with
      | Some f -> encode inner (f x)
      | None ->
          Error.no_encoder Meta.none
            ~kind:(Sort.kinded_string ~kind:k base_map_sort))
  | Key { name; inner } -> Value.table [ (name, encode inner x) ]
  | Update_key _ -> x
  | Delete_key _ -> x
  | Nth { inner; _ } -> Value.array [ encode inner x ]
  | Mem { name; inner; _ } -> Value.table [ (name, encode inner x) ]
  | Fold_array _ -> Value.array []
  | Fold_table _ -> Value.table []
  | Ignore ->
      Error.no_encoder Meta.none
        ~kind:(Sort.kinded_string ~kind:"ignore" base_map_sort)
  | Zero -> Value.table []
  | Identity -> x
  | Value_mems -> Value.table x
  | Any spec -> (
      match spec.any_enc with
      | Some selector -> encode (selector x) x
      | None -> failwith "any: enc not provided")
  | Array_builder spec ->
      let items =
        spec.enc_fold.fold (fun acc elt -> encode spec.elt elt :: acc) [] x
      in
      Value.array (List.rev items)
  | Array_of_tables { inner; _ } -> Value.array (List.map (encode inner) x)
  | Table_obj { spec; _ } -> encode_table spec x

(* ---- Decoder interpreter ---- *)

let rec decode : type a. a t -> limits -> Value.t -> a =
 fun c lim v ->
  match c with
  | Bool -> decode_bool lim v
  | Int64 -> decode_int64 lim v
  | Float -> decode_float lim v
  | String -> decode_string lim v
  | Number -> decode_number lim v
  | Int_as_string -> decode_int_as_string lim v
  | Int64_as_string -> decode_int64_as_string lim v
  | _ -> decode_datetime_or_more c lim v

and decode_datetime_or_more : type a. a t -> limits -> Value.t -> a =
 fun c lim v ->
  match c with
  | Ptime_any { tz; now; frac_s = _ } ->
      limits_bump_node lim v;
      decode_ptime_any ~tz ~now v
  | Ptime_offset _ ->
      limits_bump_node lim v;
      decode_ptime_offset v
  | Ptime_span ->
      limits_bump_node lim v;
      decode_ptime_span v
  | Ptime_date ->
      limits_bump_node lim v;
      decode_ptime_date v
  | Ptime_full { tz_offset_s } ->
      limits_bump_node lim v;
      decode_ptime_full ~tz_offset_s v
  | Const { value; _ } ->
      limits_bump_node lim v;
      value
  | Enum { cases; _ } -> decode_enum cases lim v
  | _ -> decode_combinator c lim v

and decode_combinator : type a. a t -> limits -> Value.t -> a =
 fun c lim v ->
  match c with
  | List inner -> decode_list inner lim v
  | Map { inner; dec; _ } -> locate_at v (fun () -> dec (decode inner lim v))
  | Labeled { inner; _ } -> decode inner lim v
  | Option { inner; _ } -> Some (decode inner lim v)
  | Result { ok; error } -> (
      try Ok (decode ok lim v) with Loc.Error _ -> Error (decode error lim v))
  | Fix lazy_c -> decode (Lazy.force lazy_c) lim v
  | Iter { inner; dec; _ } ->
      let x = decode inner lim v in
      (match dec with Some f -> f x | None -> ());
      x
  | Recode { dec; f; _ } -> f (decode dec lim v)
  | Uni_map { inner; dec; kind = k; _ } -> (
      match dec with
      | Some f -> locate_at v (fun () -> f (decode inner lim v))
      | None ->
          Error.no_decoder (Value.meta v)
            ~kind:(Sort.kinded_string ~kind:k base_map_sort))
  | _ -> decode_query c lim v

and decode_query : type a. a t -> limits -> Value.t -> a =
 fun c lim v ->
  match c with
  | Key { name; inner } -> decode_key ~name inner lim v
  | Update_key { name; inner } -> decode_update_key ~name inner lim v
  | Delete_key { name } -> decode_delete_key ~name lim v
  | Nth { index; absent; inner } -> decode_nth ~index ~absent inner lim v
  | Mem { name; absent; inner } -> decode_mem ~name ~absent inner lim v
  | Fold_array { inner; f; init } -> decode_fold_array inner ~f ~init lim v
  | Fold_table { inner; f; init } -> decode_fold_table inner ~f ~init lim v
  | _ -> decode_leaf_or_structural c lim v

and decode_leaf_or_structural : type a. a t -> limits -> Value.t -> a =
 fun c lim v ->
  match c with
  | Ignore ->
      limits_bump_node lim v;
      ()
  | Zero ->
      limits_bump_node lim v;
      ()
  | Identity ->
      limits_bump_node lim v;
      v
  | Value_mems -> (
      match v with
      | Value.Table (pairs, _) ->
          limits_bump_node lim v;
          List.map (fun ((k, _), v) -> (k, v)) pairs
      | v -> err_expected_got "table" v)
  | Any spec -> decode_any spec lim v
  | Array_builder spec -> decode_array_builder spec lim v
  | Array_of_tables { inner; kind = k; _ } ->
      decode_array_of_tables ~kind:k inner lim v
  | Table_obj { spec; _ } -> decode_table spec lim v
  | _ -> assert false

and decode_any : type a. a any_spec -> limits -> Value.t -> a =
 fun spec lim v ->
  match v with
  | Value.String _ -> (
      match spec.dec_string with
      | Some c -> decode c lim v
      | None -> err_expected_got "string" v)
  | Value.Int _ -> (
      match spec.dec_int with
      | Some c -> decode c lim v
      | None -> err_expected_got "integer" v)
  | Value.Float _ -> (
      match spec.dec_float with
      | Some c -> decode c lim v
      | None -> err_expected_got "float" v)
  | Value.Bool _ -> (
      match spec.dec_bool with
      | Some c -> decode c lim v
      | None -> err_expected_got "boolean" v)
  | Value.Datetime _ | Value.Datetime_local _ | Value.Date_local _
  | Value.Time_local _ -> (
      match spec.dec_datetime with
      | Some c -> decode c lim v
      | None -> err_expected_got "datetime" v)
  | Value.Array _ -> (
      match spec.dec_array with
      | Some c -> decode c lim v
      | None -> err_expected_got "array" v)
  | Value.Table _ -> (
      match spec.dec_table with
      | Some c -> decode c lim v
      | None -> err_expected_got "table" v)

and decode_array_builder : type arr elt builder.
    (arr, elt, builder) array_spec -> limits -> Value.t -> arr =
 fun spec lim v ->
  match v with
  | Value.Array (items, _) ->
      limits_bump_node lim v;
      limits_enter lim v;
      let akn = (spec.kind, Meta.none) in
      let result =
        decode_array_builder_items spec lim akn (spec.dec_empty ()) 0 items
      in
      limits_leave lim;
      result
  | v -> err_expected_got "array" v

and decode_array_builder_items : type arr elt builder.
    (arr, elt, builder) array_spec ->
    limits ->
    string * Meta.t ->
    builder ->
    int ->
    Value.t list ->
    arr =
 fun spec lim akn builder i items ->
  match items with
  | [] -> spec.dec_finish builder
  | item :: rest ->
      let elt =
        push_nth_ctx akn (i, Meta.none) (fun () -> decode spec.elt lim item)
      in
      decode_array_builder_items spec lim akn (spec.dec_add elt builder) (i + 1)
        rest

and decode_list : type a. a t -> limits -> Value.t -> a list =
 fun inner lim v ->
  match v with
  | Value.Array (items, _) ->
      limits_bump_node lim v;
      limits_enter lim v;
      let akn = ("list of " ^ kind inner, Meta.none) in
      let r = decode_list_items inner lim akn [] 0 items in
      limits_leave lim;
      r
  | v -> err_expected_got "array" v

and decode_list_items : type a.
    a t -> limits -> string * Meta.t -> a list -> int -> Value.t list -> a list
    =
 fun inner lim akn acc i items ->
  match items with
  | [] -> List.rev acc
  | x :: xs ->
      let y = push_nth_ctx akn (i, Meta.none) (fun () -> decode inner lim x) in
      decode_list_items inner lim akn (y :: acc) (i + 1) xs

and decode_key : type a. name:string -> a t -> limits -> Value.t -> a =
 fun ~name inner lim v ->
  match v with
  | Value.Table (pairs, _) -> (
      limits_bump_node lim v;
      let tkn = table_kind_node "" in
      match assoc_name_opt name pairs with
      | Some x ->
          push_mem_ctx tkn (name, Meta.none) (fun () -> decode inner lim x)
      | None -> Error.failf (Value.meta v) "missing member %S" name)
  | v -> err_expected_got "table" v

and decode_update_key : type a.
    name:string -> a t -> limits -> Value.t -> Value.t =
 fun ~name inner lim v ->
  match v with
  | Value.Table (pairs, m) -> (
      limits_bump_node lim v;
      let tkn = table_kind_node "" in
      match assoc_name_opt name pairs with
      | Some x ->
          let pairs' = decode_update_pairs ~name inner lim tkn x pairs in
          Value.Table (pairs', m)
      | None -> Error.failf (Value.meta v) "missing member %S" name)
  | v -> err_expected_got "table" v

and decode_update_pairs : type a.
    name:string ->
    a t ->
    limits ->
    string * Meta.t ->
    Value.t ->
    ((string * Loc.Meta.t) * Value.t) list ->
    ((string * Loc.Meta.t) * Value.t) list =
 fun ~name inner lim tkn x pairs ->
  let decoded =
    push_mem_ctx tkn (name, Meta.none) (fun () -> decode inner lim x)
  in
  let encoded = encode inner decoded in
  List.map
    (fun ((k, km), vv) ->
      if k = name then ((k, km), encoded) else ((k, km), vv))
    pairs

and decode_nth : type a.
    index:int -> absent:a option -> a t -> limits -> Value.t -> a =
 fun ~index ~absent inner lim v ->
  match v with
  | Value.Array (arr, _) -> (
      limits_bump_node lim v;
      if index >= 0 && index < List.length arr then
        let akn = array_kind_node "" in
        push_nth_ctx akn (index, Meta.none) (fun () ->
            decode inner lim (List.nth arr index))
      else
        match absent with
        | Some x -> x
        | None ->
            Error.failf (Value.meta v) "array index %d out of bounds" index)
  | v -> err_expected_got "array" v

and decode_mem : type a.
    name:string -> absent:a option -> a t -> limits -> Value.t -> a =
 fun ~name ~absent inner lim v ->
  match v with
  | Value.Table (pairs, _) -> (
      limits_bump_node lim v;
      let tkn = table_kind_node "" in
      match assoc_name_opt name pairs with
      | Some x ->
          push_mem_ctx tkn (name, Meta.none) (fun () -> decode inner lim x)
      | None -> (
          match absent with
          | Some x -> x
          | None -> Error.failf (Value.meta v) "missing member %S" name))
  | v -> err_expected_got "table" v

and decode_fold_array : type a acc.
    a t -> f:(int -> a -> acc -> acc) -> init:acc -> limits -> Value.t -> acc =
 fun inner ~f ~init lim v ->
  match v with
  | Value.Array (arr, _) ->
      limits_bump_node lim v;
      let akn = array_kind_node "" in
      decode_fold_array_items inner ~f lim akn init 0 arr
  | v -> err_expected_got "array" v

and decode_fold_array_items : type a acc.
    a t ->
    f:(int -> a -> acc -> acc) ->
    limits ->
    string * Meta.t ->
    acc ->
    int ->
    Value.t list ->
    acc =
 fun inner ~f lim akn acc i items ->
  match items with
  | [] -> acc
  | x :: xs ->
      let y = push_nth_ctx akn (i, Meta.none) (fun () -> decode inner lim x) in
      decode_fold_array_items inner ~f lim akn (f i y acc) (i + 1) xs

and decode_fold_table : type a acc.
    a t -> f:(string -> a -> acc -> acc) -> init:acc -> limits -> Value.t -> acc
    =
 fun inner ~f ~init lim v ->
  match v with
  | Value.Table (pairs, _) ->
      limits_bump_node lim v;
      let tkn = table_kind_node "" in
      decode_fold_table_items inner ~f lim tkn init pairs
  | v -> err_expected_got "table" v

and decode_fold_table_items : type a acc.
    a t ->
    f:(string -> a -> acc -> acc) ->
    limits ->
    string * Meta.t ->
    acc ->
    ((string * Loc.Meta.t) * Value.t) list ->
    acc =
 fun inner ~f lim tkn acc pairs ->
  match pairs with
  | [] -> acc
  | ((k, _), x) :: rest ->
      let y = push_mem_ctx tkn (k, Meta.none) (fun () -> decode inner lim x) in
      decode_fold_table_items inner ~f lim tkn (f k y acc) rest

and decode_array_of_tables : type a.
    kind:string -> a t -> limits -> Value.t -> a list =
 fun ~kind:k inner lim v ->
  match v with
  | Value.Array (items, _) ->
      limits_bump_node lim v;
      limits_enter lim v;
      let akn = (k, Meta.none) in
      let result = decode_array_of_tables_items inner lim akn [] 0 items in
      limits_leave lim;
      result
  | v -> err_expected_got "array" v

and decode_array_of_tables_items : type a.
    a t -> limits -> string * Meta.t -> a list -> int -> Value.t list -> a list
    =
 fun inner lim akn acc i items ->
  match items with
  | [] -> List.rev acc
  | item :: rest ->
      let y =
        push_nth_ctx akn (i, Meta.none) (fun () -> decode inner lim item)
      in
      decode_array_of_tables_items inner lim akn (y :: acc) (i + 1) rest

(* ---- Base codecs ---- *)

let bool = Bool

(* These are handed the number the inner codec read and not the node it came
   from, so the place is put on by [locate_at] where the mapping codec runs
   them. *)
let int64_to_int_checked i =
  if i >= Int64.of_int min_int && i <= Int64.of_int max_int then Int64.to_int i
  else Error.failf Meta.none "integer overflow: %Ld" i

let int64_to_int32_checked i =
  if i >= Int64.of_int32 Int32.min_int && i <= Int64.of_int32 Int32.max_int then
    Int64.to_int32 i
  else Error.failf Meta.none "integer overflow: %Ld" i

let int =
  Map
    {
      inner = Int64;
      dec = int64_to_int_checked;
      enc = Int64.of_int;
      kind = "integer";
      doc = "";
    }

let int32 =
  Map
    {
      inner = Int64;
      dec = int64_to_int32_checked;
      enc = Int64.of_int32;
      kind = "integer";
      doc = "";
    }

let int64 = Int64
let float = Float
let string = String
let number = Number
let int_as_string = Int_as_string
let int64_as_string = Int64_as_string

(* ---- Ptime codecs ---- *)

let tz_offset ?tz_offset_s ?get_tz () =
  tz_offset_s
  |> Option.fold ~none:(Option.bind get_tz (fun f -> f ())) ~some:Option.some
  |> Option.value ~default:0

let ptime ?tz_offset_s ?get_tz ?now ?(frac_s = 0) () =
  let tz () = tz_offset ?tz_offset_s ?get_tz () in
  Ptime_any { tz; now; frac_s }

let ptime_opt ?(tz_offset_s = 0) ?(frac_s = 0) () =
  Ptime_offset { tz_offset_s; frac_s }

let ptime_span = Ptime_span
let ptime_date = Ptime_date

let ptime_full ?tz_offset_s ?get_tz () =
  let tz_offset_s =
    Option.fold
      ~none:(Option.bind get_tz (fun f -> f ()))
      ~some:Option.some tz_offset_s
  in
  Ptime_full { tz_offset_s }

(* ---- Combinators ---- *)

let map ?kind:k ?doc:d ?dec ?enc c =
  let k = Option.value ~default:(kind c) k in
  let d = Option.value ~default:(doc c) d in
  match (dec, enc) with
  | Some dec_fn, Some enc_fn ->
      Map { inner = c; dec = dec_fn; enc = enc_fn; kind = k; doc = d }
  | _ -> Uni_map { inner = c; dec; enc; kind = k; doc = d }

let const ?kind:k ?doc:d value =
  let k = Option.value ~default:"constant" k in
  let d = Option.value ~default:"" d in
  Const { value; kind = k; doc = d }

let enum ?cmp ?kind:k ?doc:d cases =
  let cmp = Option.value ~default:Stdlib.compare cmp in
  let k = Option.value ~default:"enum" k in
  let d = Option.value ~default:"" d in
  Enum { cases; cmp; kind = k; doc = d }

let option ?kind:k ?doc:d c =
  let k = Option.value ~default:("optional " ^ kind c) k in
  let d = Option.value ~default:(doc c) d in
  Option { inner = c; kind = k; doc = d }

let result ~ok ~error = Result { ok; error }
let fix lazy_c = Fix lazy_c

let iter ?kind:k ?doc:d ?dec ?enc c =
  let k = Option.value ~default:(kind c) k in
  let d = Option.value ~default:(doc c) d in
  Iter { inner = c; dec; enc; kind = k; doc = d }

let recode ~dec:dec_codec f ~enc:enc_codec =
  Recode { dec = dec_codec; f; enc = enc_codec }

(* ---- Query / Update / Nth / Mem ---- *)

let key name value_codec = Key { name; inner = value_codec }
let update_key name c = Update_key { name; inner = c }
let delete_key name = Delete_key { name }
let nth ?absent index elt_codec = Nth { index; absent; inner = elt_codec }
let mem ?absent name value_codec = Mem { name; absent; inner = value_codec }
let fold_array elt_codec f init = Fold_array { inner = elt_codec; f; init }
let fold_table value_codec f init = Fold_table { inner = value_codec; f; init }

(* ---- Ignoring and placeholders ---- *)

let ignore = Ignore
let zero = Zero

(* ---- Array codecs ---- *)

module Array = struct
  type 'a codec = 'a t

  type ('array, 'elt) enc = {
    fold : 'acc. ('acc -> 'elt -> 'acc) -> 'acc -> 'array -> 'acc;
  }

  type ('array, 'elt, 'builder) map = {
    kind : string;
    doc : string;
    elt : 'elt codec;
    dec_empty : unit -> 'builder;
    dec_add : 'elt -> 'builder -> 'builder;
    dec_finish : 'builder -> 'array;
    enc : ('array, 'elt) enc;
  }

  let map ?kind:k ?doc:d
      ?(dec_empty = fun () -> failwith "decode not supported")
      ?(dec_add = fun _ _ -> failwith "decode not supported")
      ?(dec_finish = fun _ -> failwith "decode not supported")
      ?(enc = { fold = (fun _ _ _ -> failwith "encode not supported") })
      (elt : 'elt codec) : ('array, 'elt, 'builder) map =
    let k = Option.value ~default:("array of " ^ kind elt) k in
    let d = Option.value ~default:"" d in
    { kind = k; doc = d; elt; dec_empty; dec_add; dec_finish; enc }

  let list ?kind:k ?doc:d (elt : 'a codec) : ('a list, 'a, 'a list) map =
    let k = Option.value ~default:("list of " ^ kind elt) k in
    let d = Option.value ~default:"" d in
    {
      kind = k;
      doc = d;
      elt;
      dec_empty = (fun () -> []);
      dec_add = (fun x xs -> x :: xs);
      dec_finish = List.rev;
      enc = { fold = (fun f acc xs -> List.fold_left f acc xs) };
    }

  let array ?kind:k ?doc:d (elt : 'a codec) : ('a array, 'a, 'a list) map =
    let k = Option.value ~default:("array of " ^ kind elt) k in
    let d = Option.value ~default:"" d in
    {
      kind = k;
      doc = d;
      elt;
      dec_empty = (fun () -> []);
      dec_add = (fun x xs -> x :: xs);
      dec_finish = (fun xs -> Stdlib.Array.of_list (List.rev xs));
      enc = { fold = (fun f acc arr -> Stdlib.Array.fold_left f acc arr) };
    }

  let finish (type arr elt builder) (m : (arr, elt, builder) map) : arr codec =
    let spec : (arr, elt, builder) array_spec =
      {
        kind = m.kind;
        doc = m.doc;
        elt = m.elt;
        dec_empty = m.dec_empty;
        dec_add = m.dec_add;
        dec_finish = m.dec_finish;
        enc_fold = { fold = (fun f acc arr -> m.enc.fold f acc arr) };
      }
    in
    Array_builder spec
end

let list ?kind:k ?doc:d c =
  match (k, d) with
  | None, None -> List c
  | _ ->
      let new_kind = Option.value ~default:("list of " ^ kind c) k in
      let new_doc = Option.value ~default:"" d in
      Labeled { kind = new_kind; doc = new_doc; inner = List c }

let array ?kind:k ?doc:d c =
  let new_kind = Option.value ~default:("array of " ^ kind c) k in
  let new_doc = Option.value ~default:"" d in
  Map
    {
      inner = List c;
      dec = Stdlib.Array.of_list;
      enc = Stdlib.Array.to_list;
      kind = new_kind;
      doc = new_doc;
    }

(* ---- Table codecs ---- *)

module Table = struct
  let codec_kind = kind
  let codec_doc = doc

  type 'a codec = 'a t

  type nonrec unknown_handling = unknown_handling =
    | Skip
    | Error_on_unknown
    | Keep

  type nonrec 'o mem_encoder = 'o mem_encoder = {
    enc : 'o -> Value.t;
    should_omit : 'o -> bool;
  }

  type nonrec ('o, 'a) mem_spec = ('o, 'a) mem_spec = {
    name : string;
    mem_doc : string;
    mem_codec : 'a t;
    dec_absent : 'a option;
    enc_typed : 'o mem_encoder option;
  }

  let make_enc_typed (c : 'a codec) enc enc_omit =
    match enc with
    | None -> None
    | Some f ->
        let omit = Option.value ~default:(fun _ -> false) enc_omit in
        Some
          {
            enc = (fun o -> encode c (f o));
            should_omit = (fun o -> omit (f o));
          }

  module Mem = struct
    type 'a codec = 'a t
    type ('o, 'a) t = ('o, 'a) mem_spec

    let v ?doc ?(dec_absent : 'a option) ?enc ?enc_omit name (codec : 'a codec)
        =
      {
        name;
        mem_doc = Option.value ~default:"" doc;
        mem_codec = codec;
        dec_absent;
        enc_typed = make_enc_typed codec enc enc_omit;
      }

    let opt ?doc ?enc name (codec : 'a codec) =
      let opt_codec = option codec in
      {
        name;
        mem_doc = Option.value ~default:"" doc;
        mem_codec = opt_codec;
        dec_absent = Some None;
        enc_typed = make_enc_typed opt_codec enc (Some Option.is_none);
      }

    let name (m : ('o, 'a) t) = m.name
    let doc (m : ('o, 'a) t) = m.mem_doc
    let codec (m : ('o, 'a) t) = m.mem_codec
    let dec_absent (m : ('o, 'a) t) = m.dec_absent
  end

  type ('o, 'dec) map = {
    map_kind : string;
    map_doc : string;
    members : ('o, Value.t) mem_spec list;
    dec : limits -> table_input -> 'dec;
    unknown : unknown_handling;
    keep_unknown_enc : ('o -> (string * Value.t) list) option;
  }

  let obj ?kind:k ?doc:d dec =
    let k = Option.value ~default:"table" k in
    let d = Option.value ~default:"" d in
    {
      map_kind = k;
      map_doc = d;
      members = [];
      dec = (fun _ _ -> dec);
      unknown = Skip;
      keep_unknown_enc = None;
    }

  let obj' ?kind:k ?doc:d dec_fn =
    let k = Option.value ~default:"table" k in
    let d = Option.value ~default:"" d in
    {
      map_kind = k;
      map_doc = d;
      members = [];
      dec = (fun _ _ -> dec_fn ());
      unknown = Skip;
      keep_unknown_enc = None;
    }

  (* Raw passthrough codec used for members in the reverse-order list. We
     synthesize a Uni_map/Identity-style codec that decodes to the raw Value.t
     (without decoding the inner codec yet); the dec chain does the actual
     decoding inside its closure. *)
  let raw_pass_codec ~kind:k ~doc:d : Value.t codec =
    Labeled { kind = k; doc = d; inner = Identity }

  let mem ?doc ?dec_absent ?enc ?enc_omit name (c : 'a codec) m =
    let raw_spec =
      {
        name;
        mem_doc = Option.value ~default:"" doc;
        mem_codec = raw_pass_codec ~kind:(codec_kind c) ~doc:(codec_doc c);
        dec_absent = None;
        enc_typed = make_enc_typed c enc enc_omit;
      }
    in
    let tkn = (m.map_kind, Meta.none) in
    {
      m with
      members = raw_spec :: m.members;
      dec =
        (fun lim input ->
          let f = m.dec lim input in
          let v_opt = List.assoc_opt name input.all_pairs in
          match v_opt with
          | Some v ->
              let decoded =
                push_mem_ctx tkn (name, Meta.none) (fun () -> decode c lim v)
              in
              f decoded
          | None -> (
              match dec_absent with
              | Some default -> f default
              | None ->
                  push_mem_ctx tkn (name, Meta.none) (fun () ->
                      Error.failf input.table_meta "missing required member: %s"
                        name)));
    }

  let opt_mem ?doc ?enc name (c : 'a codec) m =
    let default : 'a option = None in
    mem ?doc ?enc ~dec_absent:default ~enc_omit:Option.is_none name (option c) m

  module Mems = struct
    type 'a codec = 'a t

    type ('mems, 'a) enc = {
      fold : 'acc. ('acc -> string -> 'a -> 'acc) -> 'acc -> 'mems -> 'acc;
    }

    type ('mems, 'a, 'builder) map = {
      mems_kind : string;
      mems_doc : string;
      elt : 'a codec;
      dec_empty : unit -> 'builder;
      dec_add : string -> 'a -> 'builder -> 'builder;
      dec_finish : 'builder -> 'mems;
      enc : ('mems, 'a) enc;
    }

    let kind m = m.mems_kind
    let doc m = m.mems_doc

    let map ?kind ?doc ?(dec_empty = fun () -> failwith "decode not supported")
        ?(dec_add = fun _ _ _ -> failwith "decode not supported")
        ?(dec_finish = fun _ -> failwith "decode not supported")
        ?(enc = { fold = (fun _ _ _ -> failwith "encode not supported") }) elt =
      let k = Option.value ~default:("members of " ^ codec_kind elt) kind in
      let d = Option.value ~default:"" doc in
      { mems_kind = k; mems_doc = d; elt; dec_empty; dec_add; dec_finish; enc }

    module String_map = Map.Make (String)

    let string_map ?kind ?doc elt =
      let k = Option.value ~default:("string map of " ^ codec_kind elt) kind in
      let d = Option.value ~default:"" doc in
      {
        mems_kind = k;
        mems_doc = d;
        elt;
        dec_empty = (fun () -> []);
        dec_add = (fun k v acc -> (k, v) :: acc);
        dec_finish =
          (fun pairs ->
            List.fold_left
              (fun m (k, v) -> String_map.add k v m)
              String_map.empty pairs);
        enc =
          {
            fold =
              (fun f acc m -> String_map.fold (fun k v acc -> f acc k v) m acc);
          };
      }

    let assoc ?kind ?doc elt =
      let k = Option.value ~default:("assoc of " ^ codec_kind elt) kind in
      let d = Option.value ~default:"" doc in
      {
        mems_kind = k;
        mems_doc = d;
        elt;
        dec_empty = (fun () -> []);
        dec_add = (fun k v acc -> (k, v) :: acc);
        dec_finish = List.rev;
        enc =
          {
            fold =
              (fun f acc pairs ->
                List.fold_left (fun acc (k, v) -> f acc k v) acc pairs);
          };
      }
  end

  let skip_unknown m = { m with unknown = Skip }
  let error_unknown m = { m with unknown = Error_on_unknown }

  let keep_unknown ?enc mems m =
    let tkn = (m.map_kind, Meta.none) in
    let elt_codec = mems.Mems.elt in
    (* Built from the members this decode read, inside the decode that read
       them, under that decode's limits. Nothing survives the call. *)
    let collect lim input =
      let add acc (name, v) =
        let decoded =
          push_mem_ctx tkn (name, Meta.none) (fun () -> decode elt_codec lim v)
        in
        mems.Mems.dec_add name decoded acc
      in
      mems.Mems.dec_finish
        (List.fold_left add (mems.Mems.dec_empty ()) input.unknown_pairs)
    in
    let raw_spec =
      {
        name = "";
        mem_doc = "";
        mem_codec = raw_pass_codec ~kind:"unknown" ~doc:"";
        dec_absent = Some (Value.table []);
        enc_typed = None;
      }
    in
    {
      m with
      members = raw_spec :: m.members;
      unknown = Keep;
      keep_unknown_enc =
        Option.map
          (fun f o ->
            let mems_val = f o in
            mems.Mems.enc.fold
              (fun acc k v -> (k, encode elt_codec v) :: acc)
              [] mems_val
            |> List.rev)
          enc;
      dec =
        (fun lim input ->
          let f = m.dec lim input in
          f (collect lim input));
    }

  let find_dup xs =
    let rec loop seen = function
      | [] -> None
      | x :: rest -> if List.mem x seen then Some x else loop (x :: seen) rest
    in
    loop [] xs

  let to_spec (type o) (m : (o, o) map) : o table_spec =
    let members_ordered = List.rev m.members in
    let known_names =
      List.filter_map
        (fun spec -> if spec.name = "" then None else Some spec.name)
        members_ordered
    in
    Option.iter
      (fun name -> invalid_arg ("duplicate member name: " ^ name))
      (find_dup known_names);
    {
      kind = m.map_kind;
      doc = m.map_doc;
      members = m.members;
      dec = m.dec;
      unknown = m.unknown;
      keep_unknown_enc = m.keep_unknown_enc;
    }

  let finish m = Table_obj { spec = to_spec m; inline = false }
  let inline m = Table_obj { spec = to_spec m; inline = true }
end

(* ---- Array of tables ---- *)

let array_of_tables ?kind:k ?doc:d c =
  let k = Option.value ~default:("array of " ^ kind c) k in
  let d = Option.value ~default:"" d in
  Array_of_tables { inner = c; kind = k; doc = d }

(* ---- Generic value codecs ---- *)

let any ?kind:k ?doc:d ?dec_string ?dec_int ?dec_float ?dec_bool ?dec_datetime
    ?dec_array ?dec_table ?enc () =
  let k = Option.value ~default:"any" k in
  let d = Option.value ~default:"" d in
  Any
    {
      any_kind = k;
      any_doc = d;
      dec_string;
      dec_int;
      dec_float;
      dec_bool;
      dec_datetime;
      dec_array;
      dec_table;
      any_enc = enc;
    }

(* ---- Decode/encode entry points ---- *)

let of_toml_raise ?max_depth ?max_nodes c v =
  let lim = limits ?max_depth ?max_nodes () in
  decode c lim v

let of_toml ?max_depth ?max_nodes c v =
  try Ok (of_toml_raise ?max_depth ?max_nodes c v) with Loc.Error e -> Error e

let of_toml_exn ?max_depth ?max_nodes c v =
  of_toml_raise ?max_depth ?max_nodes c v

let to_toml c v = encode c v

(* ---- AST-preserving codecs ---- *)

module Value = struct
  let t = Identity
  let mems = Value_mems
end
