open Core
module Time = Time_float_unix

module Csv_kind = struct
  (* [Cut] also supports zero-indexed fields of no-header CSVs, but it's hard to share
     code because [Cut] is streaming and [Csv_sort] is not. *)

  type t = { index_of_field : Csv_common.t -> string -> int option }

  let has_header =
    let index_of_field ({ header; lines = _ } : Csv_common.t) field =
      List.findi header ~f:(fun _idx elem -> String.( = ) elem field) |> Option.map ~f:fst
    in
    { index_of_field }
  ;;

  let no_header = { index_of_field = (fun _ field -> Int.of_string_opt field) }
end

module Line_with_sort_key = struct
  type 'a t =
    { key : 'a
    ; line : string list
    }

  let compare c a b = c a.key b.key
  let line t = t.line
  let map_key t ~f = { t with key = f t.key }
end

module type Sortable = sig
  type t [@@deriving compare]

  val of_string : string -> t
end

module Converted = struct
  (* https://en.wikipedia.org/wiki/Schwartzian_transform *)

  type t =
    | T :
        { compare : 'a -> 'a -> int
        ; lines : 'a Line_with_sort_key.t Array.t
        }
        -> t

  let create_id compare lines = T { compare; lines }

  let create (module M : Sortable) lines =
    let lines = Array.map lines ~f:(Line_with_sort_key.map_key ~f:M.of_string) in
    create_id M.compare lines
  ;;

  (* The order here kinda matters, at least in that we need to check Int before Float.
     (2^63 is a parsable float, but loses information in the process.)

     All the typed values, however, are obviously incompatible (i.e. we can't confuse a
     time/span/byte, since the suffixes are unambiguous.) *)
  let infer_choices : (module Sortable) list =
    [ (module Time_ns.Span)
    ; (module Byte_units)
    ; (module Time)
    ; (module Int)
    ; (module Float)
    ]
  ;;

  let create_inferred lines =
    List.find_map infer_choices ~f:(fun choice ->
      Option.try_with (fun () -> create choice lines))
    (* Can't default to natsort because it would change behavior. Sad. *)
    |> Option.value_or_thunk ~default:(fun () -> create_id String.compare lines)
  ;;
end

module Sort_type = struct
  module T = struct
    type t =
      | Bytes
      | Float
      | Infer
      | Int
      | Natsort
      | Span
      | String
      | Time
    [@@deriving compare, enumerate, sexp_of]
  end

  include T

  let param =
    Enum.make_param_optional_comma_separated_with_default_doc
      "-field-types"
      (module T)
      ~aliases:[ "--field-types" ]
      ~default:[]
      ~doc:"field type for sorting (default: infer)"
      ~represent_choice_with:"_"
  ;;

  let convert sort_type (lines : string Line_with_sort_key.t Array.t) =
    match sort_type with
    | Bytes -> Converted.create (module Byte_units) lines
    | Float -> Converted.create (module Float) lines
    | Infer -> Converted.create_inferred lines
    | Int -> Converted.create (module Int) lines
    | Natsort -> Converted.create_id Numeric_string.compare lines
    | Span -> Converted.create (module Time_ns.Span) lines
    | String -> Converted.create_id String.compare lines
    | Time -> Converted.create (module Time) lines
  ;;
end

module Order = struct
  type t =
    | Ascending
    | Descending
  [@@deriving compare, enumerate, sexp_of]
end

module Sort_column = struct
  type t =
    { field : string
    ; order : Order.t
    ; sort_type : Sort_type.t
    }
  [@@deriving sexp_of]
end

module Sort_columns = struct
  type t =
    | Explicit_fields of Sort_column.t list
    | All_fields of Order.t

  let param : t Command.Param.t =
    let%map_open.Command () = return ()
    and fields = Csv_param.fields_backward_compat
    and sort_types = Sort_type.param
    and reverse = Csv_param.reverse
    and reverse_fields = Csv_param.reverse_fields in
    let l v = List.map fields ~f:(const v) in
    let sort_types =
      match sort_types with
      | [] -> l Sort_type.Infer
      | _ :: _ ->
        (match List.length sort_types = List.length fields with
         | true -> sort_types
         | false ->
           failwith
             "when specifying sort types you must specify one per sort field, in the \
              same order as the sort fields")
    in
    let orders =
      match reverse, reverse_fields with
      | true, _ :: _ -> failwith "may not specify both -reverse and -reverse-fields"
      | true, [] ->
        l Order.Descending (* an empty reverse list will reverse all sort fields *)
      | false, [] -> l Order.Ascending
      | false, reverse_fields ->
        let reverse_fields = String.Set.of_list reverse_fields in
        (match Set.is_subset reverse_fields ~of_:(String.Set.of_list fields) with
         | true ->
           List.map fields ~f:(fun field : Order.t ->
             if Set.mem reverse_fields field then Descending else Ascending)
         | false -> failwith "-reverse-fields must list a subset of the sort fields")
    in
    let num_fields = List.length fields in
    let num_orders = List.length orders in
    let num_sort_types = List.length sort_types in
    if num_fields <> num_orders
    then raise_s [%message "BUG" (num_fields : int) "<>" (num_orders : int)];
    if num_fields <> num_sort_types
    then
      failwith
        [%string
          "Unequal number of fields (%{num_fields#Int}) and sort_types \
           (%{num_sort_types#Int})"];
    if List.is_empty fields
    then All_fields (if reverse then Order.Descending else Order.Ascending)
    else (
      let explicit_fields =
        List.zip_exn (List.zip_exn fields sort_types) orders
        |> List.map ~f:(fun ((field, sort_type), order) ->
          { Sort_column.field; order; sort_type })
      in
      Explicit_fields explicit_fields)
  ;;

  let to_sort_column_list ~header = function
    | Explicit_fields ts -> ts
    | All_fields order ->
      (match header with
       | `Present header ->
         List.map header ~f:(fun field -> { Sort_column.field; order; sort_type = Infer })
       | `Anonymous_with_this_many_cols num_cols ->
         List.init num_cols ~f:(fun i ->
           { Sort_column.field = Int.to_string i; order; sort_type = Infer }))
  ;;
end

let sort_on_field
  ({ index_of_field } : Csv_kind.t)
  ({ field; order; sort_type } : Sort_column.t)
  (csv : Csv_common.t)
  =
  match index_of_field csv field with
  | None -> failwithf "unable to find csv field %s" field ()
  | Some idx ->
    let lines =
      Array.of_list_map csv.lines ~f:(fun line ->
        { Line_with_sort_key.key = List.nth_exn line idx; line })
    in
    let (T { lines; compare }) = Sort_type.convert sort_type lines in
    let compare =
      match order with
      | Ascending -> compare
      | Descending -> fun a b -> Comparable.reverse compare a b
    in
    let compare = Line_with_sort_key.compare compare in
    Array.stable_sort lines ~compare;
    let lines = Array.map lines ~f:Line_with_sort_key.line |> Array.to_list in
    { csv with lines }
;;

let sort_on_fields csv_kind ts csv =
  List.fold_right ts ~init:csv ~f:(sort_on_field csv_kind)
;;

let run ?separator ts file ~no_header =
  Csv_common.Or_file.with_all file ?separator ~no_header ~f:(fun csv ->
    let csv_kind, header =
      match no_header with
      | true ->
        let num_cols = Option.value_map (List.hd csv.lines) ~f:List.length ~default:0 in
        Csv_kind.no_header, `Anonymous_with_this_many_cols num_cols
      | false -> Csv_kind.has_header, `Present csv.header
    in
    let ts = Sort_columns.to_sort_column_list ~header ts in
    csv |> sort_on_fields csv_kind ts |> Csv_common.print_csv ?separator)
;;
