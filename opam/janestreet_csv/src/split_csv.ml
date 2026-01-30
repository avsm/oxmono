open Core
module Csv = Csvlib.Csv

type t =
  { kv_maps : (string String.Map.t * string String.Map.t) list
  ; header_map : string Int.Map.t
  }
[@@deriving fields ~getters]

let create kv_maps header_map = { kv_maps; header_map }

let to_mapped_csv split_csv =
  (* Merge two maps where a given key may appear in at most one of the maps. *)
  let merge =
    Map.merge ~f:(fun ~key:_ -> function
      | `Both _ -> raise (Invalid_argument "Column appears in both key and value maps")
      | `Left s | `Right s -> Some s)
  in
  Mapped_csv.create
    (List.fold split_csv.kv_maps ~init:[] ~f:(fun accum (key_map, value_map) ->
       merge key_map value_map :: accum))
    split_csv.header_map
;;

let of_mapped_csv mapped_csv ~(key_spec : Key_specifier.t) =
  let module SM = String.Map in
  let module SS = String.Set in
  let column_set = Key_specifier.to_string_set key_spec in
  let validate_key_spec () =
    if Set.is_empty column_set
    then raise (Invalid_argument "No key columns specified")
    else (
      let header_set =
        Map.fold
          (Mapped_csv.header_map mapped_csv : _ Int.Map.t)
          ~init:SS.empty
          ~f:(fun ~key:_ ~data:header output_set -> Set.add output_set header)
      in
      Set.iter column_set ~f:(fun col ->
        if not (Set.mem header_set col)
        then raise (Invalid_argument (sprintf "Key column %s does not exist" col)));
      if Set.length column_set = Set.length header_set
      then raise (Invalid_argument "All columns are marked as key columns.")
      else ())
  in
  validate_key_spec ();
  let get_key_and_value_for_row_map row_map =
    Map.fold row_map ~init:(SM.empty, SM.empty) ~f:(fun ~key ~data (key_map, val_map) ->
      if Set.mem column_set key
      then Map.set key_map ~key ~data, val_map
      else key_map, Map.set val_map ~key ~data)
  in
  create
    (List.fold (Mapped_csv.row_maps mapped_csv) ~init:[] ~f:(fun accum row_map ->
       get_key_and_value_for_row_map row_map :: accum))
    (Mapped_csv.header_map mapped_csv)
;;

(* Converts a list of Csv.t to a list of split_csv where the key columns are specified by
   ~key. ~f is then applied to the output. *)
let by_key ?(header = true) ~key csvs =
  let col_spec =
    if header
    then Key_specifier.specifier_of_string key
    else Key_specifier.int_specifier_of_string key
  in
  let insert_numerical_header csv =
    let num_columns = Csv.columns csv in
    let rec header_row num_columns accum =
      if num_columns = 0
      then accum
      else header_row (num_columns - 1) (string_of_int num_columns :: accum)
    in
    header_row num_columns [] :: csv
  in
  let csvs = if header then csvs else List.map csvs ~f:insert_numerical_header in
  let split_csvs =
    List.map csvs ~f:(fun csv -> of_mapped_csv (Mapped_csv.of_csv csv) ~key_spec:col_spec)
  in
  split_csvs
;;

let by_key_from_files ?(header = true) ~key files =
  by_key ~header ~key (List.map files ~f:Csv.load)
;;
