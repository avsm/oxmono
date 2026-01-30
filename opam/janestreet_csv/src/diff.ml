open! Core
module Csv = Csvlib.Csv

exception Bad_transformation of string

let diff ?(header = true) ~key csv1 csv2 =
  let module SM = struct
    module T = struct
      type t = string String.Map.t [@@deriving compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end
  in
  let split_csvs = Split_csv.by_key ~header ~key [ csv1; csv2 ] in
  if List.length split_csvs <> 2
  then raise (Bad_transformation "Expected 2 split_csvs")
  else (
    let build_table split_csv =
      Split_csv.kv_maps split_csv |> Base.Map.of_alist_exn (module SM)
    in
    (* Actually perform the diff of the value columns for a single key. Wherever there is
       a change from val_map_1 to val_map_2, the new value is "val1 --> val2". If one of
       the values does not exist, represent this with a token "NONE". *)
    let diff val_map_1 val_map_2 =
      if [%compare.equal: SM.t] val_map_1 val_map_2
      then None
      else (
        let output =
          Map.fold
            val_map_1
            ~init:String.Map.empty
            ~f:(fun ~key:header ~data:value output_map ->
              match Map.find val_map_2 header with
              | None -> Map.set output_map ~key:header ~data:(value ^ " --> NONE")
              | Some s ->
                if String.( = ) s value
                then Map.set output_map ~key:header ~data:value
                else Map.set output_map ~key:header ~data:(value ^ " --> " ^ s))
        in
        (* That covered all of the keys that show up in val_map_1, so now we just need to
           account for any new keys in val_map_2. *)
        Some
          (Map.fold val_map_2 ~init:output ~f:(fun ~key:header ~data:value output_map ->
             if Map.mem val_map_1 header
             then output_map
             else Map.set output_map ~key:header ~data:("NONE --> " ^ value))))
    in
    let split_csv_1 = List.nth_exn split_csvs 0 in
    let split_csv_2 = List.nth_exn split_csvs 1 in
    let table1 = build_table split_csv_1 in
    let table2 = build_table split_csv_2 in
    (* Once our hash table is built, we can perform the diff by iterating over the rows in
       the first csv and generating key / value maps for each row, then looking up the key
       map in the hash table and comparing the value maps based on header names. The
       result will be a new split_csv representing the diff. *)
    let diffed_rows =
      List.fold
        (Split_csv.kv_maps split_csv_1)
        ~init:[]
        ~f:(fun accum (key_map, value_map) ->
          let other_value_map =
            match Map.find table2 key_map with
            | None -> String.Map.empty
            | Some s -> s
          in
          match diff value_map other_value_map with
          | None -> accum
          | Some s -> (key_map, s) :: accum)
    in
    (* All that's missing now are the rows that show up in the second table but not in the
       first. We can add these by reversing the above process, with the added constraint
       that we are only interested in keys that don't appear in table1. *)
    let diffed_rows =
      Split_csv.kv_maps split_csv_2
      |> List.fold ~init:diffed_rows ~f:(fun accum (key_map, value_map) ->
        match Map.find table1 key_map with
        | Some _ -> accum
        | None ->
          (match diff String.Map.empty value_map with
           | None -> accum
           | Some s -> (key_map, s) :: accum))
    in
    let diffed_rows = List.rev diffed_rows in
    (* Now that we have a list of header -> value maps representing the diff, we can
       construct a Csv.t, maintaining the same column ordering as in the first file. *)
    Mapped_csv.to_csv
      (Split_csv.to_mapped_csv
         (Split_csv.create diffed_rows (Split_csv.header_map split_csv_1))))
;;

let diff_from_files ?(header = true) ~key file1 file2 =
  let csv1 = Csv.load file1 in
  let csv2 = Csv.load file2 in
  diff ~header ~key csv1 csv2
;;
