(* Join csv files on some common field *)
open Core
module Csv = Csvlib.Csv

module T = struct
  type t =
    | Full
    | Inner
    | Left
  [@@deriving compare, enumerate, sexp]
end

include T

let param =
  let join_switch = "-join" in
  let keys_need_not_occur_in_all_files_switch = "-keys-need-not-occur-in-all-files" in
  let%map_open.Command t =
    Enum.make_param join_switch (module T) ~doc:"as in SQL (default: inner)" ~f:optional
  and keys_need_not_occur_in_all_files =
    flag
      keys_need_not_occur_in_all_files_switch
      no_arg
      ~doc:" deprecated alias for -join full"
  in
  match t, keys_need_not_occur_in_all_files with
  | Some t, false -> t
  | None, false -> Inner
  | None, true -> Full
  | Some _, true ->
    raise_s
      [%message "cannot specify both" join_switch keys_need_not_occur_in_all_files_switch]
;;

module Row = struct
  type t = string list [@@deriving sexp]
end

module Key : sig
  type t

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create : string array -> t
  val to_list : t -> string list
end = struct
  module T = struct
    open Ppx_hash_lib.Std.Hash.Builtin

    type 'a array_frozen = 'a array [@@deriving compare, sexp]

    (* [array_frozen] can derive hash. We don't expose any mutation. *)
    type t = string array_frozen [@@deriving compare, hash, sexp]
  end

  include T
  include Comparable.Make (T)
  include Hashable.Make (T)

  let create t = t
  let to_list = Array.to_list
end

module Rows_by_key = struct
  (* The [header] and each [Row.t] in [data_by_key] are missing the key field. *)
  type t =
    { data_by_key : Row.t list Key.Map.t (* header for each row in [data_by_key]. *)
    ; header : Row.t
    }
  [@@deriving fields ~getters, sexp]

  let load_rows file ~sep =
    protectx
      (In_channel.create file)
      ~f:(fun channel ->
        let rows = Csv.load_in ~separator:sep channel in
        match
          List.dedup_and_sort
            (List.map rows ~f:(List.length :> _ -> _))
            ~compare:[%compare: int]
        with
        | [] | [ _ ] -> rows
        | _ -> failwithf "rows in %s have different lengths" file ())
      ~finally:In_channel.close
  ;;

  let load ~file_name ~key_fields ~sep =
    match load_rows ~sep file_name with
    | [] -> failwithf "file %s is  empty" file_name ()
    | header :: rows ->
      let hmap =
        match String.Map.of_alist (List.mapi header ~f:(fun i h -> h, i)) with
        | `Ok map -> map
        | `Duplicate_key h -> failwithf "repeated column %s in %s" h file_name ()
      in
      let key_indices =
        Array.map key_fields ~f:(fun key_field ->
          match Map.find hmap key_field with
          | Some i -> i
          | None -> failwithf "No %s column in %s" key_field file_name ())
      in
      let data_indices =
        let key_fields = String.Set.of_array key_fields in
        List.filter_mapi header ~f:(fun i h ->
          if Set.mem key_fields h then None else Some i)
      in
      let data_by_key =
        List.map rows ~f:(fun row ->
          let row = Array.of_list row in
          ( Key.create @@ Array.map key_indices ~f:(Array.get row)
          , List.map data_indices ~f:(Array.get row) ))
        |> Key.Map.of_alist_multi
      in
      { data_by_key
      ; header = List.map data_indices ~f:(Array.get (Array.of_list header))
      }
  ;;
end

module Join_result : sig
  type t

  val empty_for_left_join : Rows_by_key.t list -> t
  val empty_for_inner_join : Rows_by_key.t list -> t
  val empty_for_full_join : Rows_by_key.t list -> t

  (* Any join can be expressed as a left join with the correct keys on the left side. *)

  val do_left_join : t -> Rows_by_key.t -> t
  val to_rows : t -> Row.t Sequence.t
end = struct
  type t = (Key.t * Row.t Sequence.t) Sequence.t

  let empty_of_keys keys =
    Set.to_sequence keys |> Sequence.map ~f:(fun key -> key, Sequence.singleton [])
  ;;

  let empty_for_left_join = function
    | [] -> failwith "join requires at least one csv."
    | car :: _ -> empty_of_keys (Map.key_set (Rows_by_key.data_by_key car : _ Key.Map.t))
  ;;

  let reduce_keys maps ~f =
    Sequence.of_list maps
    |> Sequence.map ~f:Rows_by_key.data_by_key
    |> Sequence.map ~f:Map.key_set
    |> Sequence.reduce_exn ~f
    |> empty_of_keys
  ;;

  let empty_for_inner_join = reduce_keys ~f:Set.inter
  let empty_for_full_join = reduce_keys ~f:Set.union

  let do_left_join t rows =
    let empty_right_side_of_rows =
      Rows_by_key.header rows |> List.map ~f:(const "") |> Sequence.singleton
    in
    Sequence.map t ~f:(fun (key, left_side_of_rows) ->
      let right_side_of_rows : Row.t Sequence.t =
        match Map.find (Rows_by_key.data_by_key rows) key with
        | None -> empty_right_side_of_rows
        | Some right_rows -> Sequence.of_list right_rows
      in
      ( key
      , Sequence.concat_map left_side_of_rows ~f:(fun left_side_of_row ->
          Sequence.map right_side_of_rows ~f:(fun right_side_of_row ->
            left_side_of_row @ right_side_of_row)) ))
  ;;

  let to_rows join_result =
    Sequence.concat_map join_result ~f:(fun (key, rows) ->
      Sequence.map rows ~f:(fun row -> List.append (Key.to_list key) row))
  ;;
end

let join t files ~key_fields ~sep =
  let maps =
    List.map files ~f:(fun file_name -> Rows_by_key.load ~file_name ~key_fields ~sep)
  in
  let combined_header =
    List.append (Array.to_list key_fields) (List.concat_map maps ~f:Rows_by_key.header)
  in
  Option.iter
    (List.find_a_dup combined_header ~compare:[%compare: string])
    ~f:(fun duplicate ->
      raise_s
        [%message
          "Only key fields may appear in multiple files."
            (duplicate : string)
            (combined_header : string list)]);
  let init =
    match t with
    | Full -> Join_result.empty_for_full_join maps
    | Inner -> Join_result.empty_for_inner_join maps
    | Left -> Join_result.empty_for_left_join maps
  in
  let rows = List.fold maps ~init ~f:Join_result.do_left_join |> Join_result.to_rows in
  Sequence.shift_right rows combined_header
;;
