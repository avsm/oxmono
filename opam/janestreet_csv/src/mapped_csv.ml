open Core

type t =
  { row_maps : string String.Map.t list
  ; header_map : string Int.Map.t
  }
[@@deriving fields ~getters]

let create row_maps header_map = { row_maps; header_map }

exception Malformed_csv of string

(* Converts a Csv.t to a Mapped_csv.t. This requires that the first row of the csv
   contains the headers. *)
let of_csv csv =
  let build_header_map header_fields =
    List.foldi header_fields ~init:Int.Map.empty ~f:(fun idx map field_name ->
      Map.set (map : _ Int.Map.t) ~key:(idx + 1) ~data:field_name)
  in
  match csv with
  | [] -> create [] Int.Map.empty
  | header_fields :: lines ->
    let mapped_rows =
      List.fold lines ~init:[] ~f:(fun accum row ->
        let line_map =
          match List.zip header_fields row with
          | Ok line ->
            List.fold line ~init:String.Map.empty ~f:(fun map (key, data) ->
              Map.set (map : _ String.Map.t) ~key ~data)
          | Unequal_lengths ->
            let msg =
              if List.length header_fields > List.length row then "More" else "Fewer"
            in
            raise (Malformed_csv (sprintf "%s header fields than columns" msg))
        in
        line_map :: accum)
    in
    create mapped_rows (build_header_map header_fields)
;;

let to_csv mapped_csv =
  let output_rows =
    List.fold mapped_csv.row_maps ~init:[] ~f:(fun csv_output row_map ->
      let next_row =
        Map.fold
          (mapped_csv.header_map : _ Int.Map.t)
          ~init:[]
          ~f:(fun ~key:_ ~data:header output_row ->
            Map.find_exn (row_map : _ String.Map.t) header :: output_row)
      in
      List.rev next_row :: csv_output)
  in
  let output_headers =
    List.rev
      (Map.fold
         (mapped_csv.header_map : _ Int.Map.t)
         ~init:[]
         ~f:(fun ~key:_ ~data:header output -> header :: output))
  in
  output_headers :: output_rows
;;
