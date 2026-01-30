open Core
open Csv_common

let merge_headers csvs =
  List.fold csvs ~init:String.Map.empty ~f:(fun init csv ->
    List.fold csv.header ~init ~f:(fun init column ->
      match Map.find init column with
      | None -> Map.set init ~key:column ~data:(Map.length init)
      | Some _ -> init))
;;

let project_into merged_headers csv =
  let len = Map.length merged_headers in
  List.map csv.lines ~f:(fun line ->
    let arr = Array.create ~len "" in
    List.iter (List.zip_exn csv.header line) ~f:(fun (header, data) ->
      let idx = Map.find_exn merged_headers header in
      arr.(idx) <- data);
    Array.to_list arr)
;;

let header_line merged_headers =
  merged_headers
  |> Map.to_alist
  |> List.sort ~compare:(fun (_, i1) (_, i2) -> [%compare: int] i1 i2)
  |> List.map ~f:fst
;;

let merge csvs =
  let merged_headers = merge_headers csvs in
  { header = header_line merged_headers
  ; lines = List.bind csvs ~f:(project_into merged_headers)
  }
;;

let run ?separator files = print_csv ?separator (merge (load_all ?separator files))
