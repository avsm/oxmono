open Core
open Async

type t =
  | Ragged of int list Int.Map.t option
  | Rectangular
[@@deriving sexp]

let create_verbose csv =
  let rectangular =
    match List.transpose csv with
    | None -> false
    | Some (_ : string list list) -> true
  in
  match rectangular with
  | true -> Rectangular
  | false ->
    List.mapi csv ~f:(fun i row -> List.length row, i + 1)
    |> Int.Map.of_alist_multi
    |> Some
    |> Ragged
;;

let is_rectangular csv =
  let row_length = ref (-1) in
  Pipe.fold_without_pushback csv ~init:true ~f:(fun accum row ->
    match !row_length with
    (* Initialize to the length of the header row *)
    | -1 ->
      row_length := Delimited.Read.Row.length row;
      accum
    | row_length ->
      if Delimited.Read.Row.length row = row_length
      then accum
      else (
        Pipe.close_read csv;
        false))
;;

let create_streaming ?sep reader =
  let csv = Delimited.Read.pipe_of_reader ?sep Delimited.Read.Row.builder reader in
  match%bind is_rectangular csv with
  | true -> return Rectangular
  | false -> return (Ragged None)
;;

type clump =
  | One of int
  | Span of int * int

let string_of_clump = function
  | One i -> Int.to_string i
  | Span (lo, hi) -> Int.to_string lo ^ "-" ^ Int.to_string hi
;;

let string_of_clumps clumps = String.concat ~sep:"," (List.map ~f:string_of_clump clumps)

let to_clumps ns =
  List.sort ~compare:Int.compare ns
  |> List.group ~break:(fun a b -> a + 1 <> b)
  |> List.map ~f:(function
    | [] -> assert false
    | [ i ] -> One i
    | x :: xs -> Span (x, List.last_exn xs))
;;

let to_error_string t =
  match t with
  | Rectangular -> Ok ()
  | Ragged None -> Error [ [ "" ] ]
  | Ragged (Some row_indexes_by_length) ->
    let csv =
      let rows =
        row_indexes_by_length
        |> Map.to_alist
        |> List.map ~f:(fun (n, rows_with_n_cols) ->
          n, List.length rows_with_n_cols, to_clumps rows_with_n_cols)
        |> List.sort ~compare:(fun a b ->
          let size (_, n, _) = n in
          -Int.compare (size a) (size b))
        |> List.map ~f:(fun (a, b, clumps) ->
          [ Int.to_string a; Int.to_string b; string_of_clumps clumps ])
      in
      [ "N"; "number of lines with N columns"; "lines with N columns" ] :: rows
    in
    Error csv
;;
