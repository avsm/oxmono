open Core
open Async
open Csv_common

let find_insert_index header ~after =
  match after with
  | None -> List.length header
  | Some after_column ->
    (match List.findi header ~f:(fun _ column -> String.equal column after_column) with
     | Some (i, _) -> i + 1
     | None -> failwithf "Column '%s' not found in header" after_column ())
;;

let insert_at xs i x =
  let rec loop i acc l =
    match i, l with
    | 0, _ -> List.rev_append acc (x :: l)
    | _, [] -> failwith "BUG: tried to insert at index past end of list"
    | n, hd :: tl -> loop (n - 1) (hd :: acc) tl
  in
  loop i [] xs
;;

let run ?sep ?after (file : Or_file.t) ~column ~value ~allow_duplicate_column =
  let%bind reader =
    match file with
    | Csv _ -> failwith "BUG: expected a file and got a csv directly"
    | File f -> Reader.open_file f
    | Stdin -> force Reader.stdin |> return
  in
  let pipe =
    Delimited.Read.pipe_of_reader Delimited.Read.Row.builder ~header:`No ?sep reader
  in
  (* Somewhat surprisingly, benchmarks did not show an appreciable speed-up with a larger
     [size_budget] for the pipe, so we leave it with the default. *)
  let%bind header =
    match%map Pipe.read pipe with
    | `Eof -> failwith "Cannot add column to an empty csv"
    | `Ok header -> Delimited.Read.Row.to_list header
  in
  if (not allow_duplicate_column) && List.mem header column ~equal:String.equal
  then
    failwithf
      "Column '%s' already exists in header (pass -allow-duplicate-column to add anyway)"
      column
      ();
  let insert_index = find_insert_index header ~after in
  let new_header = insert_at header insert_index column in
  let writer =
    Delimited.Write.Expert.By_row.of_writer_and_close ?sep (Lazy.force Writer.stdout)
  in
  let%bind () = Pipe.write writer new_header in
  Pipe.transfer' pipe writer ~f:(fun queue ->
    return
      (Queue.map queue ~f:(fun row ->
         insert_at (Delimited.Read.Row.to_list row) insert_index value)))
;;
