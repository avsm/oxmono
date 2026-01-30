open Core
open Async

let run ?sep reader =
  let%bind csv_shape = Csv_shape.create_streaming ?sep reader in
  match Csv_shape.to_error_string csv_shape with
  | Ok () -> return (Ok ())
  | Error error_csv ->
    let error_csv = List.map ~f:(String.concat ~sep:",") error_csv in
    (match error_csv with
     | [ "" ] -> return (Error "")
     | error_csv ->
       return
         (Error
            (String.concat
               ~sep:"\n"
               ("Error: lines with different numbers of columns" :: "" :: error_csv))))
;;
