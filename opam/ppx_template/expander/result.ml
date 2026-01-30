open! Stdppx
include Result

module Let_syntax = struct
  let ( let* ) t f = bind t ~f
  let ( let+ ) t f = map t ~f
  let ( >>= ) = ( >>= )
  let ( >>| ) = ( >>| )
end

open Let_syntax

let map_error t ~f =
  match t with
  | Ok _ as ok -> ok
  | Error e -> Error (f e)
;;

let all results =
  List.fold_right results ~init:(Ok []) ~f:(fun elt acc ->
    let* elt = elt in
    let+ acc = acc in
    elt :: acc)
;;

let all_unit results =
  let+ (_ : unit list) = all results in
  ()
;;

let collect_errors results =
  List.fold_right results ~init:(Ok []) ~f:(fun elt acc ->
    match elt, acc with
    | Ok x, Ok xs -> Ok (x :: xs)
    | Ok _, (Error _ as errs) -> errs
    | Error err, Ok _ -> Error (err, [])
    | Error err0, Error (err1, errs) -> Error (err0, err1 :: errs))
  |> function
  | Ok xs -> Ok xs
  | Error (err, []) -> Error err
  | Error (err, (_ :: _ as errs)) -> Error (Syntax_error.combine err errs)
;;

let to_either : _ -> _ Either.t = function
  | Ok ok -> Left ok
  | Error err -> Right err
;;

let combine_errors l =
  let oks, errs = List.partition_map to_either l in
  match errs with
  | [] -> Ok oks
  | _ :: _ -> Error errs
;;
