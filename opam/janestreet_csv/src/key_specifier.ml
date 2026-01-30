open Core

type t = String.Set.t

exception Invalid_specifier of string

let to_string_set t = t

(* An int specifier looks like 1,3,5-7. This function converts to a col_specifier. *)
let int_specifier_of_string spec =
  let validation_pattern = "\\d+(-\\d+)?(,\\d+(-\\d+)?)*" in
  if not (Pcre.pmatch ~pat:validation_pattern spec)
  then raise (Invalid_specifier "Column specifier formatted incorrectly")
  else (
    let segments = Pcre.split ~pat:"," spec in
    let convert_segment segment =
      let endpoints = Pcre.split ~pat:"-" segment in
      let range first last =
        let rec aux last accum =
          if last < first then accum else aux (last - 1) (last :: accum)
        in
        aux last []
      in
      match endpoints with
      | [ h ] -> [ int_of_string h ]
      | [ first; last ] -> range (int_of_string first) (int_of_string last)
      | _ -> raise (Invalid_specifier "Column specifier formatted incorrectly")
    in
    List.map segments ~f:convert_segment
    |> List.fold ~init:String.Set.empty ~f:(fun accum col_list ->
      List.fold col_list ~init:accum ~f:(fun accum col ->
        let col = string_of_int col in
        if Set.mem accum col
        then
          raise
            (Invalid_specifier ("Invalid column specifier: column " ^ col ^ " duplicated."))
        else Set.add accum col)))
;;

(* A specifier string looks like f1,f3,f5,f7. *)
let specifier_of_string spec =
  let fields = Pcre.split ~pat:"," spec in
  List.fold fields ~init:String.Set.empty ~f:(fun set field ->
    if Set.mem set field
    then raise (Invalid_specifier ("Column " ^ field ^ " duplicated"))
    else Set.add set field)
;;
