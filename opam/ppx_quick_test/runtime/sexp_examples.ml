open! Core

module One = struct
  type t =
    { sexp_string : string
    ; body : string
    ; body_location : Location.t
    ; delimiters : string option
    }
end

type t =
  | Ignore
  | Empty of Location.t
  | List of One.t Nonempty_list.t

let choose_tag_for str =
  let rec loop len =
    let tag = String.make len 'x' in
    let delimiters = [ sprintf "{%s|" tag; sprintf "|%s}" tag ] in
    if List.exists delimiters ~f:(fun delim -> String.is_substring str ~substring:delim)
    then loop (len + 1)
    else tag
  in
  loop 0
;;

let string_to_ocaml_string str =
  let tag = choose_tag_for str in
  [%string "{%{tag}|%{str}|%{tag}}"]
;;

let get_parsed_examples t params =
  match t with
  | Ignore | Empty _ -> Ok []
  | List ones ->
    let tup_of_sexp = unstage (Params.tup_of_sexp params) in
    Nonempty_list.to_list ones
    |> List.map ~f:(fun one ->
      match Sexp.of_string one.sexp_string |> tup_of_sexp with
      | value -> Ok value
      | exception e ->
        Or_error.error_s
          [%message
            "" ~sexp_example_string:(one.sexp_string : string) ~raised_exception:(e : exn)])
    |> Or_error.all
;;

let insertion_for_new_example t (new_example : Sexp.t)
  : File_corrections.Insertion.t option
  =
  let new_example_str = new_example |> Sexp.to_string_hum |> string_to_ocaml_string in
  let value = [%string " %{new_example_str}"] in
  match t with
  | Ignore -> None
  | Empty location ->
    let placement_cnum = location.loc_end.pos_cnum in
    Some { placement_cnum; value }
  | List examples ->
    let last = Nonempty_list.last examples in
    let placement_cnum =
      last.body_location.loc_end.pos_cnum
      +
      match last.delimiters with
      | None -> String.length {|"|}
      | Some delimiter -> String.length [%string "|%{delimiter}}"]
    in
    Some { placement_cnum; value }
;;

module For_testing = struct
  let string_to_ocaml_string = string_to_ocaml_string
end
