open! Core

type t =
  { namespace : string option [@sexp.option]
  ; name : string
  }
[@@deriving sexp_of, compare]

let create ~namespace ~name =
  { namespace = (if String.is_empty namespace then None else Some namespace); name }
;;

let of_qualified_name_exn { Qualified_name.prefix; name } ~prefixes =
  let namespace =
    let%map.Option prefix in
    match Map.find prefixes prefix with
    | Some prefix -> prefix
    | None -> raise_s [%message "unknown prefix" (prefix : string)]
  in
  { namespace; name }
;;

include Comparable.Make_plain (struct
    type nonrec t = t [@@deriving sexp_of, compare]
  end)
