open Core

type t =
  [ `Null
  | `True
  | `False
  | `String of string
  | `Number of string
  | `Object of (string * t) list
  | `Array of t list
  ]
  constraint t = Jsonaf.t
[@@deriving sexp]

let of_string s =
  match Jsonaf.parse s with
  | Ok json -> json
  | Error error -> raise_s [%message "Json.of_string: parse error" (error : Error.t)]
;;

let to_string t = Jsonaf.to_string t

let typeof = function
  | `Object _ -> "object"
  | `True | `False -> "bool"
  | `Number _ -> "number"
  | `Array _ -> "array"
  | `Null -> "null"
  | `String _ -> "string"
;;

let type_error json ~expected =
  let got = typeof json in
  raise_s [%message "JSON type error" (expected : string) (got : string)]
;;

let as_assoc = function
  | `Object assoc -> assoc
  | json -> raise_s (type_error json ~expected:"object")
;;

let as_number = function
  | `Number number -> number
  | json -> raise_s (type_error json ~expected:"number")
;;

let as_int json =
  let number = as_number json in
  try Int.of_string number with
  | _exn ->
    (* JSON does not have a definition of integers in itself. However, JSON schema (used
       to impose structure on JSON documents) defines "integer" as "any number with a zero
       fractional part". This means all of the following should be considered integers:

       - [0] (obvious)
       - [1.0] (equal to [1])
       - [2.1e1] (equal to [21])

       Using [Float.is_integer] should at least approximate this behavior. (We still try
       [Int.of_string] first to avoid float precision issues at high values if possible.)
    *)
    let number =
      try Float.of_string number with
      | exn -> raise_s [%message "Json.as_int: unable to parse number" (exn : exn)]
    in
    (match Float.is_integer number with
     | false -> raise_s [%message "Json.as_int: number had non-zero fractional part"]
     | true -> Float.iround_towards_zero_exn number)
;;

let as_float json =
  let number = as_number json in
  try Float.of_string number with
  | exn ->
    raise_s [%message "Json.as_float: unable to convert number to float" (exn : exn)]
;;

let as_string = function
  | `String string -> string
  | json -> raise_s (type_error json ~expected:"string")
;;

let as_bool = function
  | `True -> true
  | `False -> false
  | json -> raise_s (type_error json ~expected:"bool")
;;

let as_list = function
  | `Array list -> list
  | json -> raise_s (type_error json ~expected:"array")
;;

let as_option = function
  | `Null -> None
  | x -> Some x
;;

let keys json = as_assoc json |> List.map ~f:fst

let member name = function
  | `Object assoc -> List.Assoc.find assoc name ~equal:String.equal
  | json -> raise_s (type_error json ~expected:"object")
;;
