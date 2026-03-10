(** Internal JSON utilities for working with Jsont.json values.
    Not exposed in the public API. *)

let none = Jsont.Meta.none

(** {2 Constructors} *)

let str s = Jsont.String (s, none)
let num f = Jsont.Number (f, none)
let int i = Jsont.Number (float_of_int i, none)
let bool b = Jsont.Bool (b, none)
let null = Jsont.Null ((), none)

let obj mems = Jsont.Object (mems, none)
let arr elts = Jsont.Array (elts, none)
let mem name value : Jsont.mem = ((name, none), value)

(** {2 Extractors} *)

let find_member name = function
  | Jsont.Object (mems, _) ->
    (match List.find_opt (fun ((n, _), _) -> n = name) mems with
     | Some (_, v) -> Some v
     | None -> None)
  | _ -> None

let member name json =
  match find_member name json with
  | Some v -> v
  | None -> null

let to_string_exn = function
  | Jsont.String (s, _) -> s
  | _ -> failwith "expected string"

let to_string_opt = function
  | Jsont.String (s, _) -> Some s
  | _ -> None

let to_int_exn = function
  | Jsont.Number (f, _) ->
    if Float.is_integer f then int_of_float f
    else failwith "expected integer"
  | _ -> failwith "expected number"

let to_int_opt = function
  | Jsont.Number (f, _) when Float.is_integer f -> Some (int_of_float f)
  | _ -> None

let to_float_exn = function
  | Jsont.Number (f, _) -> f
  | _ -> failwith "expected number"

let to_float_opt = function
  | Jsont.Number (f, _) -> Some f
  | _ -> None

let to_bool_exn = function
  | Jsont.Bool (b, _) -> b
  | _ -> failwith "expected boolean"

let to_bool_opt = function
  | Jsont.Bool (b, _) -> Some b
  | _ -> None

let to_list_exn = function
  | Jsont.Array (l, _) -> l
  | _ -> failwith "expected array"

let to_list_opt = function
  | Jsont.Array (l, _) -> Some l
  | _ -> None

let to_object_exn = function
  | Jsont.Object (mems, _) -> mems
  | _ -> failwith "expected object"

let is_null = function
  | Jsont.Null _ -> true
  | _ -> false

(** {2 String I/O} *)

let of_string s =
  match Jsont_bytesrw.decode_string Jsont.json s with
  | Ok json -> json
  | Error e -> failwith e

let to_string ?(format = Jsont.Minify) json =
  match Jsont_bytesrw.encode_string ~format Jsont.json json with
  | Ok s -> s
  | Error _ -> "{}"

let to_string_pretty json = to_string ~format:Jsont.Indent json
