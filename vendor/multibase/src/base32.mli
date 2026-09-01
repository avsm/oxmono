@@ portable

type alphabet : immutable_data
type sub = string * int * int

val make_alphabet : string -> alphabet
val length_alphabet : alphabet -> int
val alphabet : alphabet -> string
val default_alphabet : alphabet
val default_lower : alphabet
val extended_hex : alphabet
val hex_lower : alphabet

val encode :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string ->
  (string, [> `Msg of string ]) result

val encode_string : ?pad:bool -> ?alphabet:alphabet -> string -> string

val encode_sub :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string ->
  (sub, [> `Msg of string ]) result

val encode_exn :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string -> string

val decode :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string ->
  (string, [> `Msg of string ]) result

val decode_sub :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string ->
  (sub, [> `Msg of string ]) result

val decode_exn :
  ?pad:bool -> ?alphabet:alphabet -> ?off:int -> ?len:int -> string -> string
