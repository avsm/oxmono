(** Shared HTTP byte and field syntax. This library is private to [httpz]. *)

val[@zero_alloc] is_token_char : char# -> bool @@ portable
val[@zero_alloc] is_space : char# -> bool @@ portable
val[@zero_alloc] is_field_value_char : char# -> bool @@ portable
val[@zero_alloc] is_qdtext_char : char# -> bool @@ portable
val[@zero_alloc] is_quoted_pair_char : char# -> bool @@ portable
val[@zero_alloc] is_digit : char# -> bool @@ portable
val[@zero_alloc] digit_value : char# -> int @@ portable
val[@zero_alloc] to_lower : char# -> char# @@ portable

val[@zero_alloc] is_token : local_ string -> bool @@ portable
val[@zero_alloc] is_token_sub : local_ string -> pos:int -> len:int -> bool @@ portable
val[@zero_alloc] is_field_value : local_ string -> bool @@ portable
val[@zero_alloc] is_quoted_string_sub :
  local_ string -> pos:int -> len:int -> bool @@ portable
val quote_string : local_ string -> string @@ portable
val unquote_string : local_ string -> string option @@ portable
val[@zero_alloc] qvalue_sub : local_ string -> pos:int -> len:int -> int @@ portable

(** The following helpers require valid substring bounds. *)
val[@zero_alloc] skip_space : local_ string -> int -> int -> int @@ portable
val[@zero_alloc] trim_space : local_ string -> int -> int -> int @@ portable
val[@zero_alloc] equal_ci :
  local_ string -> int -> local_ string -> int -> int -> bool @@ portable
