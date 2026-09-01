@@ portable

type alphabet : immutable_data

exception Invalid_alphabet
exception Invalid_base58_character

val default_alphabet : alphabet
val flickr_alphabet : alphabet
val make_alphabet : string -> alphabet
val encode : ?alphabet:alphabet -> string -> string
val decode : ?alphabet:alphabet -> string -> string
