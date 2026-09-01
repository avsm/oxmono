open Core
open State

val default_client_hello : Config.config ->
  (client_hello * tls_version * (group * dh_secret) list)
val default_client_hello_with_rng : g:Mirage_crypto_rng.portable_g ->
  Config.config -> (client_hello * tls_version * (group * dh_secret) list)
  @@ portable
val handle_change_cipher_spec : client_handshake_state -> handshake_state -> string -> (handshake_return, failure) result
  @@ portable
val handle_handshake : client_handshake_state -> handshake_state -> string -> (handshake_return, failure) result
val handle_handshake_with_rng : g:Mirage_crypto_rng.portable_g ->
  client_handshake_state -> handshake_state -> string ->
  (handshake_return, failure) result @@ portable
val answer_hello_request : handshake_state -> (handshake_return, failure) result
