let ( let* ) = Result.bind

type t = ?ip:Ipaddr.t -> host:[`host] Domain_name.t option ->
  Certificate.t list -> Validation.r

(* XXX
   * Authenticator just hands off a list of certs. Should be indexed.
   * *)
let chain_of_trust ~time ?crls ?(allowed_hashes = Validation.sha2) cas =
  let revoked = match crls with
    | None -> None
    | Some crls -> Some (Crl.is_revoked crls ~allowed_hashes)
  in
  fun ?ip ~host certificates ->
    Validation.verify_chain_of_trust ?ip ~host ~time ?revoked ~allowed_hashes
      ~anchors:cas certificates

module Iarray = Stdlib_stable.Iarray

let chain_of_trust_no_crl ~(time @ portable) cas : t @ portable =
  let anchors_der =
    Iarray.of_list (List.map Certificate.encode_der cas)
  in
  fun ?ip ~host certificates ->
    let anchors =
      Iarray.fold_right
        (fun der anchors ->
           match Certificate.decode_der der with
           | Ok cert -> cert :: anchors
           | Error (`Msg msg) ->
             invalid_arg ("invalid encoded trust anchor: " ^ msg))
        anchors_der []
    in
    let allowed_hashes = [ `SHA256; `SHA384; `SHA512 ] in
    Validation.verify_chain_of_trust ?ip ~host ~time ~allowed_hashes
      ~anchors certificates

let key_fingerprint ~time ~hash ~fingerprint =
  fun ?ip ~host certificates ->
    Validation.trust_key_fingerprint ?ip ~host ~time ~hash ~fingerprint certificates

let cert_fingerprint ~(time @ portable) ~hash ~fingerprint : t @ portable =
  fun ?ip ~host certificates ->
    Validation.trust_cert_fingerprint ?ip ~host ~time ~hash ~fingerprint certificates

let hash_of_string = function
  | "sha224" -> Ok `SHA224
  | "sha256" -> Ok `SHA256
  | "sha384" -> Ok `SHA384
  | "sha512" -> Ok `SHA512
  | hash -> Error (`Msg (Fmt.str "Unknown hash algorithm %S" hash))

let fingerprint_of_string s =
  let* d =
    Result.map_error
      (function `Msg m ->
         `Msg (Fmt.str "Invalid base64 encoding in fingerprint (%s): %S" m s))
      (Base64.decode ~pad:false s)
  in
  Ok d

let format =
  {|
The format of an authenticator is:
- [none]: no authentication
- [key-fp(:<hash>?):<base64-encoded fingerprint>]: to authenticate a peer via
  its key fingerprintf (hash is optional and defaults to SHA256)
- [cert-fp(:<hash>?):<base64-encoded fingerprint>]: to authenticate a peer via
  its certificate fingerprint (hash is optional and defaults to SHA256)
- [trust-anchor(:<base64-encoded DER certificate>)+] to authenticate a peer from
  a list of certificates (certificate must be in PEM format witthout header and
  footer (----BEGIN CERTIFICATE----) and without newlines).
|}

let of_string str =
  begin match String.split_on_char ':' str with
  | [ "key-fp" ; hash ; tls_key_fingerprint ] ->
    let* hash = hash_of_string (String.lowercase_ascii hash) in
    let* fingerprint = fingerprint_of_string tls_key_fingerprint in
    Ok (fun time -> key_fingerprint ~time ~hash ~fingerprint)
  | [ "key-fp" ; tls_key_fingerprint ] ->
    let* fingerprint = fingerprint_of_string tls_key_fingerprint in
    Ok (fun time -> key_fingerprint ~time ~hash:`SHA256 ~fingerprint)
  | [ "cert-fp" ; hash ; tls_cert_fingerprint ] ->
    let* hash = hash_of_string (String.lowercase_ascii hash) in
    let* fingerprint = fingerprint_of_string tls_cert_fingerprint in
    Ok (fun time -> cert_fingerprint ~time ~hash ~fingerprint)
  | [ "cert-fp" ; tls_cert_fingerprint ] ->
    let* fingerprint = fingerprint_of_string tls_cert_fingerprint in
    Ok (fun time -> cert_fingerprint ~time ~hash:`SHA256 ~fingerprint)
  | "trust-anchor" :: certs ->
    let* anchors =
      List.fold_left (fun acc s ->
          let* acc = acc in
          let* der = Base64.decode ~pad:false s in
          let* cert = Certificate.decode_der der in
          Ok (cert :: acc))
        (Ok []) certs
    in
    Ok (fun time -> chain_of_trust ~time (List.rev anchors))
  | [ "none" ] -> Ok (fun _ ?ip:_ ~host:_ _ -> Ok None)
  | _ -> Error (`Msg (Fmt.str "Invalid TLS authenticator: %S" str))
  end |> Result.map_error (function `Msg e -> `Msg (e ^ format))
