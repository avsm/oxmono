val authenticator :
  ?crls:X509.CRL.t list ->
  ?allowed_hashes:Digestif.hash' list ->
  unit ->
  (X509.Authenticator.t, [> `Msg of string ]) result
(** [authenticator ~crls ~allowed_hashes ()] detects the root CAs (trust
    anchors) in the operating system's trust store using {!trust_anchors}. It
    constructs an authenticator with the current timestamp {!Ptime_clock.now},
    and the provided [~crls] and [~allowed_hashes] arguments. The resulting
    authenticator can be used for {!Tls.Config.client}. Returns [Error `Msg msg]
    if detection did not succeed. *)

val system_authenticator :
  unit ->
  (X509.Authenticator.t, [> `Msg of string ]) result @ portable
(** [system_authenticator ()] loads the operating system trust anchors and
    constructs the usual HTTPS authenticator. Unlike {!authenticator}, it has
    no CRL or digest-policy customization, so its successful result is
    portable and can be captured by a cross-domain client capability. The
    loading operation itself is ordinary because it performs platform-specific
    process and file-system discovery. *)

val trust_anchors : unit -> (string, [> `Msg of string ]) result
(** [trust_anchors ()] detects the root CAs (trust anchors) in the operating
    system's trust store. Additional CAs can be provided by setting the
    environment variable [OCAML_EXTRA_CA_CERTS] to a filename containing
    pem-encoded X509 certificates.

    On Unix systems, if the environment variable [SSL_CERT_FILE] is set, its
    value is used as path to the system trust anchors. Otherwise, if
    [NIX_SSL_CERT_FILE] is set, its value is used.

    The successful result is a list of pem-encoded X509 certificates. *)
