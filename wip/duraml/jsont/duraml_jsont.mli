(** JSON codecs for the typed Duraml client.

    {1 NAME}
    Duraml_jsont - optional Jsont adapter for [duraml.client].

    {1 SYNOPSIS}
    {[
      let queue = Duraml_client.Queue.v store ~name:"numbers" ~kind:"square"
        ~version:1 ~request:(Duraml_jsont.codec Jsont.int)
        ~result:(Duraml_jsont.codec Jsont.int)
    ]}

    The core and client do not depend on a JSON library. Link [duraml.jsont]
    only when application payloads use Jsont. *)

val codec : 'a Jsont.t -> 'a Duraml_client.Codec.t
(** [codec json] encodes and decodes UTF-8 JSON using [json]. Encoding errors
    raise [Invalid_argument] with a fixed diagnostic. Decode errors are returned
    to the client. The runner records a fixed [invalid_payload] failure rather
    than copying decoder diagnostics, which may contain input, into storage. *)
