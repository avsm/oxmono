(** Normalisation of publisher XML before conversion.

    Publisher APIs sometimes wrap the article in a response envelope that
    converters refuse. This module reduces such documents to the article
    they carry, without interpreting the article itself. *)

val unwrap : string -> (string, string) result
(** [unwrap xml] is the JATS article inside a Springer Nature API response
    whose root element is [response], preceded by an XML declaration, or
    [xml] unchanged when the root is anything else. The response must hold
    exactly one [article] element. Named entities declared by the
    envelope's document type are not carried over, which the Pub2TEI
    service tolerates. Errors describe a malformed or ambiguous envelope. *)
