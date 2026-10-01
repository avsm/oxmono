(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The words numptyd answers with.

    Every result the model reads is composed here, so that a fetch, a head and a
    run read alike however they were driven, and so that the wording of a
    refusal is written once. A refusal is text of a result rather than an error
    of the protocol: the model is the reader, and a model told what was refused
    and why can act on it. *)

val text_of_html : string -> string
(** [text_of_html s] is the text of the HTML document [s]: the content of its
    script and style elements dropped, its comments dropped, its tags removed,
    the common entities decoded and its whitespace collapsed.

    It reads the markup rather than parsing it. An attribute value holding a [>]
    ends a tag here where a parser would not, and nothing here knows which
    elements are blocks, so the line structure is the source's. This is a
    reduction of a page to the words in it and not a renderer, which is why
    [raw] exists beside it. *)

val fetch :
  status:int ->
  url:string ->
  content_type:string ->
  bytes:int ->
  body:string ->
  reduced:bool ->
  string
(** [fetch ~status ~url ~content_type ~bytes ~body ~reduced] is the answer to a
    fetch that arrived: the status code, the final URL after redirects, the
    content type, the [bytes] that arrived and then [body]. [reduced] says that
    [body] is what {!text_of_html} made of those bytes, which is stated so that
    a reader knows it is not looking at what the server sent.

    A status outside the 2xx range is named as such above the body, since a
    error page returned as though it were the document is the failure an agent
    is least able to detect for itself. *)

val head :
  status:int -> url:string -> content_type:string -> length:int option -> string
(** [head ~status ~url ~content_type ~length] is the answer to a head: the same
    metadata a fetch reports, with the size the server declared in place of the
    body. *)

val refused : url:string -> size:int option -> bound:int -> string
(** [refused ~url ~size ~bound] is the answer to a body over [bound] bytes.
    [size] is what it would have been, where the server declared it or the
    transfer got far enough to know, and [None] where it did not.

    Nothing of the body is returned. A document cut at a bound reads as the
    whole of it, which is what this refusal exists to prevent. *)

val ran :
  program:string ->
  status:Eio.Process.exit_status ->
  output:string ->
  total:int ->
  string
(** [ran ~program ~status ~output ~total] is the answer to a run: how [program]
    ended, then [output], its standard output and standard error interleaved as
    it wrote them. [total] is how many bytes it wrote, which exceeds [output]
    where the bound cut it, and the count of what was dropped is stated rather
    than left to read as the whole. *)

val failed : what:string -> url:string -> code:int -> said:string -> string
(** [failed ~what ~url ~code ~said] is the answer to a curl that could not do
    [what] to [url], being the exit status [code] and [said], curl's own
    standard error. A name that does not resolve and a host that refuses a
    connection both arrive here, and both are the model's to act on. *)

val no_program : program:string -> reason:string -> string
(** [no_program ~program ~reason] is the answer to a run whose program could not
    be started at all, which is usually one that is not on the PATH. *)

val no_curl : string
(** [no_curl] is the answer to a fetch or a head where numptyd found no curl. *)
