(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Modules for line-oriented protocols.

    A message is one compact JSON object on one line. A line carries exactly one
    message, and a message never spans two lines. Each protocol supplies its own
    codecs and takes the framing from here: the bound on a line, a reader that
    hands one line to a codec, a writer that ends one with a newline, and the
    scrub that keeps a stray byte from becoming a protocol fault.

    A line ends [\n] on writing, and either [\n] or [\r\n] on reading. *)

val max_line : int
(** [max_line] is the buffer size both ends should give their readers, being 16
    MiB. It bounds a single message, and a source file or a build log that
    reaches it is past what a model can use anyway.

    A line over [max_line] reads as [`Bad], holding a truncated prefix of what
    was seen. That [`Bad] is terminal for the connection. The reader is left
    part way through a line it will never finish, so no later read is
    trustworthy and the peer must be shut down rather than resynchronised. *)

val utf_8 : string -> string
(** [utf_8 s] is [s] with every invalid UTF-8 sequence replaced by [U+FFFD].

    JSON text is UTF-8, and a peer's parser rejects a string that is not. The
    bytes these protocols carry are file contents and command output, which are
    not always valid UTF-8, so a codec passes each string it writes through
    here. Sending it as it stands would turn one stray byte in the user's data
    into a fault that stops both ends. *)

val write : ('a -> string) -> Eio.Buf_write.t -> 'a -> unit
(** [write encode w msg] writes [encode msg] and its terminating newline to [w].
    It does not flush. [encode] is the protocol's own compact encoder and must
    return a single line. *)

val read :
  (string -> 'a option) ->
  Eio.Buf_read.t ->
  [ `Msg of 'a | `Eof | `Bad of string ]
(** [read decode r] reads one line from [r]. It is [`Msg msg] where [decode]
    accepted the line, [`Eof] if [r] holds no further line, and [`Bad line] for
    anything else, where [line] is the offending text without its newline. Give
    [r] a buffer of {!max_line}.

    A bad line is a protocol fault rather than bad input, and the caller is
    expected to report it and stop. A bad line is never skipped, since a stream
    of sequential calls that loses a line goes on answering the wrong question.

    [`Bad] carries the whole offending line, and a call carrying a source file
    or a result carrying build output is long. Truncate it before it reaches a
    log or a model. *)
