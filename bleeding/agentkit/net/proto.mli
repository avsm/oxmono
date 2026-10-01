(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The line protocol between numpty and numptyd.

    numpty spawns numptyd before it loads a model and speaks to it over its
    pipes. A message is one compact JSON object on one line, in both directions.
    A line carries exactly one message, and a message never spans two lines. The
    object holds a single member, whose name is the message and whose value
    holds the fields.

    {v
    <- {"hello":{"status":"numpty: curl 8.7.1","curl":true}}
    -> {"call":{"id":1,"op":"fetch","url":"https://x/","render":"text"}}
    <- {"trace":{"id":1,"line":"fetch: https://x/"}}
    <- {"result":{"id":1,"output":"200 https://x/ …"}}
    -> {"shutdown":{}}
    v}

    Both ends are the same binary, so the wire carries no version and no schema.
    Reading therefore refuses an operation it does not know, and ignores a
    member it did not ask for, since neither can come from a peer of the same
    build.

    The framing is {!Agentkit.Line}, which is where a line's bound, the meaning
    of a line that does not parse, and the treatment of a string that is not
    valid UTF-8 are all decided. What is here is the codecs. *)

type hello = {
  status : string;  (** The note a person or a journal is shown. *)
  curl : bool;
      (** Whether a curl answered, and so whether {!Fetch} and {!Head} have
          anything to run. *)
}
(** The greeting numptyd sends once, before any result. *)

(** How a fetched body is reported. *)
type render =
  | Text
      (** HTML reduced to its text: script and style content dropped, tags
          removed, the common entities decoded and whitespace collapsed. It is a
          reduction and not a renderer. A body that is not HTML is returned as
          it stands. *)
  | Raw  (** The bytes as they arrived. *)

(** An operation a call asks numptyd to perform. *)
type op =
  | Fetch of { url : string; render : render; max_bytes : int option }
      (** Fetch [url] and report its status, its final URL, its content type,
          its byte count and then its body as [render] says. A body over
          [max_bytes] is refused rather than truncated. [None] takes numptyd's
          own default bound. *)
  | Head of { url : string }
      (** Ask for [url]'s headers alone and report the same metadata with no
          body, for checking whether something moved. *)
  | Run of { program : string; args : string list }
      (** Run [program] with [args] and report its exit status and its combined
          output. No shell is involved, so nothing in [args] is expanded. *)

type call = {
  id : int;  (** Identifies the call in the traces and the result. *)
  op : op;  (** What to do. *)
}
(** A request for one operation. Calls are sequential, one in flight. *)

type trace = {
  id : int option;  (** The call being run, if a call is running. *)
  line : string;  (** One line of progress. *)
}
(** A line of progress, streamed while a call runs. *)

type result = {
  id : int;  (** The call being answered. *)
  output : string;  (** The tool's text, ordinary output and refusals alike. *)
}
(** The answer to a call. *)

(** A message numpty sends numptyd. *)
type to_server =
  | Call of call  (** Perform an operation. *)
  | Shutdown  (** Stop. *)

(** A message numptyd sends numpty. *)
type to_client =
  | Hello of hello  (** Sent once, before any result. *)
  | Trace of trace  (** Progress on the call in flight. *)
  | Result of result  (** The answer to a call. *)

val name : op -> string
(** [name op] is the word the wire uses for [op], which is what a trace, a
    journal record and a timeout message call it. *)

val write_to_server : Eio.Buf_write.t -> to_server -> unit
(** [write_to_server w msg] writes [msg] and its terminating newline to [w]. It
    does not flush. *)

val read_to_server :
  Eio.Buf_read.t -> [ `Msg of to_server | `Eof | `Bad of string ]
(** [read_to_server r] reads one line from [r]. It is [`Msg msg] for a message
    numpty may send, [`Eof] if [r] holds no further line, and [`Bad line] for
    anything else, where [line] is the offending text without its newline. Give
    [r] a buffer of {!Agentkit.Line.max_line}. *)

val write_to_client : Eio.Buf_write.t -> to_client -> unit
(** [write_to_client w msg] writes [msg] and its terminating newline to [w]. It
    does not flush. *)

val read_to_client :
  Eio.Buf_read.t -> [ `Msg of to_client | `Eof | `Bad of string ]
(** [read_to_client r] is [read_to_server] for the messages numptyd sends. *)
