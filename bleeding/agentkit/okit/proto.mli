(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The line protocol between humpty and okitd.

    Humpty spawns okitd and speaks to it over its pipes. A message is one
    compact JSON object on one line, in both directions. A line carries exactly
    one message, and a message never spans two lines. The object holds a single
    member, whose name is the message and whose value holds the fields.

    {v
    <- {"hello":{"status":"okit: dune tools active","dune":true,"merlin":true}}
    -> {"call":{"id":1,"op":"build","targets":"."}}
    <- {"trace":{"id":1,"line":"dune: build ."}}
    <- {"result":{"id":1,"output":"build ok"}}
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
  status : string;  (** The note the interface shows. *)
  dune : bool;  (** Whether the dune tools answer. *)
  merlin : bool;  (** Whether the merlin tools answer. *)
}
(** The greeting okitd sends once, after its session is up. *)

(** An operation a call asks okitd to perform. The merlin operations carry the
    source text, since okitd does not read the caller's buffers. *)
type op =
  | Build of { targets : string }  (** Build [targets]. *)
  | Test  (** Run the test suite. *)
  | Promote of { path : string }  (** Promote the diff for [path]. *)
  | Project of { module_ : string }
      (** Describe the project, or the component holding module [module_] when
          it is not empty. *)
  | After_write of { path : string; verb : string }
      (** The diagnostics for [path], as [write] and [edit] append them. [verb]
          is how the tool that saved the file names what it did, and okitd
          traces the call with it, so a person watching sees the call that was
          made rather than the one it is built like. *)
  | Outline of { path : string; source : string }  (** Outline [source]. *)
  | Errors of { path : string; source : string }
      (** What merlin objects to in [source]. *)
  | Type_at of { path : string; source : string; line : int; col : int }
      (** The type at [line] and [col] of [source]. *)
  | Locate of { path : string; source : string; line : int; col : int }
      (** The definition of the name at [line] and [col] of [source]. *)
  | Occurrences of { path : string; source : string; line : int; col : int }
      (** Every use of the name at [line] and [col] of [source]. *)
  | Search of { path : string; source : string; query : string; limit : int }
      (** At most [limit] values whose type is close to [query], searched in the
          configuration [path] and [source] supply. *)
  | Complete of {
      path : string;
      source : string;
      line : int;
      col : int;
      prefix : string;
    }
      (** What can be named at [line] and [col] of [source] beginning with
          [prefix]. *)
  | Bash of { command : string }  (** Run [command]. *)

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

(** A message humpty sends okitd. *)
type to_server =
  | Call of call  (** Perform an operation. *)
  | Shutdown  (** Stop. *)

(** A message okitd sends humpty. *)
type to_client =
  | Hello of hello  (** Sent once, before any result. *)
  | Trace of trace  (** Progress on the call in flight. *)
  | Result of result  (** The answer to a call. *)

val write_to_server : Eio.Buf_write.t -> to_server -> unit
(** [write_to_server w msg] writes [msg] and its terminating newline to [w]. It
    does not flush. *)

val read_to_server :
  Eio.Buf_read.t -> [ `Msg of to_server | `Eof | `Bad of string ]
(** [read_to_server r] reads one line from [r]. It is [`Msg msg] for a message
    humpty may send, [`Eof] if [r] holds no further line, and [`Bad line] for
    anything else, where [line] is the offending text without its newline. Give
    [r] a buffer of {!Agentkit.Line.max_line}. *)

val write_to_client : Eio.Buf_write.t -> to_client -> unit
(** [write_to_client w msg] writes [msg] and its terminating newline to [w]. It
    does not flush. *)

val read_to_client :
  Eio.Buf_read.t -> [ `Msg of to_client | `Eof | `Bad of string ]
(** [read_to_client r] is [read_to_server] for the messages okitd sends. *)
