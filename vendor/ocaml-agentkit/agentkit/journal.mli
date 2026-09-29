(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** An append-only account of what an agent did.

    One JSON object per line, in a directory of segments named by UTC date. Four
    members are on every record: [v], the schema version, [seq], a gapless
    counter over the whole journal, [t], the time in RFC 3339 UTC, and [run],
    which names the process run. A fifth member names the kind and carries its
    fields.

    A record is appended and fsynced before the thing it describes is allowed to
    proceed. {!append} raises if it cannot write, and the caller is expected to
    stop the run rather than continue unlogged. A trace with a hole in it is
    worse than no trace, because it reads as complete.

    Reading is forward compatible in one direction. A kind this build does not
    know decodes as {!Unknown} carrying its raw JSON, so a later journal still
    shows everything in it. A [v] above {!schema_version} is an error, since a
    record whose common members may have moved cannot be shown honestly.

    There is no counter file. {!recover} reads the last line of the newest
    segment for the next [seq] and [run]. A counter kept beside the journal
    could disagree with it after a crash between the two writes. *)

val schema_version : int
(** [schema_version] is the schema this build writes and the highest it reads,
    being 1. *)

(** {1 Kinds} *)

type run_start = {
  pid : int;  (** the process id of the run *)
  version : string;  (** the version of the program that wrote the record *)
  backend : string;  (** the inference backend it linked *)
  model : string;  (** the path of the model it loaded *)
  ctx_size : int;  (** the context window an agent starts with *)
}
(** What a run was, recorded before it does anything else. *)

type wake = {
  task : string;  (** the id of the scheduled task that fired *)
  due : string;  (** when it was due, in RFC 3339 UTC *)
  why : string;  (** why it fired now, such as [due], [missed] or [run_now] *)
  serial : int option;
      (** the [run_now] serial that caused it, when that is why *)
}
(** A scheduled task firing. This is the only record of a [once] task having
    run, and of a [run_now] serial having been honoured, so the schedule file
    needs no state written back into it. *)

type brief = {
  version : int;  (** the memory version the brief was assembled from *)
  open_items : int;  (** how many [open_item] entries it carried *)
  bytes : int;  (** how large the assembled brief was *)
}
(** The first message of a wake-up, described rather than quoted. The prompt
    that follows carries its text. *)

type tool_call = {
  call : int;  (** the call id, which pairs this with its result *)
  name : string;  (** the tool the model asked for *)
  arguments : string;
      (** the canonical JSON arguments supplied by the adapter *)
}
(** A tool call, recorded before it is made. *)

type tool_result = {
  call : int;  (** the call id of the request this answers *)
  name : string;  (** the tool that answered *)
  output : string;  (** what it returned *)
  seconds : float;  (** how long it took *)
  truncated : bool;  (** whether the model saw less than [output] *)
}
(** A tool result, recorded before it reaches the model. *)

type continued = {
  task : string;  (** the task both sessions worked on *)
  session : int;  (** the ordinal of the session that starts here *)
  previous : int;  (** the ordinal of the session it succeeded *)
}
(** One session succeeding another on the same task, after a handover taken
    because the context was filling. A task spanning five sessions reads as one
    task through these. *)

type memory_write = {
  from : int;  (** the version in force before the write *)
  to_ : int;  (** the version it minted *)
  entry : string;  (** the id of the entry that changed *)
  why : string;  (** what the write was for *)
}
(** A memory version being minted. It is appended after the snapshot is fsynced
    and before [current] is moved, so a snapshot no [memory_write] names was
    never in force. *)

type error = {
  where : string;  (** what was being done *)
  what : string;  (** what went wrong *)
}
(** Something that failed. *)

type schedule_load = {
  tasks : string list;  (** the task ids read, in file order *)
  changed : string list;  (** the ids that differ from the last read *)
}
(** The schedule file being read, so that a person's edit is in the trace like
    everything else. *)

type cut_off = {
  tokens : int;  (** the ceiling the reply stopped at *)
  tool_call : bool;
      (** whether a tool call was being written, and so was discarded *)
}
(** A reply generation stopped rather than the model. What the model wrote of a
    discarded call is in the [content] record before this one, since an account
    should hold what was written even where the conversation does not. *)

type unknown = {
  name : string;  (** the member that named the kind *)
  json : Jsont.json;  (** its value, exactly as it was read *)
}
(** A kind this build does not know. It is carried rather than dropped, so a
    later journal read by an earlier build still shows every record in it. *)

(** What a record says. The constructor carrying a bare value names its one
    member in the JSON: [Run_stop] is [why], [Prompt], [Reasoning] and [Content]
    are [text], [Expanded] is [ctx_size], [Squeezed] is [tokens], and [Handover]
    is [version]. *)
type kind =
  | Run_start of run_start
  | Run_stop of string  (** why the run stopped *)
  | Wake of wake
  | Brief of brief
  | Prompt of string  (** what was sent to the model *)
  | Reasoning of string  (** the model's reasoning, when thinking is on *)
  | Content of string  (** the model's reply *)
  | Tool_call of tool_call
  | Tool_result of tool_result
  | Stats of Agent.stats  (** what the turn had cost *)
  | Expanded of int  (** the context grew to this many tokens *)
  | Squeezed of int  (** the turn had only this many tokens to reply in *)
  | Cut_off of cut_off
  | Compacted of Agent.compaction
      (** the conversation was replaced by a summary the model wrote *)
  | Continued of continued
  | Memory_write of memory_write
  | Handover of int  (** the memory version the wake-up finished at *)
  | Error of error
  | Schedule_load of schedule_load
  | Unknown of unknown

val kind_name : kind -> string
(** [kind_name k] is the member name [k] is written under, such as
    ["tool_call"]. For {!Unknown} it is the name that was read. *)

val kind_names : string list
(** [kind_names] are the names this build writes, in the order the kinds are
    declared above. It is what a command filtering on a kind checks its argument
    against, since a misspelled name would otherwise match nothing and read as a
    journal with nothing in it. A journal written by a later build may hold a
    name that is not here, which {!Unknown} carries. *)

(** {1 Records} *)

type record = {
  v : int;  (** the schema version the record was written to *)
  seq : int;  (** its place in the journal, counting from 1 *)
  time : string;  (** when it was written, in RFC 3339 UTC *)
  run : int;  (** the run that wrote it *)
  kind : kind;
}
(** One line of the journal. *)

val jsont : record Jsont.t
(** [jsont] is the codec, which is the schema. Nothing writes a record by hand.
*)

val to_string : record -> string
(** [to_string r] is the one compact JSON line for [r], without its newline.
    Every string in [r] passes through {!Line.utf_8}, since a tool result and a
    model reply carry bytes that are not valid UTF-8 and a journal that refused
    them would lose the record. *)

val of_string : string -> (record, string) result
(** [of_string s] decodes one journal line, or says why it could not. A [v]
    above {!schema_version} is one of the reasons. *)

val stamp : now:float -> run:int -> seq:int -> kind -> record
(** [stamp ~now ~run ~seq k] is the record for [k] at POSIX time [now], numbered
    [seq] within run [run]. It writes nothing and consumes no number.

    It is for a command that streams its account to a sink rather than to a
    store, such as a one-shot run writing journal lines to standard error, where
    there is no {!t} to take the time and the numbering from. A command with a
    store uses {!append}, which stamps and writes in one step. *)

val rfc3339 : float -> string
(** [rfc3339 t] is the POSIX time [t] as RFC 3339 in UTC, to the second. This is
    the format of a record's [t] and of a memory version's. *)

val utc_date : float -> string
(** [utc_date t] is the [YYYY-MM-DD] UTC date of the POSIX time [t], which is
    what a segment is named after. *)

(** {1 Writing} *)

type t
(** A journal open for appending. *)

exception Locked of { path : string; pid : int }
(** Raised when another process, or another writer in this process, owns the
    journal at [path]. *)

val open_ : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> _ Eio.Path.t -> t
(** [open_ ~sw ~clock dir] takes exclusive ownership of [dir], recovers its next
    run and sequence numbers, and opens a journal for that run. It raises
    {!Locked} when another writer owns the directory. The lock is released when
    [sw] finishes. *)

val create :
  sw:Eio.Switch.t ->
  clock:_ Eio.Time.clock ->
  run:int ->
  seq:int ->
  _ Eio.Path.t ->
  t
(** [create ~sw ~clock ~run ~seq dir] is the journal in [dir], which is created
    if it is absent. [run] names this run in every record, and [seq] is the
    number the next record takes. Take both from {!recover}.

    No segment is opened until the first {!append}, so a directory that cannot
    be written to fails there rather than here. Segments opened later are closed
    when [sw] finishes. This lower-level operation does not lock [dir]. Use
    {!open_} unless a surrounding store already provides exclusive ownership. *)

val append : t -> kind -> record
(** [append t k] appends a record of kind [k] and fsyncs it, then returns what
    was written.

    It raises whatever the write raised, and the run must then stop. It does not
    report a failure as a value, since a caller that ignores one goes on
    producing an account that reads as complete. The sequence number is not
    consumed by a failed append.

    A record whose UTC date differs from the open segment's rolls to a new
    segment, and [seq] carries across the roll. *)

val next_seq : t -> int
(** [next_seq t] is the number the next {!append} will use. A memory snapshot
    names the journal record that caused it, and it is written before that
    record, so this is how it learns the number. *)

val run : t -> int
(** [run t] is the run number [t] stamps on every record. *)

val close : t -> unit
(** [close t] closes the open segment. Appending afterwards opens it again, so
    this releases its file descriptors and any lock held by {!open_}. A locked
    journal cannot be appended after it is closed. *)

(** {1 Reading} *)

type next = {
  next_seq : int;  (** the number the next record should take *)
  next_run : int;  (** the number a new run should take *)
}
(** Where a journal has got to. *)

val recover : _ Eio.Path.t -> next
(** [recover dir] reads the last record of the newest segment of [dir]. Both
    numbers are 1 when [dir] is absent or holds no record. It raises
    {!Bad_record} if the last record does not decode, since a journal whose end
    cannot be read cannot say where to carry on from. *)

exception Bad_record of { segment : string; line : int; msg : string }
(** Raised by the readers for a line that does not decode, naming the segment,
    the line number counting from 1, and what was wrong. A bad line stops the
    read rather than being skipped, since a skipped line is a hole in an account
    that would otherwise read as complete. *)

val segments : _ Eio.Path.t -> string list
(** [segments dir] are the names of the segment files in [dir], oldest first. It
    is the empty list when [dir] is absent. A file that is not named for a UTC
    date is not a segment and is not returned. *)

val iter_segment :
  ?kinds:string list -> _ Eio.Path.t -> string -> (record -> unit) -> unit
(** [iter_segment dir name f] applies [f] to each record of the segment [name]
    in [dir], in order. [kinds] restricts it to records whose {!kind_name} is in
    the list, which is what a digest or a filtered log wants. Every line is
    still decoded, so a bad one is still reported. *)

val iter : ?kinds:string list -> _ Eio.Path.t -> (record -> unit) -> unit
(** [iter dir f] applies [f] to every record in [dir], oldest segment first, so
    the order is the order they were written in. [kinds] is as {!iter_segment}.
*)
