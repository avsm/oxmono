open! Core
open! Import

module Naming_scheme : sig
  type t =
    [ `Numbered
    | `Timestamped
    | `Dated
    | `User_defined of (module Rotation_id.S)
    ]
end

(** Description of boundaries for file rotation.

    If all fields are [None] the file will never be rotated. Any field set to [Some] will
    cause rotation to happen when that boundary is crossed. Multiple boundaries may be
    set. Log rotation always causes incrementing rotation conditions (e.g., size) to
    reset.

    The condition [keep] is special and does not follow the rules above. When a log is
    rotated, [keep] is examined and logs that do not fall under its instructions are
    deleted. This deletion takes place on rotation only, and so may not happen. The
    meaning of keep options are:

    - [`All] -- never delete
    - [`Newer_than span] -- delete files with a timestamp older than
      [Time.sub (Time.now ()) span]. This normally means keeping files that contain at
      least one message logged within [span]. If [span] is short enough this option can
      delete a just-rotated file.
    - [`At_least i] -- keep the [i] most recent files

    Log rotation does not support symlinks, and you're encouraged to avoid them in
    production applications. Issues with symlinks:

    - You can't tail symlinks without being careful (e.g., you must remember to pass [-F]
      to [`tail`]).
    - Symlinks are hard to reason about when the program crashes, especially on startup
      (i.e., is the symlink pointing me at the right log file?).
    - Atomicity is hard.
    - Symlinks encourage tailing, which is a bad way to communicate information.
    - They complicate archiving processes (the symlink must be skipped). *)
type t =
  { messages : int option
  ; size : Byte_units.t option
  ; time : Time_float.Ofday.t option
  ; keep : [ `All | `Newer_than of Time_float.Span.t | `At_least of int ]
  ; naming_scheme : Naming_scheme.t
  ; zone : Time_float.Zone.t
  }
[@@deriving fields ~getters, sexp_of]

val create
  :  ?messages:int
  -> ?size:Byte_units.t
  -> ?time:Time_float.Ofday.t
  -> ?zone:Time_float.Zone.t
  -> keep:[ `All | `Newer_than of Time_float.Span.t | `At_least of int ]
  -> naming_scheme:Naming_scheme.t
  -> unit
  -> t

(** Sane defaults for log rotation.

    Writes dated log files. Files are rotated every time the day changes in the given zone
    (uses the machine's zone by default). If the dated log file already exists, it's
    appended to.

    Logs are never deleted. Best practice is to have an external mechanism archive old
    logs for long-term storage. *)
val default : ?zone:Time_float.Zone.t -> unit -> t

val should_rotate
  :  t
  -> last_messages:int
  -> last_size:Byte_units.t
  -> last_time:Time_float.t
  -> current_time:Time_float.t
  -> bool
