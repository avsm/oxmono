(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Unix integration for Tomlt.

    This module provides Unix-native functions for parsing and encoding TOML
    files using standard channels, with system timezone support via
    {{:https://erratique.ch/software/ptime}ptime.clock.os}.

    {2 Quick Start}

    {[
      (* Read and decode a config file *)
      type config = { host : string; port : int }

      let config_codec = Tomlt.(Table.(
        obj (fun host port -> { host; port })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> finish
      ))

      let config = Tomlt_unix.decode_file_exn config_codec "config.toml"

      (* With datetime using system timezone *)
      type event = { name : string; time : Ptime.t }

      let event_codec = Tomlt.(Table.(
        obj (fun name time -> { name; time })
        |> mem "name" string ~enc:(fun e -> e.name)
        |> mem "time" (Tomlt_unix.ptime ()) ~enc:(fun e -> e.time)
        |> finish
      ))
    ]}
*)

(** {1 File I/O}

    Read and write TOML files directly. *)

val of_file : string -> Tomlt.Toml.t
(** [of_file path] reads and parses a TOML file.
    @raise Tomlt.Error.Error on parse errors.
    @raise Sys_error on file errors. *)

val to_file : string -> Tomlt.Toml.t -> unit
(** [to_file path value] writes [value] as TOML to a file.
    @raise Invalid_argument if [value] is not a table.
    @raise Sys_error on file errors. *)

(** {1 Channel I/O}

    Read and write TOML via standard channels. *)

val of_channel : ?file:string -> in_channel -> Tomlt.Toml.t
(** [of_channel ic] reads and parses TOML from an input channel.
    @param file Optional filename for error messages.
    @raise Tomlt.Toml.Error.exception-Error on parse errors. *)

val to_channel : out_channel -> Tomlt.Toml.t -> unit
(** [to_channel oc value] writes [value] as TOML to an output channel.
    @raise Invalid_argument if [value] is not a table. *)

(** {1 Codec-Based File Operations}

    Decode and encode typed values directly to/from files. *)

val decode_file : 'a Tomlt.t -> string -> ('a, Tomlt.Toml.Error.t) result
(** [decode_file codec path] reads a TOML file and decodes it with [codec].
    @raise Sys_error on file errors. *)

val decode_file_exn : 'a Tomlt.t -> string -> 'a
(** [decode_file_exn codec path] is like {!decode_file} but raises on errors.
    @raise Tomlt.Toml.Error.exception-Error on parse or decode errors.
    @raise Sys_error on file errors. *)

val encode_file : 'a Tomlt.t -> 'a -> string -> unit
(** [encode_file codec value path] encodes [value] and writes to a file.
    @raise Sys_error on file errors. *)

(** {1 Ptime Codecs with System Timezone}

    Pre-configured datetime codecs that use the system timezone.
    These are convenience wrappers around {!Tomlt.ptime} and {!Tomlt.ptime_full}
    with [~get_tz:current_tz_offset_s] and [~now] already applied. *)

val ptime : ?frac_s:int -> unit -> Ptime.t Tomlt.t
(** [ptime ()] is a datetime codec using the system timezone.

    Equivalent to:
    {[Tomlt.ptime ~get_tz:Tomlt_unix.current_tz_offset_s
        ~now:Tomlt_unix.now ()]}

    @param frac_s Fractional seconds to include when encoding (0-12). *)

val ptime_full : unit -> Tomlt.Toml.ptime_datetime Tomlt.t
(** [ptime_full ()] preserves datetime variant information using system timezone.

    Equivalent to:
    {[Tomlt.ptime_full ~get_tz:Tomlt_unix.current_tz_offset_s ()]} *)

(** {1 Time Utilities}

    Low-level time functions. Prefer using {!ptime} and {!ptime_full}
    for datetime handling. *)

val current_tz_offset_s : unit -> int option
(** [current_tz_offset_s ()] returns the current system timezone offset in
    seconds from UTC. Returns [Some offset] where positive values are east
    of UTC (e.g., 3600 for +01:00) and negative values are west. *)

val now : unit -> Ptime.t
(** [now ()] returns the current time as a [Ptime.t]. *)

val today_date : ?tz_offset_s:int -> unit -> Ptime.date
(** [today_date ?tz_offset_s ()] returns today's date as [(year, month, day)].
    If [tz_offset_s] is not provided, uses [current_tz_offset_s ()]. *)
