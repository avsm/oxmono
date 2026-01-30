(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Eio integration for Tomlt.

    This module provides Eio-native functions for parsing and encoding TOML,
    with proper integration into Eio's exception system and system timezone
    support via {{:https://erratique.ch/software/ptime}ptime.clock.os}.

    {2 Quick Start}

    {[
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in

      (* Read and decode a config file *)
      type config = { host : string; port : int }

      let config_codec = Tomlt.(Table.(
        obj (fun host port -> { host; port })
        |> mem "host" string ~enc:(fun c -> c.host)
        |> mem "port" int ~enc:(fun c -> c.port)
        |> finish
      ))

      let config = Tomlt_eio.decode_file_exn config_codec Eio.Path.(fs / "config.toml")

      (* With datetime using system timezone *)
      type event = { name : string; time : Ptime.t }

      let event_codec = Tomlt.(Table.(
        obj (fun name time -> { name; time })
        |> mem "name" string ~enc:(fun e -> e.name)
        |> mem "time" (Tomlt_eio.ptime ()) ~enc:(fun e -> e.time)
        |> finish
      ))
    ]}
*)

(** {1 Eio Exception Integration} *)

type Eio.Exn.err += E of Tomlt.Toml.Error.t
(** TOML errors as Eio errors. *)

val err : Tomlt.Toml.Error.t -> exn
(** [err e] creates an [Eio.Io] exception from TOML error [e]. *)

val wrap_error : (unit -> 'a) -> 'a
(** [wrap_error f] runs [f] and converts [Tomlt.Toml.Error.Error] to [Eio.Io]. *)

(** {1 Raw TOML Parsing} *)

val parse : ?file:string -> string -> Tomlt.Toml.t
(** [parse s] parses TOML string [s] with Eio error handling.
    @param file optional filename for error context.
    @raise Eio.Io on parse errors. *)

val of_flow : ?file:string -> _ Eio.Flow.source -> Tomlt.Toml.t
(** [of_flow flow] reads and parses TOML from an Eio flow.
    @param file optional filename for error context.
    @raise Eio.Io on read or parse errors. *)

val of_path : fs:_ Eio.Path.t -> string -> Tomlt.Toml.t
(** [of_path ~fs path] reads and parses TOML from a file path.
    @raise Eio.Io on file or parse errors. *)

val of_file : _ Eio.Path.t -> Tomlt.Toml.t
(** [of_file path] reads and parses TOML from a file path.
    @raise Eio.Io on file or parse errors. *)

(** {1 Raw TOML Encoding} *)

val to_flow : _ Eio.Flow.sink -> Tomlt.Toml.t -> unit
(** [to_flow flow t] writes TOML value [t] to an Eio flow.
    @raise Invalid_argument if [t] is not a table. *)

val to_path : fs:_ Eio.Path.t -> string -> Tomlt.Toml.t -> unit
(** [to_path ~fs path t] writes TOML value [t] to a file.
    @raise Eio.Io on file errors.
    @raise Invalid_argument if [t] is not a table. *)

val to_file : _ Eio.Path.t -> Tomlt.Toml.t -> unit
(** [to_file path t] writes TOML value [t] to a file.
    @raise Eio.Io on file errors.
    @raise Invalid_argument if [t] is not a table. *)

(** {1 Codec-Based Operations}

    Decode and encode typed values directly. *)

val decode_flow :
  ?file:string -> 'a Tomlt.t -> _ Eio.Flow.source ->
  ('a, Tomlt.Toml.Error.t) result
(** [decode_flow codec flow] reads TOML from [flow] and decodes with [codec].
    @param file optional filename for error context. *)

val decode_flow_exn : ?file:string -> 'a Tomlt.t -> _ Eio.Flow.source -> 'a
(** [decode_flow_exn codec flow] is like {!decode_flow} but raises on errors.
    @raise Eio.Io on parse or decode errors. *)

val decode_path :
  'a Tomlt.t -> fs:_ Eio.Path.t -> string ->
  ('a, Tomlt.Toml.Error.t) result
(** [decode_path codec ~fs path] reads a TOML file and decodes with [codec]. *)

val decode_path_exn : 'a Tomlt.t -> fs:_ Eio.Path.t -> string -> 'a
(** [decode_path_exn codec ~fs path] is like {!decode_path} but raises.
    @raise Eio.Io on file, parse, or decode errors. *)

val decode_file : 'a Tomlt.t -> _ Eio.Path.t -> ('a, Tomlt.Toml.Error.t) result
(** [decode_file codec path] reads a TOML file and decodes with [codec]. *)

val decode_file_exn : 'a Tomlt.t -> _ Eio.Path.t -> 'a
(** [decode_file_exn codec path] is like {!decode_file} but raises.
    @raise Eio.Io on file, parse, or decode errors. *)

val encode_flow : 'a Tomlt.t -> 'a -> _ Eio.Flow.sink -> unit
(** [encode_flow codec value flow] encodes [value] and writes to [flow]. *)

val encode_path : 'a Tomlt.t -> 'a -> fs:_ Eio.Path.t -> string -> unit
(** [encode_path codec value ~fs path] encodes [value] and writes to a file.
    @raise Eio.Io on file errors. *)

val encode_file : 'a Tomlt.t -> 'a -> _ Eio.Path.t -> unit
(** [encode_file codec value path] encodes [value] and writes to a file.
    @raise Eio.Io on file errors. *)

(** {1 Ptime Codecs with System Timezone}

    Pre-configured datetime codecs that use the system timezone.
    These are convenience wrappers around {!Tomlt.ptime} and {!Tomlt.ptime_full}
    with [~get_tz:current_tz_offset_s] and [~now] already applied. *)

val ptime : ?frac_s:int -> unit -> Ptime.t Tomlt.t
(** [ptime ()] is a datetime codec using the system timezone.

    Equivalent to:
    {[Tomlt.ptime ~get_tz:Tomlt_eio.current_tz_offset_s
        ~now:Tomlt_eio.now ()]}

    @param frac_s Fractional seconds to include when encoding (0-12). *)

val ptime_full : unit -> Tomlt.Toml.ptime_datetime Tomlt.t
(** [ptime_full ()] preserves datetime variant information using system timezone.

    Equivalent to:
    {[Tomlt.ptime_full ~get_tz:Tomlt_eio.current_tz_offset_s ()]} *)

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
