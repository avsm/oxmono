(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test harness for running the CalDAV client against a real server.

    The oracle is the Radicale server [scripts/carddav-up.sh] starts in docker,
    which serves CalDAV on the same port. Every test is skipped unless
    [CARDDAV_ORACLE_URL] is set, so [dune runtest] stays hermetic. The variables
    are those of the CardDAV oracle. *)

type t = {
  env : Eio_unix.Stdenv.base;
      (** The standard environment of the running test. *)
  sw : Eio.Switch.t;  (** The switch scoping the connection to the oracle. *)
  client : Caldav_eio.Client.t;  (** The client connected to the oracle. *)
  user : string;  (** The login name used to connect. *)
}
(** The type for a test running against the oracle. *)

val configured : unit -> bool
(** [configured ()] is [true] when [CARDDAV_ORACLE_URL] is set. *)

val url : unit -> string
(** [url ()] is [CARDDAV_ORACLE_URL], failing the test when it is unset. *)

val user : unit -> string
(** [user ()] is [CARDDAV_ORACLE_USER], default [alice]. *)

val password : unit -> string
(** [password ()] is [CARDDAV_ORACLE_PASSWORD], default [x]. *)

val credentials : unit -> Fetch.Credential.t list
(** [credentials ()] is the basic credential of {!val-user} and {!password}. *)

val connect_with :
  sw:Eio.Switch.t ->
  ?credentials:Fetch.Credential.t list ->
  Eio_unix.Stdenv.base ->
  (Caldav_eio.Client.t, Caldav_eio.Client.error) result
(** [connect_with ~sw env] is {!Caldav_eio.Client.connect} against the oracle
    with [~allow_insecure:true], since the test server is plain HTTP on
    localhost. *)

val test_case : string -> (t -> unit) -> unit Alcotest.test_case
(** [test_case name f] is a quick Alcotest case that connects to the oracle and
    runs [f]. It is skipped when the oracle is not configured. *)

val ok : string -> ('a, Caldav_eio.Client.error) result -> 'a
(** [ok what r] is the value of [r], failing the test with the error and [what]
    otherwise. *)

val fresh_calendar : ?components:string list -> t -> string
(** [fresh_calendar t] creates a calendar with a unique name under the first
    home set and is its URL. *)

val created : string list ref
(** [created] are the collections the running test made, removed when it ends.
*)

val unique : string -> string
(** [unique prefix] is a name no other test run has used. *)

val utc :
  year:int ->
  month:int ->
  day:int ->
  hour:int ->
  minute:int ->
  Ical.Date.date_time
(** [utc ~year ~month ~day ~hour ~minute] is that UTC date-time, on the second.
*)

val event :
  ?uid:string ->
  ?rrule:string ->
  ?all_day:Ical.Date.date ->
  ?props:Ical.Property.t list ->
  ?alarm:bool ->
  summary:string ->
  Ical.Date.date_time ->
  Ical.Date.date_time ->
  Ical.t
(** [event ~summary start finish] is a calendar object holding one VEVENT from
    [start] to [finish] in UTC, with a fresh UID unless given. [rrule] adds an
    RRULE, [all_day] replaces the times with a DATE start and the next day as
    end, [props] adds properties and [alarm] a VALARM. *)

val todo : ?uid:string -> summary:string -> Ical.Date.date_time -> Ical.t
(** [todo ~summary due] is a calendar object holding one VTODO due at [due]. *)

val journal : ?uid:string -> summary:string -> Ical.Date.date -> Ical.t
(** [journal ~summary day] is a calendar object holding one VJOURNAL dated
    [day], with a fresh UID unless given. *)

val summary : Ical.t -> string option
(** [summary cal] is the SUMMARY of the first component of [cal]. *)

val uid : Ical.t -> string
(** [uid cal] is the UID shared by the components of [cal], failing the test if
    [cal] has none. *)
