(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test harness for running the CardDAV client against a real server.

    The oracle is the Radicale server [scripts/carddav-up.sh] starts in docker.
    Every test is skipped unless [CARDDAV_ORACLE_URL] is set, so [dune runtest]
    stays hermetic.

    Environment variables:
    - [CARDDAV_ORACLE_URL] the server root, such as [http://localhost:15232/]
    - [CARDDAV_ORACLE_USER] login name (default [alice])
    - [CARDDAV_ORACLE_PASSWORD] password (default [x]; Radicale accepts
      anything) *)

type t = {
  env : Eio_unix.Stdenv.base;
  sw : Eio.Switch.t;
  client : Carddav_eio.Client.t;
  user : string;
}

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
  (Carddav_eio.Client.t, Carddav_eio.Client.error) result
(** [connect_with ~sw env] is {!Carddav_eio.Client.connect} against the oracle
    with [~allow_insecure:true], since the test server is plain HTTP on
    localhost. *)

val test_case : string -> (t -> unit) -> unit Alcotest.test_case
(** [test_case name f] is a quick Alcotest case that connects to the oracle and
    runs [f]. It is skipped when the oracle is not configured. *)

val ok : string -> ('a, Carddav_eio.Client.error) result -> 'a
(** [ok what r] is the value of [r], failing the test with the error and [what]
    otherwise. *)

val created : string list ref
(** [created] are the collections the running test made, removed when it ends.
*)

val unique : string -> string
(** [unique prefix] is a name no other test run has used. *)

val fresh_addressbook : t -> string
(** [fresh_addressbook t] creates an address book with a unique name under the
    first home set and is its URL. *)

val vcard : ?uid:string -> ?email:string -> string -> Vcard.t
(** [vcard ~uid ~email full] is a vCard 4.0 with the FN [full], a UID that
    defaults to a fresh one, and an EMAIL when given. *)
