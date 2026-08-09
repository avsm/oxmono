(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** The platform vocabulary.

    A platform is a service a contact holds an account on. The vocabulary is
    closed, so an unrecognised platform key is a decoding error rather than
    a silently accepted typo.

    Adding a platform means adding a constructor and extending [all_simple]
    or [all_federated] in the implementation. The compiler demands the new
    rows in the key, URL and check functions, and a sentinel match beside
    the lists fails the build so that a new constructor cannot be added
    without the lists being read. Nothing verifies that the constructor
    then reached the list itself. Treat that one step as manual, because a
    constructor missing from the list vanishes from [all] and [of_key] in
    silence, and the test suite, which walks [all], cannot see what [all]
    does not contain. *)

type simple =
  | Github | Gitlab | Codeberg
  | Orcid | Scholar
  | Twitter | LinkedIn | Threads | Instagram | Flickr
(** Platforms whose URL derives from a bare handle. *)

type federated =
  | Mastodon | Pixelfed | PeerTube
  | Matrix | Zulip | Discourse
(** Platforms federated across instances, where an account is a user at a
    host. *)

type id = Simple of simple | Federated of federated | Atproto
(** [id] names any platform, and is what a lookup takes. *)

val all : id list @@ portable
(** [all] is every platform, in the order they are declared. *)

val key : id -> string @@ portable
(** [key id] is the YAML mapping key for [id]. Keys are unique across
    platforms. *)

val of_key : string -> id option @@ portable
(** [of_key s] is the platform [s] names, or [None] if [s] is not a platform
    key. *)

val simple_url : simple -> string -> string @@ portable
(** [simple_url p handle] is the canonical URL of [handle] on [p]. *)

val federated_url : federated -> user:string -> host:string -> string @@ portable
(** [federated_url p ~user ~host] is the canonical URL of [user] at [host] on
    [p]. Zulip is the exception: a Zulip account is recorded by display name
    rather than by handle, so no user URL can be derived and the host URL is
    returned instead. *)

val check_simple : simple -> string -> (unit, string) result @@ portable
(** [check_simple p handle] is [Ok ()] if [handle] is syntactically a [p]
    handle, or [Error why] naming the problem. The check is local and makes
    no network request. *)

val check_federated : federated -> user:string -> host:string ->
  (unit, string) result @@ portable
(** [check_federated p ~user ~host] is [Ok ()] if [user] at [host] is
    syntactically a [p] account, or [Error why]. Zulip is the exception: a
    Zulip account is recorded by display name, so [user] may contain
    whitespace and only its emptiness is checked. *)

val check_atproto_handle : string -> (unit, string) result @@ portable
(** [check_atproto_handle h] is [Ok ()] if [h] satisfies the AT Protocol
    handle syntax, or [Error why]. A handle is ASCII, at most 253 characters,
    and has two or more dot-separated segments of 1 to 63 characters drawn
    from letters, digits and hyphens, where no segment starts or ends with a
    hyphen and the final segment does not start with a digit. *)
