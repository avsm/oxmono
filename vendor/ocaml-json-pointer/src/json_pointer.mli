@@ portable

(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** RFC 6901 JSON Pointers for [Jsont] JSON values.

    A JSON Pointer is an address for one location inside a JSON document, in the
    way a file path is an address for a file. It is a sequence of string tokens,
    written with each token prefixed by [/]. A token's meaning depends on the
    value being traversed. It is a member name for an object and an array index
    for an array.

    In the document

    {x@json[
    {"users": [{"name": "Ada"}, {"name": "Grace"}], "active": true}
    ]x}

    [/users/0/name] is ["Ada"], [/users] is the whole array, and the empty
    pointer is the document itself. The JSON value decides how a token is read,
    so numeric object member names remain ordinary tokens and [/0] is the member
    named ["0"] of an object and element zero of an array. The token [-] is
    special only against an array, where it denotes the position after the last
    element.

    Two characters are escaped inside a token. [~1] stands for [/] and [~0] for
    [~], so [/a~1b] is the member named ["a/b"]. {!of_string} and {!to_string}
    apply the escaping, and the token-level functions are in {!Token}.

    Parsing a pointer does not check that its target exists. Resolution happens
    in {!get}, {!get_result} and {!find}. The RFC 6902 JSON Patch operations
    build new documents from a pointer, {!path} and its companions lift a
    pointer into a [Jsont.t] codec, and {!Jmap} adds the RFC 8620 wildcard
    extension.

    See the {{!page-tutorial}tutorial} for a worked introduction. *)

module Token : sig
  type t = string
  (** An unescaped reference token. Strings are expected to contain UTF-8. *)

  val escape : t -> string
  (** [escape token] replaces [~] with [~0] and [/] with [~1].

      Raises [Jsont.Error] if [token] is not valid UTF-8. *)

  val unescape : string -> t
  (** [unescape s] reverses JSON Pointer token escaping.

      Raises [Jsont.Error] if [s] is not valid UTF-8 or contains [~] not
      followed by [0] or [1]. *)
end

type t : immutable_data
(** A JSON Pointer. *)

val root : t
(** The empty pointer, which identifies the whole document. *)

val is_root : t -> bool
(** [is_root p] is [true] exactly when [p] is {!root}. *)

val of_tokens : Token.t list -> t
(** [of_tokens tokens] is the pointer made of [tokens], in traversal order.

    Raises [Jsont.Error] if a token is not valid UTF-8. *)

val tokens : t -> Token.t list
(** [tokens p] returns [p]'s unescaped tokens in traversal order. *)

val ( / ) : t -> Token.t -> t
(** [p / token] appends [token] to [p].

    Raises [Jsont.Error] if [token] is not valid UTF-8. *)

val append : t -> Token.t -> t
(** [append p token] is [p / token]. *)

val concat : t -> t -> t
(** [concat a b] appends all of [b]'s tokens to [a]. *)

val parent : t -> t option
(** [parent p] drops [p]'s last token, or is [None] for {!root}. *)

val last : t -> Token.t option
(** [last p] is [p]'s last token, or [None] for {!root}. *)

(** {1 Parsing and formatting} *)

val of_string : string -> t
(** [of_string s] parses the JSON Pointer string representation [s].

    Raises [Jsont.Error] if a nonempty [s] does not start with [/] or a token
    contains invalid UTF-8 or an invalid escape sequence. *)

val of_string_result : string -> (t, string) result
(** [of_string_result] is {!of_string} with errors returned as strings. *)

val to_string : t -> string
(** [to_string p] is [p] in JSON Pointer string representation. *)

val of_uri_fragment : string -> t
(** [of_uri_fragment s] parses the percent-encoded content of a URI fragment.
    The leading [#] is not part of [s]. Percent-decoding is performed before
    JSON Pointer token unescaping.

    Raises [Jsont.Error] on invalid percent encoding or pointer syntax. *)

val of_uri_fragment_result : string -> (t, string) result
(** [of_uri_fragment_result] is {!of_uri_fragment} with errors returned as
    strings. *)

val to_uri_fragment : t -> string
(** [to_uri_fragment p] is the percent-encoded URI fragment content for [p],
    without the leading [#]. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats the JSON Pointer string representation. *)

val equal : t -> t -> bool
(** [equal a b] is [true] exactly when [a] and [b] have equal tokens. *)

val compare : t -> t -> int
(** [compare] orders pointers lexicographically by token. *)

val of_path : Jsont.Path.t -> t
(** [of_path path] converts a [Jsont.Path.t] to a JSON Pointer.

    JSON Pointer syntax does not distinguish a numeric object member from an
    array index; the resulting token is interpreted from the JSON value when
    evaluated. There is consequently no context-free inverse conversion.

    Raises [Jsont.Error] if [path] contains a negative array index or an object
    member name that is not valid UTF-8. *)

(** {1 Evaluation} *)

val get : t -> Jsont.json -> Jsont.json
(** [get p json] returns the value identified by [p].

    Raises [Jsont.Error] if [p] cannot be resolved, including when a referenced
    object member name is not unique. *)

val get_result : t -> Jsont.json -> (Jsont.json, Jsont.Error.t) result
(** [get_result] is {!get} with errors returned in the result. *)

val find : t -> Jsont.json -> Jsont.json option
(** [find p json] is [Some value] if [p] resolves and [None] otherwise. *)

(** {1 JSON Patch operations} *)

val add : t -> Jsont.json -> value:Jsont.json -> Jsont.json
(** [add path json ~value] implements the RFC 6902 [add] operation.

    It replaces or creates an object member, inserts at an array index, and
    appends to an array when the final token is [-]. All preceding tokens must
    identify existing values. The root pointer replaces the document.

    Raises [Jsont.Error] if the target parent cannot be resolved or an array
    index is out of bounds. *)

val remove : t -> Jsont.json -> Jsont.json
(** [remove path json] implements the RFC 6902 [remove] operation.

    Raises [Jsont.Error] if [path] is root or does not identify an existing
    value. *)

val replace : t -> Jsont.json -> value:Jsont.json -> Jsont.json
(** [replace path json ~value] implements the RFC 6902 [replace] operation. The
    root pointer replaces the document.

    Raises [Jsont.Error] if [path] does not identify an existing value. *)

val move : from:t -> path:t -> Jsont.json -> Jsont.json
(** [move ~from ~path json] implements the RFC 6902 [move] operation. Identical
    source and destination pointers leave [json] unchanged.

    Raises [Jsont.Error] if [from] does not resolve, [path]'s parent does not
    resolve, or [from] is a proper prefix of [path]. *)

val copy : from:t -> path:t -> Jsont.json -> Jsont.json
(** [copy ~from ~path json] implements the RFC 6902 [copy] operation. *)

val test : t -> Jsont.json -> expected:Jsont.json -> bool
(** [test path json ~expected] implements the RFC 6902 [test] comparison. It
    returns [false] whenever [path] does not resolve, including on errors such
    as a non-unique object member name. *)

(** {1 Jsont codecs and queries} *)

val jsont : t Jsont.t
(** A codec for JSON Pointer string representations. *)

val jsont_uri_fragment : t Jsont.t
(** A codec for percent-encoded URI fragment content, without the leading [#].
*)

val path : ('a : value mod portable contended). ?absent:'a -> t -> 'a Jsont.t -> 'a Jsont.t
(** [path p codec] extracts the value at [p] and decodes it with [codec]. If
    [absent] is supplied, it is returned when a referenced member or array
    element is missing. Invalid indices, incompatible JSON values, duplicate
    member names, and codec errors are still reported.

    The resulting codec is decode-only: encoding through it raises
    [Jsont.Error]. *)

val set_path : ('a : value mod portable contended). ?allow_absent:bool -> 'a Jsont.t -> t -> 'a -> Jsont.json Jsont.t
(** [set_path codec p value] produces a codec that replaces the value at [p].
    With [allow_absent:true], a missing final object member or the position at
    the current end of an array may be created; preceding values must still
    exist. *)

val update_path : ('a : value mod portable contended). ?absent:'a -> t -> 'a Jsont.t -> Jsont.json Jsont.t
(** [update_path p codec] produces a codec that decodes and re-encodes the value
    at [p]. If [absent] is supplied, that value is inserted when the final
    target is missing and its parent exists. *)

val delete_path : ?allow_absent:bool -> t -> Jsont.json Jsont.t
(** [delete_path p] produces a codec that removes the value at [p]. With
    [allow_absent:true], a missing final object member or array element leaves
    the input unchanged; preceding tokens must still resolve. The root pointer
    is rejected. *)

(** {1 JMAP result references} *)

module Jmap : sig
  (** RFC 8620 result references.

      A JMAP method call may refer to the result of an earlier call in the same
      request. Such a reference is written as an ordinary JSON Pointer with one
      added evaluation rule. When a [*] token is applied to an array, the
      remaining tokens are evaluated against every element of that array, and
      results that are themselves arrays are flattened by one level.

      In the response

      {x@json[
      {"list": [{"id": "a", "tags": ["x", "y"]},
                {"id": "b", "tags": ["z"]}]}
      ]x}

      the pointer [/list/*/id] is [["a", "b"]], and [/list/*/tags] is
      [["x", "y", "z"]] rather than [[["x", "y"], ["z"]]]. The wildcard is
      special only against an array. On an object, [*] is the ordinary member
      name ["*"], and a pointer containing no array wildcard evaluates exactly
      as it does under {!Json_pointer.get}.

      Pointers are parsed by {!Json_pointer.of_string} as usual. A pointer
      acquires RFC 8620 semantics only through the functions below, so an
      application that must reject wildcards keeps using the functions of
      {!Json_pointer}.

      A wildcard can emit more values than the document holds. Nested wildcards
      multiply, and a subtree shared between several elements is expanded once
      for each element that reaches it. The optional [max_results] argument
      bounds that expansion. Set it when evaluating a reference supplied by an
      untrusted peer.

      The {{!page-tutorial}tutorial} works through further examples. *)

  val get : ?max_results:int -> t -> Jsont.json -> Jsont.json
  (** [get ~max_results p json] is the value identified by [p] in [json], with
      the JMAP wildcard extension applied. A pointer whose last wildcard has
      been traversed yields an array of the collected values.

      [max_results] bounds the number of values emitted by wildcard expansion. A
      result reached without a wildcard is not counted, since it is already part
      of the input tree. One bound is shared by nested wildcards and by repeated
      visits to a physically shared subtree. [max_results] defaults to
      unbounded.

      Raises [Jsont.Error] if [p] cannot be resolved or the bound would be
      exceeded, and [Invalid_argument] if [max_results] is negative. *)

  val get_result :
    ?max_results:int -> t -> Jsont.json -> (Jsont.json, Jsont.Error.t) result
  (** [get_result] is {!get} with resolution and result-bound errors returned in
      the result. A negative bound still raises [Invalid_argument]. *)

  val find : ?max_results:int -> t -> Jsont.json -> Jsont.json option
  (** [find p json] is [Some value] if [p] resolves within [max_results], and
      [None] otherwise. A negative bound raises [Invalid_argument]. An exceeded
      bound is reported as [None]. Use {!get_result} to tell the two apart. *)

  val path : ('a : value mod portable contended). ?max_results:int -> ?absent:'a -> t -> 'a Jsont.t -> 'a Jsont.t
  (** [path p codec] is a codec that locates the result reference [p] and
      decodes it with [codec]. See {!Json_pointer.path} for [absent] and error
      behavior, including the decode-only restriction.

      With a wildcard pointer, [absent] applies to the whole pointer. If the
      remaining tokens fail to resolve for a single array element, [absent] is
      returned for the entire reference rather than for that element.
      [max_results] has the meaning and default documented on {!get}. *)

  val path_list : ('a : value mod portable contended). ?max_results:int -> t -> 'a Jsont.t -> 'a list Jsont.t
  (** [path_list p codec] is [path p (Jsont.list codec)]. It decodes the array
      collected by a wildcard reference into a list. *)
end
