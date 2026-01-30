open! Core

(** This library offers a lightweight way for applications protocols to version
    themselves. The more protocols that add themselves to [Known_protocol], the nicer
    error messages we will get when connecting to a service while using the wrong
    protocol.

    The library also offers a mechanism for some data to be shared in the connection
    header. A header can contain [additional_magic_numbers] that are not used in the
    version negotiation but can be used in some way by the peer (e.g. the kerberos rpc
    library could use this to indicate whether the server is doing any form of
    authorization). *)

type t [@@deriving bin_io ~localize, globalize, sexp, stable_witness]

val bin_read_t__local : t Bin_prot.Read.reader__local [@@zero_alloc opt arity 2]

(** [create_exn] raises if one of the following is true:
    - [List.length supported_versions + List.length additional_magic_numbers >= 100]
    - [supported_versions] containing version number greater than [max_supported_version]
    - [additional_magic_numbers] containing version numbers not greater than
      [max_supported_version]
    - [additional_magic_numbers] overlaps with potential magic values for [protocol]s. *)
val create_exn
  :  ?additional_magic_numbers:int list
  -> protocol:Known_protocol.t
  -> supported_versions:int list
  -> unit
  -> t

(** [negotiate ~allow_legacy_peer ~us ~peer] inspects the magic numbers of [us] and
    [peer]. If the magic numbers match, the highest shared version number is returned.

    If [allow_legacy_peer] then the magic number of [peer] is assumed to be [us] if no
    magic number exists. *)
val negotiate : allow_legacy_peer:bool -> us:t -> peer:t -> int Or_error.t

(** [contains_magic_prefix] reads a bin_protted value of type [t] and returns a boolean
    saying whether this magic number was observed. *)
val contains_magic_prefix : protocol:Known_protocol.t -> bool Bin_prot.Type_class.reader

(** [any_magic_prefix] and [any_magic_prefix_from_six_bytes] read the magic number for one
    of the known protocols. They differ in how many bytes they read.

    [any_magic_prefix] reads the entire [Protocol_version_header.t] and then checks if
    there's any magic prefix in there. The number of bytes in the bin_io representation of
    a [Protocol_version_header.t] is not fixed because it depends on the supported
    versions of the protocol.

    [any_magic_prefix_from_six_bytes] reads exactly 6 bytes to check magic prefix.

    [any_magic_prefix_from_six_bytes_bin_size] is 6: the number of bytes that
    [any_magic_prefix_from_six_bytes] reads. *)
val any_magic_prefix : Known_protocol.t option Bin_prot.Type_class.reader

(** See [any_magic_prefix] *)
val any_magic_prefix_from_six_bytes : Known_protocol.t option Bin_prot.Type_class.reader

(** See [any_magic_prefix] *)
val any_magic_prefix_from_six_bytes_bin_size : int

module Known_protocol = Known_protocol

(** This is useful for passing around a pair of [t]s that will eventually be passed to
    [negotiate]. *)
module Pair : sig
  type nonrec t =
    { us : t
    ; peer : t
    }
end

module Expert : sig
  (** Gets the raw list of version numbers contained in the header. *)
  val raw_version_list : t -> int list

  val none : t
  val is_none : t -> bool
end

module For_test : sig
  module Make_list_with_max_len (Config : List_with_max_len.Config) : List_with_max_len.S

  (** The number of bytes in the bin_io representation of a magic number. All magic
      numbers are represented in this fixed number of bytes. *)
  val magic_number_bin_size : int

  (** Max version that's possible to be supported ever. Any numbers bigger than this are
      considered metadata that's shared in the header. *)
  val max_supported_version : int

  module Validated_for_fast_path : sig
    type t =
      { header : int iarray
      ; protocol : Known_protocol.t
      ; versions_start_index : int
      }

    val validate : int iarray -> local_ t or_null
    val max_common_version : local_ t -> local_ t -> int or_null
  end

  val create_unvalidated : int list -> t
end
