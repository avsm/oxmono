(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Server capabilities and metadata.

    This module provides a high-level interface for querying server capabilities
    and metadata. It wraps the underlying protocol representation and provides
    convenient accessors and capability checks. *)

(** {1 Server Information} *)

type t
(** Server metadata and capabilities. *)

val version : t -> string
(** [version t] returns the server version string. *)

val capabilities : t -> string list
(** [capabilities t] returns the list of available server capabilities. *)

val commands : t -> string list
(** [commands t] returns the list of available CLI commands. *)

val output_styles : t -> string list
(** [output_styles t] returns the list of supported output formats. *)

(** {1 Capability Checks} *)

val has_capability : t -> string -> bool
(** [has_capability t cap] returns true if the specified capability is
    available. *)

val supports_hooks : t -> bool
(** [supports_hooks t] checks if the hooks capability is available. *)

val supports_structured_output : t -> bool
(** [supports_structured_output t] checks if the structured output capability is
    available. *)

(** {1 Internal} *)

val of_proto : Proto.Control.Server_info.t -> t
(** [of_proto proto] converts from the protocol representation. *)

val of_sdk_control : Sdk_control.Server_info.t -> t
(** [of_sdk_control sdk] converts from the SDK control representation. *)
