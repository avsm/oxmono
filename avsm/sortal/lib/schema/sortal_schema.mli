(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sortal Schema - Versioned data types and serialization

    This library provides versioned schema definitions for contact metadata
    with minimal I/O dependencies. It includes:
    - Temporal validity support (ISO 8601 dates and ranges)
    - Feed subscription types
    - Contact metadata schemas (versioned)

    The schema library depends on jsont, yamlt, bytesrw, fmt for serialization
    and formatting, plus ptime and ptime.clock.os for date/time operations. *)

(** {1 Schema Version 1} *)

module V1 : sig
  (** Version 1 of the contact schema (current stable version). *)

  (** Temporal validity support for time-bounded fields. *)
  module Temporal = Sortal_schema_temporal

  (** Feed subscription metadata. *)
  module Feed = Sortal_schema_feed

  (** Contact metadata with temporal support. *)
  module Contact = Sortal_schema_contact_v1
end

(** {1 Current Version Aliases}

    These aliases point to the current stable schema version (V1).
    When V2 is introduced, these will continue pointing to V1 for
    backward compatibility. *)

module Temporal = V1.Temporal
module Feed = V1.Feed
module Contact = V1.Contact
