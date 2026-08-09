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

(** {1 Schema Version 2} *)

module V2 : sig
  (** Version 2 of the contact schema. Not yet the default: {!Contact}
      below still aliases {!V1.Contact} until Task 8 switches it over. *)

  (** Contact metadata, V2. *)
  module Contact = Sortal_schema_contact_v2
end

(** {1 Current Version Aliases}

    These aliases point to the current stable schema version (V1).
    When V2 is introduced, these will continue pointing to V1 for
    backward compatibility. *)

(** ISO 8601 calendar dates, used by V2's affiliations. *)
module Date = Sortal_schema_date

(** The closed platform vocabulary, used by V2's accounts. *)
module Platform = Sortal_schema_platform

(** Accounts a contact holds on a platform, V2's replacement for V1's four
    overlapping ways of recording where a person is online. *)
module Account = Sortal_schema_account

module Temporal = V1.Temporal
module Feed = V1.Feed
module Contact = V1.Contact
