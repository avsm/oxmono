(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Sortal Schema - Versioned data types and serialization

    This library provides versioned schema definitions for contact metadata
    with minimal I/O dependencies. It includes:
    - Temporal validity support for V1 (ISO 8601 dates and ranges)
    - ISO 8601 calendar dates for V2, and the platform and account schemas
      built on them
    - Feed subscription types, shared by both versions
    - Contact metadata schemas (versioned)

    The schema library depends on jsont, yamlt, bytesrw, fmt for serialization
    and formatting, plus ptime and ptime.clock.os for date/time operations. *)

(** {1 Schema Version 1}

    V1 is retained so that {!Sortal_schema_migrate} can read existing files.
    It is removed once every store has been migrated. *)

module V1 : sig
  module Temporal = Sortal_schema_temporal
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v1
end

(** {1 Schema Version 2}

    V2 is the default: {!Contact} below aliases {!V2.Contact}, and the
    store reads and writes V2. *)

module V2 : sig
  module Date = Sortal_schema_date
  module Platform = Sortal_schema_platform
  module Account = Sortal_schema_account
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v2
end

module Migrate = Sortal_schema_migrate
(** {!Sortal_schema_migrate} converts a {!V1.Contact.t} to a
    {!V2.Contact.t}. *)

(** {1 Current version aliases}

    These name the schema in current use. Contact now points at V2, since
    the store reads and writes V2. Temporal still points at V1: it has no
    V2 counterpart, and is removed once the last V1 reader is gone. *)

module Date = V2.Date
module Platform = V2.Platform
module Account = V2.Account
module Feed = V2.Feed
module Temporal = V1.Temporal
module Contact = V2.Contact
