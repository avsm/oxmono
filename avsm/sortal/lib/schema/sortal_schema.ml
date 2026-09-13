(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Typed projections of native vCard contacts. *)

module V2 = struct
  module Date = Sortal_schema_date
  module Platform = Sortal_schema_platform
  module Account = Sortal_schema_account
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v2
end

module Date = V2.Date
module Platform = V2.Platform
module Account = V2.Account
module Feed = V2.Feed
module Contact = V2.Contact
