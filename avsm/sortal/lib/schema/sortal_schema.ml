(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module V1 = struct
  module Temporal = Sortal_schema_temporal
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v1
end

(* Additive only: Task 5 restructures this properly. V1 stays the default
   until Task 8, so nothing here may change what [Contact] below aliases. *)
module V2 = struct
  module Contact = Sortal_schema_contact_v2
end

module Date = Sortal_schema_date
module Platform = Sortal_schema_platform
module Account = Sortal_schema_account
module Temporal = V1.Temporal
module Feed = V1.Feed
module Contact = V1.Contact
