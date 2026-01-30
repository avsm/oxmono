(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module V1 = struct
  module Temporal = Sortal_schema_temporal
  module Feed = Sortal_schema_feed
  module Contact = Sortal_schema_contact_v1
end

module Temporal = V1.Temporal
module Feed = V1.Feed
module Contact = V1.Contact
