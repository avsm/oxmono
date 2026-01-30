module Record = Record
module Variant = Variant

let within_inflexible_context ~f = Deep_inflexible_context.run_within ~f

module Stable = struct
  module Record = Record.Stable
  module Variant = Variant.Stable
end
