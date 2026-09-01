include Uriz.Raw

let[@zero_alloc] needs_percent_decode s ~pos ~len ~plus_as_space =
  needs_decode s ~pos ~len ~plus_as_space

let[@zero_alloc] percent_decode_into s ~pos ~len ~dst ~dst_pos ~plus_as_space =
  pct_decode_into s ~pos ~len ~dst ~dst_pos ~plus_as_space
