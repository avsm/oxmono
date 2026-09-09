module S = Mqttz.Slice

let[@zero_alloc] local_view_sum s =
  let local_ view = S.sub_local s 1 (S.length s - 1) in
  let local_ total = ref 0 in
  for i = 0 to S.length view - 1 do total := !total + S.get_uint8 view i done;
  !total
