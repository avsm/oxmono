(* Portable overlapping copy fallback (no SIMD).
   Always returns false so the caller uses its byte-by-byte fallback,
   except for distance 1 which uses Bytes.unsafe_fill. *)

let[@inline always] copy_overlapping dst dst_pos distance copy_length =
  match distance with
  | 1 ->
    let byte_val = Bytes.unsafe_get dst (dst_pos - 1) in
    Bytes.unsafe_fill dst dst_pos copy_length byte_val;
    true
  | _ ->
    false
