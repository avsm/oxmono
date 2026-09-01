
module AES = struct
  external enc      : string -> int -> bytes -> int -> string -> int -> int -> unit
    @@ portable = "mc_aes_enc_bc" "mc_aes_enc" [@@noalloc]
  external dec      : string -> int -> bytes -> int -> string -> int -> int -> unit
    @@ portable = "mc_aes_dec_bc" "mc_aes_dec" [@@noalloc]
  external derive_e : string -> bytes -> int -> unit
    @@ portable = "mc_aes_derive_e_key" [@@noalloc]
  external derive_d : string -> bytes -> int -> string option -> unit
    @@ portable = "mc_aes_derive_d_key" [@@noalloc]
  external rk_s     : int  -> int @@ portable = "mc_aes_rk_size" [@@noalloc]
  external mode     : unit -> int = "mc_aes_mode" [@@noalloc]
end

module DES = struct
  external ddes    : string -> int -> bytes -> int -> int -> string -> unit
    @@ portable = "mc_des_ddes_bc" "mc_des_ddes" [@@noalloc]
  external des3key : bytes -> int -> bytes -> unit
    @@ portable = "mc_des_des3key" [@@noalloc]
  external k_s     : unit -> int = "mc_des_key_size" [@@noalloc]
end

module Chacha = struct
  external round : int -> bytes -> bytes -> int -> unit
    @@ portable = "mc_chacha_round" [@@noalloc]
end

module Poly1305 = struct
  external init     : bytes -> string -> unit
    @@ portable = "mc_poly1305_init" [@@noalloc]
  external update   : bytes -> string -> int -> int -> unit
    @@ portable = "mc_poly1305_update" [@@noalloc]
  external finalize : bytes -> bytes -> int -> unit
    @@ portable = "mc_poly1305_finalize" [@@noalloc]
  external ctx_size : unit -> int @@ portable = "mc_poly1305_ctx_size" [@@noalloc]
  external mac_size : unit -> int = "mc_poly1305_mac_size" [@@noalloc]
end

module GHASH = struct
  external keysize : unit -> int = "mc_ghash_key_size" [@@noalloc]
  external keyinit : string -> bytes -> unit
    @@ portable = "mc_ghash_init_key" [@@noalloc]
  external ghash : string -> bytes -> string -> int -> int -> unit
    @@ portable = "mc_ghash" [@@noalloc]
  external mode : unit -> int = "mc_ghash_mode" [@@noalloc]
end

(* XXX TODO
 * Unsolved: bounds-checked XORs are slowing things down considerably... *)
external xor_into_bytes : string -> int -> bytes -> int -> int -> unit
  @@ portable = "mc_xor_into_bytes" [@@noalloc]

external count8be   : ctr:bytes -> bytes -> off:int -> blocks:int -> unit
  @@ portable = "mc_count_8_be" [@@noalloc]
external count16be  : ctr:bytes -> bytes -> off:int -> blocks:int -> unit
  @@ portable = "mc_count_16_be" [@@noalloc]
external count16be4 : ctr:bytes -> bytes -> off:int -> blocks:int -> unit
  @@ portable = "mc_count_16_be_4" [@@noalloc]

external misc_mode : unit -> int = "mc_misc_mode" [@@noalloc]

external _detect_cpu_features : unit -> unit = "mc_detect_cpu_features" [@@noalloc]
external _detect_entropy : unit -> unit = "mc_entropy_detect"

let () =
  _detect_cpu_features ();
  _detect_entropy ()
