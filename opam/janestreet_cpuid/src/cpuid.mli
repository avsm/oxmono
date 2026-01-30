open Core

(** It is currently an error to run this on a non AMD/Intel CPU.

    For what it's worth, ARM is a pain to do this on, but at least then we'd know at
    compile time that we're going to be running on ARM and could be a bit more clever. *)
type t =
  | Amd of Amd_cpuid.t
  | Intel of Intel_cpuid.t
  | Arm

val create : unit -> t Or_error.t
val canonical_identifier : t -> string

(** If you use a flag on both platforms go ahead and add it here. It would be really nice
    to generate these based on the shared functionality but we really don't use enough CPU
    feature flags to make that worth it. *)

(** Does the processor support the SSE3 instruction set extensions.

    Support starts with Celeron / Opteron. *)
val supports_sse3 : t -> bool

(** Does the processor support the SSE4.1 instruction set extensions.

    Note that this is different from the AMD flag SSE4A.

    Support starts with Penryn / Jaguar. *)
val supports_sse4_1 : t -> bool

(** Does the processor support the SSE4.2 instruction set extensions.

    Note that this is different from the AMD flag SSE4A.

    Support starts with Nehalem / Jaguar. *)
val supports_sse4_2 : t -> bool

(** Does the processor support the AVX instruction set extensions.

    Support starts with Sandy Bridge / Jaguar. *)
val supports_avx : t -> bool

(** Does the processor support the AVX2 instruction set extensions.

    Support starts with Haswell / Excavator. *)
val supports_avx2 : t -> bool

(** Does the processor support the AVX-512F instruction set extensions.

    Support starts with Skylake-X / Knights Landing. *)
val supports_avx512f : t -> bool

(** Does the processor support the AVX-512DQ instruction set extensions.

    Support starts with Skylake-X. *)
val supports_avx512dq : t -> bool

(** Does the processor support the AVX-512IFMA instruction set extensions.

    Support starts with Knights Landing. *)
val supports_avx512ifma : t -> bool

(** Does the processor support the AVX-512PF instruction set extensions.

    Support starts with Knights Landing. *)
val supports_avx512pf : t -> bool

(** Does the processor support the AVX-512ER instruction set extensions.

    Support starts with Knights Landing. *)
val supports_avx512er : t -> bool

(** Does the processor support the AVX-512CD instruction set extensions.

    Support starts with Skylake-X / Knights Landing. *)
val supports_avx512cd : t -> bool

(** Does the processor support the AVX-512BW instruction set extensions.

    Support starts with Skylake-X. *)
val supports_avx512bw : t -> bool

(** Does the processor support the AVX-512VL instruction set extensions.

    Support starts with Skylake-X. *)
val supports_avx512vl : t -> bool

(** Does the processor support the AVX-512VBMI instruction set extensions.

    Support starts with Cannon Lake. *)
val supports_avx512vbmi : t -> bool

(** Does the processor support the AVX-512VBMI2 instruction set extensions.

    Support starts with Ice Lake. *)
val supports_avx512vbmi2 : t -> bool

(** Does the processor support the AVX-512VNNI instruction set extensions.

    Support starts with Cascade Lake. *)
val supports_avx512vnni : t -> bool

(** Does the processor support the AVX-512BITALG instruction set extensions.

    Support starts with Ice Lake. *)
val supports_avx512bitalg : t -> bool

(** Does the processor support the AVX-512VPOPCNTDQ instruction set extensions.

    Support starts with Ice Lake. *)
val supports_avx512vpopcntdq : t -> bool

(** Does the processor support the AVX-5124VNNIW instruction set extensions.

    Support starts with Knights Mill. *)
val supports_avx5124vnniw : t -> bool

(** Does the processor support the AVX-5124FMAPS instruction set extensions.

    Support starts with Knights Mill. *)
val supports_avx5124fmaps : t -> bool

(** Does the processor support the AVX-512VP2INTERSECT instruction set extensions.

    Support starts with Tiger Lake. *)
val supports_avx512_vp2intersect : t -> bool

(** Does the processor support the PCLMULQDQ instruction.

    Supports starts with Sandy Bridge / Jaguar. *)
val supports_pclmulqdq : t -> bool

(** Does the processor support the FMA3 instruction set extensions.

    Support starts with Haswell / Piledriver. *)
val supports_fma : t -> bool

(** Does the processor support the WAITPKG instruction set extensions.

    Support starts with the Tremont microarchitecture. For servers, Sapphire Rapids. *)
val supports_waitpkg : t -> bool

(** Does the processor support the RTM instruction set extensions.

    Add [tsx=on] or [tsx=auto] to kernel command-line to enable RTM. *)
val supports_rtm : t -> bool

(** Module gateway into AMD specific flags and functionality. *)
module Amd = Amd_cpuid

(** Module gateway into Intel specific flags and functionality. *)
module Intel = Intel_cpuid

module Registers : sig
  type t

  val print : local_ t -> unit
end

val arbitrary_leaf_and_subleaf : leaf:int -> subleaf:int -> local_ Registers.t
