(** All of the information in this file comes from (currently) page 1779 of the AMD
    Architecture Programmer's Manual, Volumes 1-5. Specifically, Volume 3: Appendix E
    detailing the CPUID instruction. *)

include Cpuid_intf.S

(** On AMD CPUs the leaf retrieved when EAX is set to 0x1 contains processor model
    information as well as some basic features about the processor. *)
module Version_and_feature_information : sig
  module Eax : sig
    type t =
      { mutable step : int
      ; mutable model : int
      ; mutable family : int
      ; mutable extended_model : int
      ; mutable extended_family : int
      }
    [@@deriving sexp_of]
  end

  (** This is the only field in the CPUID which changes when called multiple times. The
      [initial_apic_id] field changes based on which core the process is currently running
      on. *)
  module Ebx : sig
    type t =
      { mutable brand_index : int
      ; mutable clflush_line_size : int
      ; mutable max_addressable_logical_processors : int
      ; mutable initial_apic_id : int
      }
    [@@deriving sexp_of]
  end

  module Ecx_flags : sig
    include Flags.S

    val sse3 : t
    val pclmulqdq : t
    val monitor : t
    val ssse3 : t
    val fma : t
    val cmpxchg16b : t
    val sse4_1 : t
    val sse4_2 : t
    val popcnt : t
    val aes : t
    val xsave : t
    val osxsave : t
    val avx : t
    val f16c : t
    val rdrand : t
  end

  module Edx_flags : sig
    include Flags.S

    val fpu : t
    val vme : t
    val de : t
    val pse : t
    val tsc : t
    val msr : t
    val pae : t
    val mce : t
    val cmpxchg8b : t
    val apic : t
    val sysentersysexit : t
    val mtrr : t
    val pge : t
    val mca : t
    val cmov : t
    val pat : t
    val pse36 : t
    val clfsh : t
    val mmx : t
    val fxsr : t
    val sse : t
    val sse2 : t
    val htt : t
  end

  type t =
    { eax : Eax.t
    ; ebx : Ebx.t
    ; mutable ecx : Ecx_flags.t
    ; mutable edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  val canonical_version_string : t -> string
  val retrieve : unit -> t

  module For_testing : sig
    val build_from_ints : eax:int -> ebx:int -> ecx:int -> edx:int -> t
  end
end

(** Canonical identifier similar to the one we use on Intel CPUs to index into a list of
    supported performance counters.

    See [Intel_cpuid.canonical_identifier] for a description of that. *)
val canonical_identifier : t -> string

(** The leaf at 0x7 is special because in addition to putting 0x7 it also lets you put a
    value into ECX to get a particular feature subleaf. In this case we want ECX=0x0, and
    any processor which supports EAX=0x7 (which we do check) supports ECX=0x0.

    This provides flags into more modern features on AMD CPUs, including some which most
    of our machines definitely do not have. If you use any of these features, check to
    make sure they're supported. *)
module Extended_feature_flags_subleaf_0 : sig
  module Ebx_flags : sig
    include Flags.S

    val fsgsbase : t
    val bmi1 : t
    val avx2 : t
    val smep : t
    val bmi2 : t
    val avx512f : t
    val avx512dq : t
    val rdseed : t
    val adx : t
    val smap : t
    val avx512ifma : t
    val rdpid : t
    val clflushopt : t
    val clwb : t
    val avx512pf : t
    val avx512er : t
    val avx512cd : t
    val sha : t
    val avx512bw : t
    val avx512vl : t
  end

  module Ecx_flags : sig
    include Flags.S

    val avx512vbmi : t
    val umip : t
    val pku : t
    val ospke : t
    val avx512vmbi2 : t
    val cet_ss : t
    val vaes : t
    val vpcmulqdq : t
    val avx512vnni : t
    val avx512bitalg : t
    val avx512vpopcntdq : t
  end

  module Edx_flags : sig
    include Flags.S

    val avx5124vnniw : t
    val avx5124fmaps : t
    val avx512_vp2intersect : t
  end

  type t =
    { max_subleaf : int
    ; ebx : Ebx_flags.t
    ; ecx : Ecx_flags.t
    ; edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  val retrieve : unit -> t

  module For_testing : sig
    val build_from_ints : eax:int -> ebx:int -> ecx:int -> edx:int -> t
  end
end
