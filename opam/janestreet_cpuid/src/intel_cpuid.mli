(** All of the information in this file comes from (currently) page 792 of the Intel
    Software Developer's Library (combined 4 volumes), a.k.a the section in Volume 2
    detailing the CPUID instruction. *)

include Cpuid_intf.S

(** On Intel CPUs the leaf retrieved when EAX is set to 0x1 contains processor model
    information as well as some basic features about the processor. Typically if you're
    checking a flag in this section it's probably available on every machine that we run
    on (our machines tend towards modernity) but good on you for checking! *)
module Version_and_feature_information : sig
  (** This information can be used to build a unique identifier for a given processor,
      which can be useful for any pre-computed data which you need to index into based on
      what machine it's running on. *)
  module Eax : sig
    type t =
      { mutable step : int
      ; mutable model : int
      ; mutable family : int
      ; mutable proc_type : int
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
          (* this value is conditionally valid *)
      ; mutable initial_apic_id : int
      }
    [@@deriving sexp_of]
  end

  module Ecx_flags : sig
    include Flags.S

    val sse3 : t
    val pclmulqdq : t
    val dtes64 : t
    val monitor : t
    val dscpl : t
    val vmx : t
    val smx : t
    val eist : t
    val tm2 : t
    val ssse3 : t
    val cnxtid : t
    val sdbg : t
    val fma : t
    val cmpxchg16b : t
    val xtpr_update_control : t
    val pdcm : t
    val pcid : t
    val dca : t
    val sse4_1 : t
    val sse4_2 : t
    val x2apic : t
    val movbe : t
    val popcnt : t
    val tsc_deadline : t
    val aesni : t
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
    val cx8 : t
    val apic : t
    val sep : t
    val mtrr : t
    val pge : t
    val mca : t
    val cmov : t
    val pat : t
    val pse36 : t
    val psn : t
    val clfsh : t
    val ds : t
    val acpi : t
    val mmx : t
    val fxsr : t
    val sse : t
    val sse2 : t
    val ss : t
    val htt : t
    val tm : t
    val pbe : t
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

(** For identifying which table of perf-events applies for this processor. We use the form
    specified in the https://download.01.org/perfmon/mapfile.csv, i.e.:
    Family-Model-optionalStepping,Version,Filename,EventType
    GenuineIntel-6-2E,V2,/NHM-EX/NehalemEX_core_V2.json,core
    GenuineIntel-6-1E,V2,/NHM-EP/NehalemEP_core_V2.json,core
    GenuineIntel-6-55-[01234],V1.12,/SKX/skylakex_core_v1.12.json,core

    When family-model pair is not sufficient to identify the supported events, the
    description includes stepping.

    For example, processors in family 6 and model 0x55 from stepping 5 onwards use
    different perf events than those in previous steppings of the same model.

    To distinguish between these two cases, CPU ID format is GenuineIntel-6-55-[01234] and
    GenuineIntel-6-55-[56789ABCDEF], instead of GenuineIntel-6-55. *)
val canonical_identifier : t -> string

(** The leaf at 0x7 is special because in addition to putting 0x7 it also lets you put a
    value into ECX to get a particular feature subleaf. In this case we want ECX=0x0, and
    any processor which supports EAX=0x7 (which we do check) supports ECX=0x0.

    This provides flags into more modern features on Intel CPUs, including some which most
    of our machines definitely do not have. If you use any of these features, check to
    make sure they're supported. *)
module Extended_feature_flags_subleaf_0 : sig
  module Ebx_flags : sig
    include Flags.S

    val fsgsbase : t
    val ia32_tsc_adjust : t
    val sgx : t
    val bmi1 : t
    val hle : t
    val avx2 : t
    val fdp_excptn_only : t
    val smep : t
    val bmi2 : t
    val enhanced_rep_movsto : t
    val invpcid : t
    val rtm : t
    val rdt_m : t
    val deprecate_fpu_csds : t
    val mpx : t
    val rdt_a : t
    val avx512f : t
    val avx512dq : t
    val rdseed : t
    val adx : t
    val smap : t
    val avx512ifma : t
    val clflushopt : t
    val clwb : t
    val intel_processor_trace : t
    val avx512pf : t
    val avx512er : t
    val avx512cd : t
    val sha : t
    val avx512bw : t
    val avx512vl : t
  end

  module Ecx_flags : sig
    include Flags.S

    val prefetchwt1 : t
    val avx512vbmi : t
    val umip : t
    val pku : t
    val ospke : t
    val waitpkg : t
    val avx512vmbi2 : t
    val cet_ss : t
    val gfni : t
    val vaes : t
    val vpclmulqdq : t
    val avx512vnni : t
    val avx512bitalg : t
    val avx512vpopcntdq : t
    val la57 : t
    val rdpid : t
    val keylocker : t
    val cldemote : t
    val movdiri : t
    val movdir64b : t
    val sgx_lc : t
    val pks : t
    val mawau : t -> int
  end

  module Edx_flags : sig
    include Flags.S

    val avx5124vnniw : t
    val avx5124fmaps : t
    val fast_short_rep_move : t
    val avx512_vp2intersect : t
    val md_clear : t
    val hybrid : t
    val cet_ibt : t
    val ibrs_ibpb : t
    val stibp : t
    val l1d_flush : t
    val ia32_arch_capabilities : t
    val ia32_core_capabilities : t
    val ssbd : t
  end

  type t =
    { max_subleaf : int
    ; mutable ebx : Ebx_flags.t
    ; mutable ecx : Ecx_flags.t
    ; mutable edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  val retrieve : unit -> t

  module For_testing : sig
    val build_from_ints : eax:int -> ebx:int -> ecx:int -> edx:int -> t
  end
end
