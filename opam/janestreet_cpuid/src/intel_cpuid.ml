open Core

type t = Cpuid_intf.Basic_info.t

let bit63 bit = Flags.create ~bit
let kind = Type_equal.Id.create ~name:"Intel CPUID" [%sexp_of: _]
let create basic = basic
let maximum_leaf (t : t) = t.maximum_value

module Version_and_feature_information = struct
  module Eax = struct
    type t =
      { mutable step : int
      ; mutable model : int
      ; mutable family : int
      ; mutable proc_type : int
      ; mutable extended_model : int
      ; mutable extended_family : int
      }
    [@@deriving sexp_of]

    let empty () =
      { step = 0
      ; model = 0
      ; family = 0
      ; proc_type = 0
      ; extended_model = 0
      ; extended_family = 0
      }
    ;;
  end

  module Ebx = struct
    type t =
      { mutable brand_index : int
      ; mutable clflush_line_size : int
      ; mutable max_addressable_logical_processors : int
      ; mutable initial_apic_id : int
      }
    [@@deriving sexp_of]

    let empty () =
      { brand_index = 0
      ; clflush_line_size = 0
      ; max_addressable_logical_processors = 0
      ; initial_apic_id = 0
      }
    ;;
  end

  module Ecx_flags = struct
    let sse3 = bit63 0
    let pclmulqdq = bit63 1
    let dtes64 = bit63 2
    let monitor = bit63 3
    let dscpl = bit63 4
    let vmx = bit63 5
    let smx = bit63 6
    let eist = bit63 7
    let tm2 = bit63 8
    let ssse3 = bit63 9
    let cnxtid = bit63 10
    let sdbg = bit63 11
    let fma = bit63 12
    let cmpxchg16b = bit63 13
    let xtpr_update_control = bit63 14
    let pdcm = bit63 15

    (* let reserved = bit63 16 *)
    let pcid = bit63 17
    let dca = bit63 18
    let sse4_1 = bit63 19
    let sse4_2 = bit63 20
    let x2apic = bit63 21
    let movbe = bit63 22
    let popcnt = bit63 23
    let tsc_deadline = bit63 24
    let aesni = bit63 25
    let xsave = bit63 26
    let osxsave = bit63 27
    let avx = bit63 28
    let f16c = bit63 29
    let rdrand = bit63 30

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ sse3, "sse3"
          ; pclmulqdq, "pclmulqdq"
          ; dtes64, "dtes64"
          ; monitor, "monitor"
          ; dscpl, "dscpl"
          ; vmx, "vmx"
          ; smx, "smx"
          ; eist, "eist"
          ; tm2, "tm2"
          ; ssse3, "ssse3"
          ; cnxtid, "cnxtid"
          ; sdbg, "sdbg"
          ; fma, "fma"
          ; cmpxchg16b, "cmpxchg16b"
          ; xtpr_update_control, "xtpr_update_control"
          ; pdcm, "pdcm"
          ; pcid, "pcid"
          ; dca, "dca"
          ; sse4_1, "sse4_1"
          ; sse4_2, "sse4_2"
          ; x2apic, "x2apic"
          ; movbe, "movbe"
          ; popcnt, "popcnt"
          ; tsc_deadline, "tsc_deadline"
          ; aesni, "aesni"
          ; xsave, "xsave"
          ; osxsave, "osxsave"
          ; avx, "avx"
          ; f16c, "f16c"
          ; rdrand, "rdrand"
          ]
        ;;
      end)
  end

  module Edx_flags = struct
    let fpu = bit63 0
    let vme = bit63 1
    let de = bit63 2
    let pse = bit63 3
    let tsc = bit63 4
    let msr = bit63 5
    let pae = bit63 6
    let mce = bit63 7
    let cx8 = bit63 8
    let apic = bit63 9

    (* let reserved1 = bit63 10 *)
    let sep = bit63 11
    let mtrr = bit63 12
    let pge = bit63 13
    let mca = bit63 14
    let cmov = bit63 15
    let pat = bit63 16
    let pse36 = bit63 17
    let psn = bit63 18
    let clfsh = bit63 19

    (* let reserved2 = bit63 20 *)
    let ds = bit63 21
    let acpi = bit63 22
    let mmx = bit63 23
    let fxsr = bit63 24
    let sse = bit63 25
    let sse2 = bit63 26
    let ss = bit63 27
    let htt = bit63 28
    let tm = bit63 29

    (* let reserved3 = bit63 30 *)
    let pbe = bit63 31

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ fpu, "fpu"
          ; vme, "vme"
          ; de, "de"
          ; pse, "pse"
          ; tsc, "tsc"
          ; msr, "msr"
          ; pae, "pae"
          ; mce, "mce"
          ; cx8, "cx8"
          ; apic, "apic"
          ; sep, "sep"
          ; mtrr, "mtrr"
          ; pge, "pge"
          ; mca, "mca"
          ; cmov, "cmov"
          ; pat, "pat"
          ; pse36, "pse36"
          ; psn, "psn"
          ; clfsh, "clfsh"
          ; ds, "ds"
          ; acpi, "acpi"
          ; mmx, "mmx"
          ; fxsr, "fxsr"
          ; sse, "sse"
          ; sse2, "sse2"
          ; ss, "ss"
          ; htt, "htt"
          ; tm, "tm"
          ; pbe, "pbe"
          ]
        ;;
      end)
  end

  type t =
    { eax : Eax.t
    ; ebx : Ebx.t
    ; mutable ecx : Ecx_flags.t
    ; mutable edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  external _get
    :  Eax.t
    -> Ebx.t
    -> t
    -> unit
    = "intel_cpuid_version_and_feature_information"

  let canonical_version_string t =
    let eax = t.eax in
    let model =
      if eax.family = 6 || eax.family = 15
      then (eax.extended_model lsl 4) + eax.model
      else 0
    in
    let family =
      if eax.family = 15 then eax.extended_family else eax.extended_family + eax.family
    in
    let stepping =
      match family, model with
      | 6, 0x55 when eax.step <= 4 -> "-[01234]"
      | 6, 0x55 -> "-[56789ABCDEF]"
      | _ -> ""
    in
    sprintf "%X-%X%s" family model stepping
  ;;

  let retrieve () : t =
    let t =
      { eax = Eax.empty ()
      ; ebx = Ebx.empty ()
      ; ecx = Ecx_flags.empty
      ; edx = Edx_flags.empty
      }
    in
    _get t.eax t.ebx t;
    t
  ;;

  module For_testing = struct
    let build_from_ints ~eax ~ebx ~ecx ~edx =
      let eax =
        { Eax.step = eax land 0xF
        ; model = (eax lsr 4) land 0xF
        ; family = (eax lsr 8) land 0xF
        ; proc_type = (eax lsr 12) land 0xF
        ; extended_model = (eax lsr 16) land 0xF
        ; extended_family = (eax lsr 20) land 0xFF
        }
      in
      let ebx =
        { Ebx.brand_index = ebx land 0xFF
        ; clflush_line_size = (ebx lsr 8) land 0xFF
        ; max_addressable_logical_processors = (ebx lsr 16) land 0xFF
        ; initial_apic_id = (ebx lsr 24) land 0xFF
        }
      in
      { eax; ebx; ecx = Ecx_flags.of_int ecx; edx = Edx_flags.of_int edx }
    ;;
  end
end

let canonical_identifier (t : t) =
  sprintf
    "%s-%s"
    t.brand
    (Version_and_feature_information.retrieve ()
     |> Version_and_feature_information.canonical_version_string)
;;

module Extended_feature_flags_subleaf_0 = struct
  module Ebx_flags = struct
    let fsgsbase = bit63 0
    let ia32_tsc_adjust = bit63 1
    let sgx = bit63 2
    let bmi1 = bit63 3
    let hle = bit63 4
    let avx2 = bit63 5
    let fdp_excptn_only = bit63 6
    let smep = bit63 7
    let bmi2 = bit63 8
    let enhanced_rep_movsto = bit63 9
    let invpcid = bit63 10
    let rtm = bit63 11
    let rdt_m = bit63 12
    let deprecate_fpu_csds = bit63 13
    let mpx = bit63 14
    let rdt_a = bit63 15
    let avx512f = bit63 16
    let avx512dq = bit63 17
    let rdseed = bit63 18
    let adx = bit63 19
    let smap = bit63 20
    let avx512ifma = bit63 21

    (* let reserved = bit63 22 *)
    let clflushopt = bit63 23
    let clwb = bit63 24
    let intel_processor_trace = bit63 25
    let avx512pf = bit63 26
    let avx512er = bit63 27
    let avx512cd = bit63 28
    let sha = bit63 29
    let avx512bw = bit63 30
    let avx512vl = bit63 31

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ fsgsbase, "fsgsbase"
          ; ia32_tsc_adjust, "ia32_tsc_adjust"
          ; sgx, "sgx"
          ; bmi1, "bmi1"
          ; hle, "hle"
          ; avx2, "avx2"
          ; fdp_excptn_only, "fdp_excptn_only"
          ; smep, "smep"
          ; bmi2, "bmi2"
          ; enhanced_rep_movsto, "enhanced_rep_movsto"
          ; invpcid, "invpcid"
          ; rtm, "rtm"
          ; rdt_m, "rdt_m"
          ; deprecate_fpu_csds, "deprecate_fpu_csds"
          ; mpx, "mpx"
          ; rdt_a, "rdt_a"
          ; avx512f, "avx512f"
          ; avx512dq, "avx512dq"
          ; rdseed, "rdseed"
          ; adx, "adx"
          ; smap, "smap"
          ; avx512ifma, "avx512ifma"
          ; clflushopt, "clflushopt"
          ; clwb, "clwb"
          ; intel_processor_trace, "intel_processor_trace"
          ; avx512pf, "avx512pf"
          ; avx512er, "avx512er"
          ; avx512cd, "avx512cd"
          ; sha, "sha"
          ; avx512bw, "avx512bw"
          ; avx512vl, "avx512vl"
          ]
        ;;
      end)
  end

  module Ecx_flags = struct
    let prefetchwt1 = bit63 0
    let avx512vbmi = bit63 1
    let umip = bit63 2
    let pku = bit63 3
    let ospke = bit63 4
    let waitpkg = bit63 5
    let avx512vmbi2 = bit63 6
    let cet_ss = bit63 7
    let gfni = bit63 8
    let vaes = bit63 9
    let vpclmulqdq = bit63 10
    let avx512vnni = bit63 11
    let avx512bitalg = bit63 12

    (* let reserved1 = bit63 13 *)
    let avx512vpopcntdq = bit63 14

    (* let reserved2 = bit63 15 *)
    let la57 = bit63 16

    (* mawau is 5 bits long *)
    let rdpid = bit63 22
    let keylocker = bit63 23

    (* let reserved3 = bit63 24 *)
    let cldemote = bit63 25

    (* let reserved4 = bit63 26 *)
    let movdiri = bit63 27
    let movdir64b = bit63 28

    (* let reserved4 = bit63 29 *)
    let sgx_lc = bit63 30
    let pks = bit63 31

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ prefetchwt1, "preefetchwt1"
          ; avx512vbmi, "avx512vbmi"
          ; umip, "umip"
          ; pku, "pku"
          ; ospke, "ospke"
          ; waitpkg, "waitpkg"
          ; avx512vmbi2, "avx512vmbi2"
          ; cet_ss, "cet_ss"
          ; gfni, "gfni"
          ; vaes, "vaes"
          ; vpclmulqdq, "vpclmulqdq"
          ; avx512vnni, "avx512vnni"
          ; avx512bitalg, "avx512bitalg"
          ; avx512vpopcntdq, "avx512vpopcntdq"
          ; la57, "la57"
          ; rdpid, "rdpid"
          ; keylocker, "keylocker"
          ; cldemote, "cldemote"
          ; movdiri, "movdiri"
          ; movdir64b, "movdir64b"
          ; sgx_lc, "sgx_lc"
          ; pks, "pks"
          ]
        ;;
      end)

    let mawau t =
      let mask = Int63.of_int (0x1F lsl 18) in
      let masked = Int63.bit_and t mask in
      Int63.(( lsr )) masked 18 |> Int63.to_int_trunc
    ;;
  end

  module Edx_flags = struct
    (* let reserved1 = bit63 0-1 *)
    let avx5124vnniw = bit63 2
    let avx5124fmaps = bit63 3
    let fast_short_rep_move = bit63 4

    (* let reserved2 5-7 *)
    let avx512_vp2intersect = bit63 8

    (* let reserved3 9 *)
    let md_clear = bit63 10

    (* let reserved4 11-14 *)
    let hybrid = bit63 15

    (* let reserved5 16-19 *)
    let cet_ibt = bit63 20

    (* let reserved6 21-25 *)
    let ibrs_ibpb = bit63 26
    let stibp = bit63 27
    let l1d_flush = bit63 28
    let ia32_arch_capabilities = bit63 29
    let ia32_core_capabilities = bit63 30
    let ssbd = bit63 31

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ avx5124vnniw, "avx5124vnniw"
          ; avx5124fmaps, "avx5124fmaps"
          ; fast_short_rep_move, "fast_short_rep_move"
          ; avx512_vp2intersect, "avx512_vp2intersect"
          ; md_clear, "md_clear"
          ; hybrid, "hybrid"
          ; cet_ibt, "cet_ibt"
          ; ibrs_ibpb, "ibrs_ibpb"
          ; stibp, "stibp"
          ; l1d_flush, "l1d_flush"
          ; ia32_arch_capabilities, "ia32_arch_capabilities"
          ; ia32_core_capabilities, "ia32_core_capabilities"
          ; ssbd, "ssbd"
          ]
        ;;
      end)
  end

  type t =
    { max_subleaf : int
    ; mutable ebx : Ebx_flags.t
    ; mutable ecx : Ecx_flags.t
    ; mutable edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  external _get : t -> int = "cpuid_extended_feature_flags_subleaf0"

  let retrieve () =
    let t =
      { max_subleaf = 0
      ; ebx = Ebx_flags.empty
      ; ecx = Ecx_flags.empty
      ; edx = Edx_flags.empty
      }
    in
    let max_subleaf = _get t in
    { t with max_subleaf }
  ;;

  module For_testing = struct
    let build_from_ints ~eax ~ebx ~ecx ~edx =
      { max_subleaf = eax
      ; ebx = Ebx_flags.of_int ebx
      ; ecx = Ecx_flags.of_int ecx
      ; edx = Edx_flags.of_int edx
      }
    ;;
  end
end
