open Core

type t = Cpuid_intf.Basic_info.t

let bit63 n = Int63.(one lsl n)
let kind = Type_equal.Id.create ~name:"AMD CPUID" [%sexp_of: _]
let create = Fn.id
let maximum_leaf (t : t) = t.maximum_value

module Version_and_feature_information = struct
  module Eax = struct
    type t =
      { mutable step : int
      ; mutable model : int
      ; mutable family : int
      ; mutable extended_model : int
      ; mutable extended_family : int
      }
    [@@deriving sexp_of]

    let empty () =
      { step = 0; model = 0; family = 0; extended_family = 0; extended_model = 0 }
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
    let monitor = bit63 3
    let ssse3 = bit63 9
    let fma = bit63 12
    let cmpxchg16b = bit63 13
    let sse4_1 = bit63 19
    let sse4_2 = bit63 20
    let popcnt = bit63 23
    let aes = bit63 25
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
          ; monitor, "monitor"
          ; ssse3, "ssse3"
          ; fma, "fma"
          ; cmpxchg16b, "cmpxchg16b"
          ; sse4_1, "sse4_1"
          ; sse4_2, "sse4_2"
          ; popcnt, "popcnt"
          ; aes, "aes"
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
    let cmpxchg8b = bit63 8
    let apic = bit63 9
    let sysentersysexit = bit63 11
    let mtrr = bit63 12
    let pge = bit63 13
    let mca = bit63 14
    let cmov = bit63 15
    let pat = bit63 16
    let pse36 = bit63 17
    let clfsh = bit63 19
    let mmx = bit63 23
    let fxsr = bit63 24
    let sse = bit63 25
    let sse2 = bit63 26
    let htt = bit63 28

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
          ; cmpxchg8b, "cmpxchg8b"
          ; apic, "apic"
          ; sysentersysexit, "sysentersysexit"
          ; mtrr, "mtrr"
          ; pge, "pge"
          ; mca, "mca"
          ; cmov, "cmov"
          ; pat, "pat"
          ; pse36, "pse36"
          ; clfsh, "clfsh"
          ; mmx, "mmx"
          ; fxsr, "fxsr"
          ; sse, "sse"
          ; sse2, "sse2"
          ; htt, "htt"
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

  let canonical_version_string t =
    let eax = t.eax in
    let model =
      if eax.family < 0xF then eax.model else (eax.extended_model lsl 4) lor eax.model
    in
    let family =
      if eax.family < 0xF then eax.family else eax.extended_family + eax.family
    in
    sprintf "%X-%X" family model
  ;;

  external _get
    :  Eax.t
    -> Ebx.t
    -> t
    -> unit
    = "amd_cpuid_version_and_feature_information"

  let retrieve () =
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
      let ecx = Ecx_flags.of_int ecx in
      let edx = Edx_flags.of_int edx in
      let eax =
        { Eax.step = eax land 0xF
        ; model = (eax lsr 4) land 0xF
        ; family = (eax lsr 8) land 0xF
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
      { eax; ebx; ecx; edx }
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
    let bmi1 = bit63 3
    let avx2 = bit63 5
    let smep = bit63 7
    let bmi2 = bit63 8
    let avx512f = bit63 16
    let avx512dq = bit63 17
    let rdseed = bit63 18
    let adx = bit63 19
    let smap = bit63 20
    let avx512ifma = bit63 21
    let rdpid = bit63 22
    let clflushopt = bit63 23
    let clwb = bit63 24
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
          ; bmi1, "bmi1"
          ; avx2, "avx2"
          ; smep, "smep"
          ; bmi2, "bmi2"
          ; avx512f, "avx512f"
          ; avx512dq, "avx512dq"
          ; rdseed, "rdseed"
          ; adx, "adx"
          ; smap, "smap"
          ; avx512ifma, "avx512ifma"
          ; rdpid, "rdpid"
          ; clflushopt, "clflushopt"
          ; clwb, "clwb"
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
    let avx512vbmi = bit63 1
    let umip = bit63 2
    let pku = bit63 3
    let ospke = bit63 4
    let avx512vmbi2 = bit63 6
    let cet_ss = bit63 7
    let vaes = bit63 9
    let vpcmulqdq = bit63 10
    let avx512vnni = bit63 11
    let avx512bitalg = bit63 12
    let avx512vpopcntdq = bit63 14

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ avx512vbmi, "avx512vbmi"
          ; umip, "umip"
          ; pku, "pku"
          ; ospke, "ospke"
          ; avx512vmbi2, "avx512vmbi2"
          ; cet_ss, "cet_ss"
          ; vaes, "vaes"
          ; vpcmulqdq, "vpcmulqdq"
          ; avx512vnni, "avx512vnni"
          ; avx512bitalg, "avx512bitalg"
          ; avx512vpopcntdq, "avx512vpopcntdq"
          ]
        ;;
      end)
  end

  module Edx_flags = struct
    let avx5124vnniw = bit63 2
    let avx5124fmaps = bit63 3
    let avx512_vp2intersect = bit63 8

    include Flags.Make (struct
        let allow_intersecting = false
        let should_print_error = true
        let remove_zero_flags = false

        let known =
          [ avx5124vnniw, "avx5124vnniw"
          ; avx5124fmaps, "avx5124fmaps"
          ; avx512_vp2intersect, "avx512_vp2intersect"
          ]
        ;;
      end)
  end

  type t =
    { max_subleaf : int
    ; ebx : Ebx_flags.t
    ; ecx : Ecx_flags.t
    ; edx : Edx_flags.t
    }
  [@@deriving sexp_of]

  module Raw_arg = struct
    type t =
      { mutable ignored : int
      ; mutable ebx : Ebx_flags.t
      ; mutable ecx : Ecx_flags.t
      ; mutable edx : Edx_flags.t
      }
  end

  external _get : Raw_arg.t -> int = "cpuid_extended_feature_flags_subleaf0"

  let retrieve () =
    let raw_arg =
      { ignored = 0
      ; Raw_arg.ebx = Ebx_flags.empty
      ; ecx = Ecx_flags.empty
      ; edx = Edx_flags.empty
      }
    in
    let max_subleaf = _get raw_arg in
    { max_subleaf; ebx = raw_arg.ebx; ecx = raw_arg.ecx; edx = raw_arg.edx }
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
