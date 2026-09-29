(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Build-time backend C-flag discovery.

   Emits the flag files consumed by the engine-archive rules in ../lib_metal,
   ../lib_cpu and ../lib_cuda via %{read-lines:...}:

     core_cflags          - flags for the C core (ds4.c, ds4_ssd.c, ...)
     engram_cflags        - core_cflags without -ffast-math, for ds4_engram.c
     objc_cflags          - flags for the Metal backend (ds4_metal.m)
     cuda_nvcc            - the nvcc to run for ds4_cuda.cu
     cuda_cflags          - flags for that nvcc invocation
     cuda_link_flags.sexp - the CUDA runtime libraries to link against, written
                            as a sexp because ../lib_cuda takes it with
                            (:include ...) rather than %{read-lines:...}

   The only platform difference in the C flags is the native-arch flag and a
   couple of Linux knobs; everything else mirrors the upstream Makefile.

   The three cuda_* files are written unconditionally, because this rule is not
   gated, but they are read only by ../lib_cuda, which is. A host without CUDA
   therefore gets unusable values in files nothing opens. Set DS4_CUDA=yes to
   turn the CUDA backend on, and this discovery becomes load-bearing: a missing
   toolkit then fails the build here, with the variable to set, rather than as a
   bare "nvcc: not found" from a compile rule. *)

module C = Configurator.V1

let getenv v = match Sys.getenv_opt v with Some "" -> None | o -> o

(* [cuda_requested ()] is whether the CUDA backend is switched on, and matches
   the (enabled_if ...) test in ../lib_cuda/dune. Any value other than yes or no
   is refused here: ../lib_cuda/dune can only test for equality, so a typo would
   otherwise skip the backend silently and leave the caller hunting for a
   library that never got built. *)
let cuda_requested () =
  match getenv "DS4_CUDA" with
  | None | Some "no" -> false
  | Some "yes" -> true
  | Some other ->
      C.die
        "DS4_CUDA must be 'yes' or 'no', not %S. Build the CUDA backend with \
         DS4_CUDA=yes dune build."
        other

(* [cuda_nvcc c] is the nvcc to compile ds4_cuda.cu with: $DS4_NVCC, else the
   one on the PATH, else the one under $DS4_CUDA_HOME. *)
let cuda_nvcc c =
  match getenv "DS4_NVCC" with
  | Some path when Sys.file_exists path -> Some path
  | Some path ->
      (* Named outright, so a wrong path is a mistake to report, not a reason to
         go looking for another compiler. *)
      C.die "DS4_NVCC is set to %S, which does not exist." path
  | None -> (
      match C.which c "nvcc" with
      | Some path -> Some path
      | None ->
          let candidate =
            Filename.concat
              (Option.value (getenv "DS4_CUDA_HOME") ~default:"/usr/local/cuda")
              "bin/nvcc"
          in
          if Sys.file_exists candidate then Some candidate else None)

(* [cuda_home nvcc] is the toolkit root, taken from $DS4_CUDA_HOME or from the
   directory two levels above nvcc, which is where a toolkit puts it. *)
let cuda_home nvcc =
  match getenv "DS4_CUDA_HOME" with
  | Some dir -> dir
  | None -> Filename.dirname (Filename.dirname nvcc)

(* [cuda_libdir home] is the first of the toolkit's library directories that
   exists. A distribution package puts them in lib64; the tarball installs a
   per-target tree as well, which is the only one populated on aarch64. *)
let cuda_libdir home =
  let candidates =
    [
      "lib64";
      "lib/x86_64-linux-gnu";
      "targets/x86_64-linux/lib";
      "targets/sbsa-linux/lib";
      "lib";
    ]
  in
  let exists d = Sys.file_exists (Filename.concat home d) in
  match List.find_opt exists candidates with
  | Some d -> Filename.concat home d
  | None -> Filename.concat home "lib64"

let () =
  C.main ~name:"ds4" (fun c ->
      let system =
        match C.ocaml_config_var c "system" with Some s -> s | None -> ""
      in
      let is_macos = system = "macosx" in
      (* -Wall/-Wextra are useful, but the vendored engine (ds4.c) carries a
         number of unused statics and parameters; silence just those two so the
         build stays quiet without dropping the rest of the warning coverage. *)
      let base =
        [
          "-O3";
          "-ffast-math";
          "-g";
          "-Wall";
          "-Wextra";
          "-Wno-unused-function";
          "-Wno-unused-parameter";
        ]
      in
      (* Apple clang spells "tune for this machine" -mcpu; GCC/clang on
         Linux/x86 use -march. *)
      let arch = if is_macos then [ "-mcpu=native" ] else [ "-march=native" ] in
      (* Linux needs _GNU_SOURCE for some libc symbols the engine uses, and
         -ffast-math there implies -ffinite-math-only, which the engine's NaN/Inf
         guards rely on NOT being set (upstream adds -fno-finite-math-only). *)
      let linux_extra =
        if is_macos then [] else [ "-D_GNU_SOURCE"; "-fno-finite-math-only" ]
      in
      let core = base @ arch @ [ "-std=c99" ] @ linux_extra in
      (* Objective-C uses ARC instead of -std=c99; only built on macOS. *)
      let objc = base @ arch @ [ "-fobjc-arc" ] in
      C.Flags.write_lines "core_cflags" core;
      (* The Engram tables hold BF16-rounded values that must be reproduced
         exactly, so upstream builds ds4_engram.c without -ffast-math. *)
      C.Flags.write_lines "engram_cflags"
        (List.filter (fun f -> f <> "-ffast-math") core);
      C.Flags.write_lines "objc_cflags" objc;
      let requested = cuda_requested () in
      let nvcc = cuda_nvcc c in
      (match (requested, nvcc) with
      | true, None ->
          C.die
            "DS4_CUDA=yes but no nvcc was found. Put the CUDA toolkit's bin \
             directory on the PATH, or set DS4_CUDA_HOME to the toolkit root, \
             or DS4_NVCC to the compiler itself."
      | _ -> ());
      let nvcc = Option.value nvcc ~default:"nvcc" in
      let home = cuda_home nvcc in
      (* The GPU to generate code for. "native" is the machine doing the build,
         which matches the -march=native above and needs a GPU present at build
         time; set DS4_CUDA_ARCH (sm_89 for an L4, sm_90 for an H100) when
         building somewhere else, or for more than one target. *)
      let cuda_arch = Option.value (getenv "DS4_CUDA_ARCH") ~default:"native" in
      let cuda_cflags =
        [
          "-O3";
          "-g";
          "-lineinfo";
          "--use_fast_math";
          "-arch=" ^ cuda_arch;
          "-Xcompiler";
          "-march=native";
          "-Xcompiler";
          "-pthread";
          (* ds4_cuda.cu has thread-local statics, which without -fPIC compile
             to the local-exec model and emit relocations the linker refuses in
             a shared object. The other engine objects have no TLS and so do not
             need this. *)
          "-Xcompiler";
          "-fPIC";
        ]
      in
      (* ds4_cuda.cu is C++ and uses std::unordered_map and exceptions, so the
         C++ runtime has to be named here: the final link is driven by ocamlopt,
         not by nvcc, which would otherwise add it. *)
      let cuda_link_flags =
        [ "-L" ^ cuda_libdir home; "-lcudart"; "-lcublas"; "-lstdc++" ]
      in
      C.Flags.write_lines "cuda_nvcc" [ nvcc ];
      C.Flags.write_lines "cuda_cflags" cuda_cflags;
      C.Flags.write_sexp "cuda_link_flags.sexp" cuda_link_flags)
