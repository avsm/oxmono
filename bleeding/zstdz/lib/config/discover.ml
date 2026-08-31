(* Finds the zstd headers and library. pkg-config knows the paths on most
   systems. Homebrew keeps headers and libraries out of the default search
   path on arm64 macOS, so when pkg-config is missing or does not know the
   package, the Homebrew prefixes are probed directly. *)
let () =
  let module C = Configurator.V1 in
  C.main ~name:"zstdz" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lzstd" ]; cflags = [] }
      in
      let prefix_probe () =
        let prefixes = [ "/opt/homebrew"; "/usr/local" ] in
        match
          List.find_opt
            (fun p -> Sys.file_exists (p ^ "/include/zstd.h"))
            prefixes
        with
        | Some p ->
          { C.Pkg_config.libs = [ "-L" ^ p ^ "/lib"; "-lzstd" ];
            cflags = [ "-I" ^ p ^ "/include" ] }
        | None -> default
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> prefix_probe ()
        | Some pc ->
          (match C.Pkg_config.query pc ~package:"libzstd" with
           | Some conf -> conf
           | None -> prefix_probe ())
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
