module C = Configurator.V1

let () =
  C.main ~name:"bytesrw_blake3" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lblake3" ]; cflags = [ "-DHAS_BLAKE3" ] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            (match C.Pkg_config.query pc ~package:"libblake3" with
             | None -> default
             | Some conf -> { conf with cflags = "-DHAS_BLAKE3" :: conf.cflags })
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
