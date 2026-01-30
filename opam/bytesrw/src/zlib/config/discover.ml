module C = Configurator.V1

let () =
  C.main ~name:"bytesrw_zlib" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lz" ]; cflags = [ "-DHAS_ZLIB" ] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            (match C.Pkg_config.query pc ~package:"zlib" with
             | None -> default
             | Some conf -> { conf with cflags = "-DHAS_ZLIB" :: conf.cflags })
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
