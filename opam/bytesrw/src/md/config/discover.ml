module C = Configurator.V1

let () =
  C.main ~name:"bytesrw_md" (fun c ->
      let default : C.Pkg_config.package_conf =
        { libs = [ "-lmd" ]; cflags = [ "-DHAS_LIBMD" ] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            (match C.Pkg_config.query pc ~package:"libmd" with
             | None -> default
             | Some conf -> { conf with cflags = "-DHAS_LIBMD" :: conf.cflags })
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
