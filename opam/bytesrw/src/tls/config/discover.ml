module C = Configurator.V1

let () =
  C.main ~name:"bytesrw_tls" (fun c ->
      let os =
        match C.ocaml_config_var c "system" with
        | Some s -> s
        | None -> ""
      in
      let darwin = os = "macosx" in
      let default_libs =
        if darwin then
          [ "-lmbedtls"; "-lmbedx509"; "-lmbedcrypto";
            "-framework"; "Security"; "-framework"; "CoreFoundation" ]
        else
          [ "-lmbedtls"; "-lmbedx509"; "-lmbedcrypto" ]
      in
      let default : C.Pkg_config.package_conf =
        { libs = default_libs; cflags = [ "-DHAS_MBEDTLS" ] }
      in
      let conf =
        match C.Pkg_config.get c with
        | None -> default
        | Some pc ->
            (match C.Pkg_config.query pc ~package:"mbedtls" with
             | None -> default
             | Some conf ->
                 let libs =
                   (* Also need mbedx509 *)
                   match C.Pkg_config.query pc ~package:"mbedx509" with
                   | None -> conf.libs
                   | Some x509 -> conf.libs @ x509.libs
                 in
                 let libs =
                   if darwin then
                     libs @ [ "-framework"; "Security"; "-framework"; "CoreFoundation" ]
                   else
                     libs
                 in
                 { cflags = "-DHAS_MBEDTLS" :: conf.cflags; libs })
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
