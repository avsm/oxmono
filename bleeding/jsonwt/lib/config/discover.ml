(* Locate OpenSSL through pkg-config. This keeps Homebrew, MacPorts, Linux and
   cross-compilation toolchains out of the library's build description. *)
let () =
  let module C = Configurator.V1 in
  C.main ~name:"jsonwt" (fun c ->
      let fallback = { C.Pkg_config.libs = [ "-lcrypto" ]; cflags = [] } in
      let conf =
        match C.Pkg_config.get c with
        | None -> fallback
        | Some pc -> Option.value (C.Pkg_config.query pc ~package:"openssl") ~default:fallback
      in
      C.Flags.write_sexp "c_flags.sexp" conf.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" conf.libs)
