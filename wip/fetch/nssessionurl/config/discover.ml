module C = Configurator.V1

let () =
  C.main ~name:"nssessionurl" (fun c ->
      let macos =
        match C.ocaml_config_var c "system" with
        | Some "macosx" -> true
        | _ -> false
      in
      let c_flags, library_flags =
        if macos then
          ([ "-x"; "objective-c"; "-fobjc-arc" ], [ "-framework"; "Foundation" ])
        else ([], [])
      in
      C.Flags.write_sexp "c_flags.sexp" c_flags;
      C.Flags.write_sexp "c_library_flags.sexp" library_flags)
