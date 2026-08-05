let () =
  let module C = Configurator.V1 in
  C.main ~name:"sqlite3" (fun c ->
      let system =
        match C.ocaml_config_var c "system" with
        | Some s -> s
        | None -> ""
      in
      let is_macosx = String.equal system "macosx" in
      let is_mingw =
        String.equal system "mingw" || String.equal system "mingw64"
      in
      (* Flags for compiling the OCaml stubs *)
      let cflags =
        if is_macosx then [ "-DSQLITE3_DISABLE_LOADABLE_EXTENSIONS" ] else []
      in
      (* Linker flags *)
      let c_library_flags = if is_mingw then [] else [ "-lpthread" ] in
      (* Flags for compiling the vendored sqlite3.c amalgamation *)
      let native_c_flags =
        match C.ocaml_config_var c "ocamlc_cflags" with
        | Some s ->
            String.split_on_char ' ' s
            |> List.filter (fun s -> not (String.equal s ""))
        | None -> []
      in
      let vendor_flags =
        [ "-O2"; "-DSQLITE_ENABLE_FTS5"; "-DSQLITE_ENABLE_FTS3";
          "-DSQLITE_ENABLE_FTS4" ]
        @ (if is_macosx then [ "-DSQLITE_OMIT_LOAD_EXTENSION" ] else [])
        @ native_c_flags
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" c_library_flags;
      let oc = open_out "c_vendor_flags" in
      List.iter (fun flag -> output_string oc (flag ^ "\n")) vendor_flags;
      close_out oc)
