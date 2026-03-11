let () =
  let module C = Configurator.V1 in
  C.main ~name:"duckdb" (fun c ->
      let system =
        match C.ocaml_config_var c "system" with
        | Some s -> s
        | None -> ""
      in
      let is_mingw =
        String.equal system "mingw" || String.equal system "mingw64"
      in
      (* Flags for compiling the OCaml stubs *)
      let cflags = [ "-I"; "../vendor" ] in
      (* Linker flags *)
      let c_library_flags =
        if is_mingw then [ "-lstdc++" ]
        else [ "-lpthread"; "-lstdc++"; "-lm" ]
      in
      (* Flags for compiling the vendored duckdb.cpp amalgamation *)
      let native_c_flags =
        match C.ocaml_config_var c "ocamlc_cflags" with
        | Some s ->
            String.split_on_char ' ' s
            |> List.filter (fun s -> not (String.equal s ""))
        | None -> []
      in
      let vendor_flags =
        [ "-O2"; "-std=c++11"; "-DNDEBUG";
          "-DDUCKDB_BUILD_LIBRARY";
          "-w" ]
        @ native_c_flags
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" c_library_flags;
      let oc = open_out "cxx_vendor_flags" in
      List.iter (fun flag -> output_string oc (flag ^ "\n")) vendor_flags;
      close_out oc)
