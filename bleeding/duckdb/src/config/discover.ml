let brew_prefix pkg =
  try
    let ic =
      Unix.open_process_in (Printf.sprintf "brew --prefix %s 2>/dev/null" pkg)
    in
    let line = try Some (input_line ic) with End_of_file -> None in
    let _ = Unix.close_process_in ic in
    match line with
    | Some s when String.length s > 0 && Sys.file_exists (s ^ "/include") ->
        Some s
    | _ -> None
  with _ -> None

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
      (* Detect Homebrew packages for vendored C++ amalgamation deps *)
      let brew_packages = [ "zstd"; "c-blosc" ] in
      let brew_include_flags =
        List.filter_map
          (fun pkg ->
            match brew_prefix pkg with
            | Some prefix -> Some [ "-I"; prefix ^ "/include" ]
            | None -> None)
          brew_packages
        |> List.flatten
      in
      let brew_lib_flags =
        List.filter_map
          (fun pkg ->
            match brew_prefix pkg with
            | Some prefix -> Some [ "-L"; prefix ^ "/lib" ]
            | None -> None)
          brew_packages
        |> List.flatten
      in
      (* Flags for compiling the OCaml stubs *)
      let cflags = [ "-I"; "../vendor" ] in
      (* Linker flags *)
      let c_library_flags =
        if is_mingw then [ "-lstdc++" ]
        else [ "-lpthread"; "-lstdc++"; "-lm" ] @ brew_lib_flags
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
        @ brew_include_flags
      in
      C.Flags.write_sexp "c_flags.sexp" cflags;
      C.Flags.write_sexp "c_library_flags.sexp" c_library_flags;
      let oc = open_out "cxx_vendor_flags" in
      List.iter (fun flag -> output_string oc (flag ^ "\n")) vendor_flags;
      close_out oc)
