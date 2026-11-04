(* SPDX-License-Identifier: ISC *)
(* [plan OPAM_REPOSITORY PROJECT] prints the builds ocaml-ci-spindle would
   submit for the opam project in directory PROJECT, solved against the
   opam-repository clone OPAM_REPOSITORY. Nothing is submitted. *)
open Ocaml_ci_spindle

let () =
  let opam_repository = Sys.argv.(1) and project = Sys.argv.(2) in
  let files : Spindle.Executor.files =
    {
      list =
        (fun dir -> Array.to_list (Sys.readdir (Filename.concat project dir)));
      read =
        (fun path ->
          let path = Filename.concat project path in
          if Sys.file_exists path then
            Some (In_channel.with_open_bin path In_channel.input_all)
          else None);
    }
  in
  let a = Option.get (Analysis.find files) in
  let packages = Filename.concat opam_repository "packages" in
  List.iter
    (fun (v : Variant.t) ->
      let t0 = Unix.gettimeofday () in
      match
        Solve.solve
          {
            platform = v.vars;
            repositories = [ packages ];
            pins = Analysis.packages a;
            compiler = v.compiler;
            requires = v.requires;
          }
      with
      | Error why -> Printf.printf "%s: no solution\n%s\n" v.name why
      | Ok solution ->
          Printf.printf "%s: %d packages in %.1fs\n%s\n" v.name
            (List.length solution)
            (Unix.gettimeofday () -. t0)
            (String.concat " " solution);
          print_string
            (Spec.v ~variant:v ~opam_repository:"COMMIT"
               ~opam_files:(List.map fst a.opam_files)
               ~packages:solution ()))
    (let chosen = Variant.choose ~packages [ "auto" ] in
     chosen
     @
     match chosen with
     | newest :: _ ->
         List.filter_map
           (fun (_, platform) ->
             match platform with
             | Variant.Debian X86_64 -> None
             | platform -> Some (Variant.v ~platform (Variant.version newest)))
           Variant.platforms
     | [] -> [])
