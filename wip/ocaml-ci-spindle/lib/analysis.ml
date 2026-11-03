(* SPDX-License-Identifier: ISC *)

type t = { opam_files : (string * string) list }

let find (files : Spindle.Executor.files) =
  let opam =
    files.list ""
    |> List.filter (fun f -> Filename.check_suffix f ".opam")
    |> List.sort String.compare
    |> List.filter_map (fun f ->
        Option.map (fun text -> (f, text)) (files.read f))
  in
  if opam = [] then None else Some { opam_files = opam }

let packages t =
  List.map
    (fun (f, text) -> (Filename.chop_suffix f ".opam", text))
    t.opam_files
