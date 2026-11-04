(* SPDX-License-Identifier: ISC *)

type t = { opam_files : (string * string) list }

(* opam's own rule for package names, which keeps them safe in shell commands
   and OBuilder specs. *)
let valid_name n =
  n <> ""
  && n.[0] <> '-'
  && String.exists (function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false) n
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '+' -> true
         | _ -> false)
       n

let find (files : Spindle.Executor.files) =
  let opam =
    files.list ""
    |> List.filter (fun f ->
        Filename.check_suffix f ".opam"
        && valid_name (Filename.chop_suffix f ".opam"))
    |> List.sort String.compare
    |> List.filter_map (fun f ->
        Option.map (fun text -> (f, text)) (files.read f))
  in
  if opam = [] then None else Some { opam_files = opam }

let packages t =
  List.map
    (fun (f, text) -> (Filename.chop_suffix f ".opam", text))
    t.opam_files
