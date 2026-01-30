(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let read_string fs path =
  Eio.Path.(load (fs / path))

let of_file fs path =
  try
    let content = read_string fs path in
    Frontmatter.of_string ~fname:path content
  with
  | Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) ->
    Error (Printf.sprintf "File not found: %s" path)
  | Eio.Io (_, _) as e ->
    Error (Printf.sprintf "Error reading %s: %s" path (Printexc.to_string e))

let of_file_exn fs path =
  match of_file fs path with
  | Ok t -> t
  | Error msg -> failwith msg
