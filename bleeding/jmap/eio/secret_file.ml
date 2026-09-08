(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let not_regular ~subject = Fmt.str "%s is not a regular file" subject

let too_open ~subject permissions =
  Fmt.str
    "%s has permissions %03o; remove group and other access (for example, \
     chmod 600)"
    subject permissions

let with_open_in ~subject ~refused path f =
  (* Opening a FIFO for reading blocks until a writer arrives, so the kind is
     read before the open rather than from the descriptor alone. A missing
     path is opened anyway, so that the open reports why. The check after the
     open is the authoritative one, the path having been able to change in
     between. *)
  match Eio.Path.kind ~follow:true path with
  | `Regular_file | `Not_found ->
      Eio.Path.with_open_in path (fun file ->
          let stat = Eio.File.stat file in
          if stat.Eio.File.Stat.kind <> `Regular_file then
            refused (not_regular ~subject)
          else if stat.Eio.File.Stat.perm land 0o077 <> 0 then
            refused (too_open ~subject stat.Eio.File.Stat.perm)
          else f file)
  | _ -> refused (not_regular ~subject)
