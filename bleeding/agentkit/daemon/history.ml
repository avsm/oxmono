(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Journal = Agentkit.Journal

type fired = { due : float; serial : int }
type t = (string, fired) Hashtbl.t

let empty : t = Hashtbl.create 1
let fired t id = Hashtbl.find_opt t id
let note t id f = Hashtbl.replace t id f

let read dir =
  let t : t = Hashtbl.create 8 in
  if Eio.Path.is_directory dir then
    Journal.iter ~kinds:[ "wake" ] dir (fun r ->
        match r.Journal.kind with
        | Journal.Wake w ->
            (* A due time that cannot be read is not a firing that did not
               happen, so the record's own time stands in for it. Both are
               written by this program, so the two agree in every journal it
               wrote. *)
            let due =
              match Agentkit.Utc.of_rfc3339 w.Journal.due with
              | Some t -> t
              | None ->
                  Option.value ~default:0.
                    (Agentkit.Utc.of_rfc3339 r.Journal.time)
            in
            note t w.Journal.task
              { due; serial = Option.value ~default:0 w.Journal.serial }
        | _ -> ());
  t
