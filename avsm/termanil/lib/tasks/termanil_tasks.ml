(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Termanil_model
module C = Dooit.Common

let task ?store n =
  let sources =
    C.items "links" n.Dooit.Doc.meta
    |> List.filter_map (fun l ->
           if C.field "type" l <> "jmap-email" then None
           else
             let target = C.get "target" l in
             Some
               {
                 service = C.field "service" target;
                 account = C.field "account_id" target;
                 id = C.field "email_id" target;
               })
  in
  {
    id = Dooit.Doc.id n;
    revision = Dooit.Doc.revision n;
    title = Dooit.Doc.title n;
    updated =
      (match store with
      | None -> C.field "created_at" n.meta
      | Some store -> (
          let path = Dooit.Store.note_path store (Dooit.Doc.id n) in
          let time = (Unix.stat path).Unix.st_mtime in
          match Ptime.of_float_s time with
          | None -> C.field "created_at" n.meta
          | Some t -> Ptime.to_rfc3339 ~frac_s:6 ~tz_offset_s:0 t));
    status = Dooit.Doc.status n;
    tags = Dooit.Doc.tags n;
    body = n.body;
    sources;
    threads =
      C.items "links" n.Dooit.Doc.meta
      |> List.filter_map (fun l ->
             if C.field "type" l <> "jmap-email" then None
             else
               let target = C.get "target" l in
               match Option.bind (C.find "hints" l) (C.find "thread_id") with
               | None -> None
               | Some id ->
                   Some
                     {
                       service = C.field "service" target;
                       account = C.field "account_id" target;
                       id = C.string id;
                     });
  }

let list store =
  let notes, errors = Dooit.Index.query store in
  ( List.map (task ~store) notes,
    List.map (fun (path, why) -> path ^ ": " ^ why) errors )

let capture ?(tags = [ "inbox" ]) client (m : message) =
  let store = Dooit.Client.store client in
  let s = m.source in
  let target =
    Dooit.Link.email ~service:s.service ~account:s.account ~email:s.id
  in
  let hints =
    C.obj [ ("subject", C.str m.subject); ("thread_id", C.str m.thread_id) ]
  in
  let existing, _ = Eio_unix.run_in_systhread (fun () -> list store) in
  let linked =
    List.filter
      (fun (n : task) ->
        List.exists (same_email m.source) n.sources
        || List.exists (same_email { m.source with id = m.thread_id }) n.threads)
      existing
  in
  if linked <> [] then linked
  else
    let notes =
      Dooit.Client.capture ~tags ~hints ~title:m.subject ~policy:Synchronize
        client target
    in
    Eio_unix.run_in_systhread (fun () -> List.map (task ~store) notes)

let complete client (n : task) =
  let store = Dooit.Client.store client in
  let result =
    Dooit.Client.apply client ~policy:Synchronize ~check:Eio.Fiber.check
      ~id:n.id ~expected:(Some n.revision) ~operation:(C.new_uuid ())
      ~payload:"termanil:complete/v1" (function
      | None -> C.fail "task disappeared"
      | Some doc when Dooit.Doc.deleted doc -> C.fail "task was deleted"
      | Some doc -> Dooit.Doc.change "status" (Some (C.str "done")) doc)
  in
  let note = Dooit.Client.saved result in
  Eio_unix.run_in_systhread (fun () -> task ~store note)
