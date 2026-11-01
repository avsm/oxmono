(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module M = Fetch.Middleware

let src = Logs.Src.create "xrpc.http" ~doc:"HTTP exchanges of XRPC clients"

module Log = (val Logs.src_log src : Logs.LOG)

let meth = Logs.Tag.def "method" Fmt.string
let host = Logs.Tag.def "host" Fmt.string
let nsid = Logs.Tag.def "nsid" Fmt.string
let target = Logs.Tag.def "target" Fmt.string
let status = Logs.Tag.def "status" Fmt.int
let elapsed = Logs.Tag.def "elapsed" Fmt.float

let since start =
  Ptime.Span.to_float_s (Ptime.diff (Ptime_clock.now ()) start)

(* The path and query of [request]'s target, with secret parameters
   redacted. *)
let redacted_target (request : M.request) =
  let url = Fmt.str "%a" (M.pp_url request) request.url in
  let origin = M.Url.origin request.url in
  if String.starts_with ~prefix:origin url then
    String.sub url (String.length origin)
      (String.length url - String.length origin)
  else url

let nsid_of path =
  let prefix = "/xrpc/" in
  if String.starts_with ~prefix path then
    let rest =
      String.sub path (String.length prefix)
        (String.length path - String.length prefix)
    in
    match String.index_opt rest '?' with
    | Some i -> Some (String.sub rest 0 i)
    | None -> Some rest
  else None

let middleware next ~sw (request : M.request) =
  if Logs.Src.level src = None then next ~sw request
  else
    let start = Ptime_clock.now () in
    let target_ = redacted_target request in
    let tags =
      Logs.Tag.(
        empty
        |> add meth (Http.Method.to_string request.meth)
        |> add host (M.Url.host request.url)
        |> add target target_)
    in
    let tags =
      match nsid_of target_ with
      | Some n -> Logs.Tag.add nsid n tags
      | None -> tags
    in
    Log.debug (fun m -> m ~tags "%a" M.pp_request request);
    match next ~sw request with
    | response ->
        let code = M.status response in
        let tags =
          tags
          |> Logs.Tag.add status code
          |> Logs.Tag.add elapsed (since start)
        in
        Log.info (fun m ->
            m ~tags "%s %s%s -> %d"
              (Http.Method.to_string request.meth)
              (M.Url.host request.url) target_ code);
        Log.debug (fun m -> m ~tags "%a" M.pp_response response);
        response
    | exception (Eio.Io _ as ex) ->
        let tags = Logs.Tag.add elapsed (since start) tags in
        Log.warn (fun m ->
            m ~tags "%s %s%s failed: %a"
              (Http.Method.to_string request.meth)
              (M.Url.host request.url) target_ Eio.Exn.pp ex);
        raise ex

let wrap http = M.middleware middleware http

let exchange ~meth:m ~host:h ~target:t ?status:code ~elapsed:e () =
  let tags =
    Logs.Tag.(
      empty |> add meth m |> add host h |> add target t |> add elapsed e)
  in
  let tags =
    match nsid_of t with Some n -> Logs.Tag.add nsid n tags | None -> tags
  in
  match code with
  | Some code ->
      Log.info (fun f ->
          f ~tags:(Logs.Tag.add status code tags) "%s %s%s -> %d" m h t code)
  | None -> Log.warn (fun f -> f ~tags "%s %s%s failed" m h t)
