(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Cmdliner

let port_arg =
  let doc = "TCP port to listen on." in
  Arg.(value & opt int 8380 & info [ "port" ] ~docv:"PORT" ~doc)

let info =
  let doc = "Serve the contact database over a local web UI." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Serves a small HTML interface for browsing, searching, adding, \
         editing and deleting contacts. It listens on 127.0.0.1 only and has \
         no authentication, so treat access to the port as full access to the \
         data.";
      `P
        "When the data directory is a git repository, every change made in \
         the browser is committed, exactly as the corresponding subcommand \
         would commit it.";
      `P "Runs until interrupted.";
    ]
  in
  Cmd.info "serve" ~doc ~man

(* The store, the git wrapper and the filesystem all belong to this domain.
   They reach the handlers as a record of closures because a proffer handler is
   portable and so may not capture them. *)
let web_env xdg stdenv =
  let store = Sortal.create_from_xdg xdg in
  let git = Sortal.Git_store.create store stdenv in
  let versioned = Sortal.Git_store.is_initialized git in
  let save contact =
    if versioned then Sortal.Git_store.save git contact
    else (
      Sortal.save store contact;
      Ok ())
  in
  let delete handle =
    if versioned then Sortal.Git_store.delete git handle
    else (
      Sortal.delete store handle;
      Ok ())
  in
  let thumbnail handle =
    match Sortal.lookup store handle with
    | None -> None
    | Some contact -> (
      match Sortal.png_thumbnail_path store contact with
      | None -> None
      | Some path -> ( try Some (Eio.Path.load path) with _ -> None))
  in
  {
    Sortal_web.list_contacts = (fun () -> Sortal.list store);
    lookup = Sortal.lookup store;
    search = Sortal.search_all store;
    save;
    delete;
    thumbnail;
  }

let on_event (e : Proffer_httpz.event) =
  let e = Proffer_httpz.globalize_event e in
  Logs.info (fun m ->
    m "%s %s %s %d %dB %.1fms" e.remote_addr
      (Proffer.Method.to_string e.meth)
      e.target
      (Proffer.Status.code e.status)
      e.body_size
      (float_of_int e.duration_us /. 1000.))

let on_error exn = Logs.err (fun m -> m "%s" (Printexc.to_string exn))

let cmd ~port xdg stdenv =
  let env = web_env xdg stdenv in
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  Logs.app (fun m ->
    m "Sortal web UI on http://127.0.0.1:%d (Ctrl-C to stop)" port);
  (try
     Eio.Switch.run @@ fun sw ->
     Proffer_httpz.run ~sw ~addr ~on_event ~on_error stdenv ~env
       Sortal_web.compiled
   with Eio.Cancel.Cancelled _ -> ());
  0
