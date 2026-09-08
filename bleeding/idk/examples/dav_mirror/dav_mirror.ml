(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Mirrors an address book or a calendar into a directory of files.

   idk-dav-mirror (contacts|calendar) URL DIR [-u USER] [-p PASSWORD-FILE]
     [-c COLLECTION] [--insecure]

   URL is the server root or its well-known path. Without -c the first
   address book or calendar of the principal is mirrored. Each action is
   printed as it happens, and the sync token and entity tags are kept in
   DIR/.davsync so that the next run fetches only what changed. *)

let usage =
  "idk-dav-mirror (contacts|calendar) URL DIR [-u USER] [-p PASSWORD-FILE] [-c \
   COLLECTION] [--insecure]"

let () =
  let user = ref "" and password_file = ref "" and collection = ref "" in
  let insecure = ref false and positional = ref [] in
  Arg.parse
    [
      ("-u", Arg.Set_string user, "USER login name");
      ("-p", Arg.Set_string password_file, "FILE file holding the password");
      ("-c", Arg.Set_string collection, "URL the collection to mirror");
      ("--insecure", Arg.Set insecure, " allow the credential over http");
    ]
    (fun a -> positional := a :: !positional)
    usage;
  let kind, url, dir =
    match List.rev !positional with
    | [ k; u; d ] when k = "contacts" || k = "calendar" -> (k, u, d)
    | _ ->
        prerr_endline usage;
        exit 2
  in
  let password () =
    if !password_file = "" then ""
    else
      String.trim (In_channel.with_open_bin !password_file In_channel.input_all)
  in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let dir = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let log a = Format.printf "%a\n%!" Idk_eio.Dav.Mirror.pp_action a in
  let dav, collection =
    if kind = "contacts" then
      let credentials =
        if !user = "" then []
        else [ Fetch.Credential.basic ~user:!user ~password:(password ()) ]
      in
      let client =
        match
          Idk_eio.Carddav.Client.connect ~sw ~credentials
            ~allow_insecure:!insecure (Fetch_httpz.std env) url
        with
        | Ok c -> c
        | Error e ->
            Printf.eprintf "error: %s\n"
              (Idk_eio.Carddav.Client.error_to_string e);
            exit 1
      in
      let collection =
        if !collection <> "" then !collection
        else
          match Idk_eio.Carddav.Client.addressbooks client with
          | Ok (b :: _) -> b.href
          | Ok [] ->
              prerr_endline "error: no address book";
              exit 1
          | Error e ->
              Printf.eprintf "error: %s\n"
                (Idk_eio.Carddav.Client.error_to_string e);
              exit 1
      in
      (Idk_eio.Carddav.Client.dav client, collection)
    else
      let credentials =
        if !user = "" then []
        else [ Fetch.Credential.basic ~user:!user ~password:(password ()) ]
      in
      let client =
        match
          Idk_eio.Caldav.Client.connect ~sw ~credentials
            ~allow_insecure:!insecure (Fetch_httpz.std env) url
        with
        | Ok c -> c
        | Error e ->
            Printf.eprintf "error: %s\n"
              (Idk_eio.Caldav.Client.error_to_string e);
            exit 1
      in
      let collection =
        if !collection <> "" then !collection
        else
          match Idk_eio.Caldav.Client.calendars client with
          | Ok (c :: _) -> c.href
          | Ok [] ->
              prerr_endline "error: no calendar";
              exit 1
          | Error e ->
              Printf.eprintf "error: %s\n"
                (Idk_eio.Caldav.Client.error_to_string e);
              exit 1
      in
      (Idk_eio.Caldav.Client.dav client, collection)
  in
  Printf.printf "mirroring %s\n%!" collection;
  match Idk_eio.Dav.Mirror.run ~log dav ~collection ~dir with
  | s -> Printf.printf "done: %d fetched, %d removed\n" s.fetched s.removed
  | exception Idk_eio.Dav.Http_error e ->
      Printf.eprintf "error: HTTP %d %s\n" e.status e.body;
      exit 1
  | exception Idk_eio.Dav.Protocol_error m ->
      Printf.eprintf "error: %s\n" m;
      exit 1
