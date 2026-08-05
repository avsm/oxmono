(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** httpz-eio-server — an Eio static file server built on [httpz].

    All of the HTTP work lives in the libraries: {!Httpz_eio_server.Static}
    provides the file-serving route (MIME types, ETags, conditional GET, range
    requests, directory indexes) and {!Httpz_eio_server.handle_client} drives
    the connection. This module is only the command line front end. *)

open Cmdliner

(* {1 Address parsing} *)

(* Eio's [Ipaddr.of_raw] takes the raw octets, so we parse a dotted quad
   ourselves rather than depending on [eio.unix] or [ipaddr]. IPv6 literals are
   not accepted. *)
let ipaddr_of_string s =
  match s with
  | "localhost" -> Ok Eio.Net.Ipaddr.V4.loopback
  | "any" | "*" -> Ok Eio.Net.Ipaddr.V4.any
  | s ->
    (match String.split_on_char '.' s with
     | [ _; _; _; _ ] as parts ->
       let octet p =
         match int_of_string_opt p with
         | Some n when n >= 0 && n <= 255 -> Some (Char.chr n)
         | _ -> None
       in
       let octets = List.map octet parts in
       if List.for_all Option.is_some octets then
         Ok (Eio.Net.Ipaddr.of_raw
               (String.init 4 (fun i -> Option.get (List.nth octets i))))
       else Error (Printf.sprintf "invalid IPv4 address: %S" s)
     | _ ->
       Error
         (Printf.sprintf
            "invalid address: %S (expected an IPv4 address, \"localhost\" or \"any\")"
            s))

let ipaddr_conv =
  Arg.Conv.make ~docv:"ADDRESS" ~parser:ipaddr_of_string
    ~pp:(fun ppf ip -> Eio.Net.Ipaddr.pp ppf ip)
    ()

(* {1 Request logging} *)

(* [request_info] is passed [@ local], so its string fields cannot be handed to
   [Printf]. Emit them character by character instead of copying them to the
   heap — logging then allocates nothing. *)
let print_local (s : string @ local) =
  for i = 0 to String.length s - 1 do
    print_char (String.unsafe_get s i)
  done

let log_request quiet (info @ local) =
  if not quiet then begin
    print_local info.Httpz_eio_server.remote_addr;
    print_char ' ';
    print_string (Httpz.Method.to_string info.Httpz_eio_server.meth);
    print_char ' ';
    print_local info.Httpz_eio_server.target;
    print_string " -> ";
    print_string (Httpz.Res.status_to_string info.Httpz_eio_server.status);
    Printf.printf " (%d bytes, %dus)\n%!" info.Httpz_eio_server.response_body_size
      info.Httpz_eio_server.duration_us
  end

(* {1 Server} *)

let serve ~dir ~address ~port ~backlog ~max_connections ~index ~quiet =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  (* [Stdenv.fs] only names [dir]; [Static.create] confines it. *)
  let root = Eio.Path.(Eio.Stdenv.fs env / dir) in
  let static = Httpz_eio_server.Static.create ~sw ?index root in
  let routes = Httpz_eio_server.Static.routes static in
  let listen_addr = `Tcp (address, port) in
  let socket =
    Eio.Net.listen ~reuse_addr:true ~backlog ~sw (Eio.Stdenv.net env) listen_addr
  in
  if not quiet then begin
    Printf.printf "httpz serving %s on http://%s:%d/\n" dir
      (Format.asprintf "%a" Eio.Net.Ipaddr.pp address)
      port;
    print_string "  Supports: Range requests, ETag, If-None-Match, HEAD\n";
    flush stdout
  end;
  let on_error exn =
    match exn with
    | Eio.Cancel.Cancelled _ -> ()
    | exn -> Printf.eprintf "error: %s\n%!" (Printexc.to_string exn)
  in
  Eio.Net.run_server ~max_connections ~on_error socket (fun flow addr ->
    Httpz_eio_server.handle_client ~routes ~on_request:(log_request quiet) ~on_error
      flow addr)

(* {1 Command line} *)

let dir =
  let doc = "Directory to serve files from." in
  Arg.(value & opt dir "." & info [ "d"; "directory" ] ~docv:"DIR" ~doc)

let port =
  let doc = "TCP port to listen on. Use $(b,0) to pick a free port." in
  Arg.(value & opt int 8080 & info [ "p"; "port" ] ~docv:"PORT" ~doc)

let address =
  let doc =
    "IPv4 address to bind to. Also accepts $(b,localhost) and $(b,any) (all \
     interfaces)."
  in
  Arg.(
    value
    & opt ipaddr_conv Eio.Net.Ipaddr.V4.loopback
    & info [ "a"; "address" ] ~docv:"ADDRESS" ~doc)

let backlog =
  let doc = "Size of the pending connection queue (see $(b,listen)(2))." in
  Arg.(value & opt int 128 & info [ "backlog" ] ~docv:"N" ~doc)

let max_connections =
  let doc = "Maximum number of concurrent client connections." in
  Arg.(value & opt int 10000 & info [ "max-connections" ] ~docv:"N" ~doc)

let index =
  let doc =
    "Directory index file to look for when a request resolves to a directory. \
     Repeatable; the files are tried in order. Pass $(b,--no-index) to disable."
  in
  Arg.(value & opt_all string [] & info [ "i"; "index" ] ~docv:"FILE" ~doc)

let no_index =
  let doc = "Do not serve a directory index; directories return 404." in
  Arg.(value & flag & info [ "no-index" ] ~doc)

let quiet =
  let doc = "Do not log requests to standard output." in
  Arg.(value & flag & info [ "q"; "quiet" ] ~doc)

let term =
  let run dir port address backlog max_connections index no_index quiet =
    let index =
      if no_index then Some [] else match index with [] -> None | l -> Some l
    in
    serve ~dir ~address ~port ~backlog ~max_connections ~index ~quiet
  in
  Term.(
    const run
    $ dir
    $ port
    $ address
    $ backlog
    $ max_connections
    $ index
    $ no_index
    $ quiet)

let cmd =
  let doc = "Serve a directory over HTTP/1.1 using httpz and Eio" in
  let man =
    [ `S Manpage.s_description
    ; `P
        "$(tname) serves static files from a directory over HTTP/1.1. It is \
         built on the $(b,httpz) zero-allocation parser, the $(b,httpz.route) \
         router and the $(b,httpz.eio_server) Eio connection handler."
    ; `P
        "Supported features: MIME type detection from the file extension, weak \
         $(b,ETag)s derived from mtime and size, $(b,If-None-Match) conditional \
         GET (304 Not Modified), single-range $(b,Range) requests (206 Partial \
         Content and 416 Range Not Satisfiable), $(b,HEAD), directory index \
         resolution and keep-alive."
    ; `S Manpage.s_examples
    ; `Pre "  \\$ httpz-eio-server -d ./site -p 8080"
    ; `Pre "  \\$ httpz-eio-server -d /srv/www -a any -p 80 --quiet"
    ; `S Manpage.s_bugs
    ; `P "Report issues at https://github.com/avsm/httpz/issues"
    ]
  in
  Cmd.v (Cmd.info "httpz-eio-server" ~version:"0.1" ~doc ~man) term

let () = exit (Cmd.eval cmd)
