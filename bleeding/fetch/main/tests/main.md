# Fetch_main: runtime backend selection

These tests run on macOS, where both backends are functional, so the
selection logic can be exercised in both directions.

```ocaml
# #require "fetch-main";;
# #require "eio_main";;
# #require "unix";;
```

A minimal HTTP/1.1 server: prints each request line it receives and
reflects a couple of things back.

```ocaml
open Eio.Std
open Fetch

let handle_client flow _addr =
  let buf = Eio.Buf_read.of_flow flow ~max_size:65536 in
  let request_line = Eio.Buf_read.line buf in
  let rec headers acc =
    match Eio.Buf_read.line buf with
    | "" -> List.rev acc
    | l -> headers (l :: acc)
  in
  let req_headers = headers [] in
  let header name =
    List.find_map (fun l ->
        match String.index_opt l ':' with
        | Some i when String.lowercase_ascii (String.sub l 0 i) = name ->
          Some (String.trim (String.sub l (i + 1) (String.length l - i - 1)))
        | _ -> None)
      req_headers
  in
  Fmt.pr "> %s@." request_line;
  let respond body =
    Eio.Flow.copy_string
      (Fmt.str "HTTP/1.1 200 OK\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
         (String.length body) body)
      flow
  in
  match String.split_on_char ' ' request_line with
  | [ _; "/hello"; _ ] -> respond "hello from eio"
  | [ _; "/agent"; _ ] ->
    respond (Option.value (header "user-agent") ~default:"none")
  | [ _; "/flag"; _ ] ->
    respond (Option.value (header "x-flag") ~default:"absent")
  | _ -> respond "nope"

let with_server_env fn =
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let sock = Eio.Net.listen ~sw ~backlog:5 ~reuse_addr:true net
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0)) in
  let port =
    match Eio.Net.listening_addr sock with
    | `Tcp (_, port) -> port
    | _ -> assert false
  in
  Fiber.fork_daemon ~sw (fun () ->
      Eio.Net.run_server sock handle_client ~on_error:(fun _ -> ()));
  fn env sw (fun path -> Fmt.str "http://127.0.0.1:%d%s" port path)
```

## Selection

On macOS the platform default is the system networking stack:

```ocaml
# Fetch_main.select ();;
- : Fetch_main.backend = `Macos
```

`FETCH_BACKEND` overrides the default; an explicit argument beats both;
and a typo is an error rather than a silent fallback:

```ocaml
# Unix.putenv Fetch_main.env_var "curl";
  Fetch_main.select ();;
- : Fetch_main.backend = `Curl
# Fetch_main.select ~backend:`Macos ();;
- : Fetch_main.backend = `Macos
# Unix.putenv Fetch_main.env_var "gopher";
  (try ignore (Fetch_main.select ()); "selected!"
   with Invalid_argument msg -> msg);;
- : string =
"Fetch_main: FETCH_BACKEND=\"gopher\": unknown backend (expected \"curl\" or \"macos\")"
# Unix.putenv Fetch_main.env_var "";;
- : unit = ()
```

## The same call works over either backend

```ocaml
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  Fetch.read (Fetch_main.std ~sw env) (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  Fetch.read (Fetch_main.std ~sw ~backend:`Curl env) (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

## Read-only and read-write variants

`std` is read-write; `std_ro` is the same stack behind
`Fetch.read_only`, so it serves the safe methods:

```ocaml
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  Fetch.read (Fetch_main.std_ro ~sw env) (url "/hello");;
> GET /hello HTTP/1.1
- : string = "hello from eio"
```

A mutating method is denied before the network is touched, whichever
entry point it arrives through (the server prints nothing):

```ocaml
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  let t = Fetch_main.std_ro ~sw env in
  (try ignore (Fetch.fetch ~sw ~body:(String "x") t `POST (url "/hello")); "sent!"
   with Eio.Io (E (Denied _), _) -> "denied");;
- : string = "denied"
```

## Common conveniences

`user_agent` names the application on every request, whichever backend
answers, and a request's own header still wins:

```ocaml
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  let t = Fetch_main.std ~sw ~user_agent:"taposaur/1.0" env in
  let default = Fetch.read t (url "/agent") in
  Switch.run @@ fun rsw ->
  let own =
    Fetch.get ~sw:rsw t (url "/agent")
      ~headers:Header.[ user_agent, "special/2.0" ]
  in
  (default, Eio.Flow.read_all (body own));;
> GET /agent HTTP/1.1
> GET /agent HTTP/1.1
- : string * string = ("taposaur/1.0", "special/2.0")
```

`headers` sets default headers the same way, `If_absent`:

```ocaml
# with_server_env @@ fun env _sw url ->
  Switch.run @@ fun sw ->
  let t = Fetch_main.std ~sw ~headers:Header.[ raw "X-Flag" "default" ] env in
  Fetch.read t (url "/flag");;
> GET /flag HTTP/1.1
- : string = "default"
```
