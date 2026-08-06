(* fetch: a small transfer tool over Fetch_main, which picks the best
   backend for the platform (NSURLSession on macOS, libcurl elsewhere;
   override with --backend or $FETCH_BACKEND). *)

let usage = "fetch [options] URL\n\nOptions:"

let () =
  let meth = ref None in
  let headers = ref [] in
  let data = ref None in
  let output = ref None in
  let user_agent = ref None in
  let backend = ref None in
  let include_headers = ref false in
  let redirects = ref None in
  let url = ref None in
  let spec =
    Arg.align
      [ ("-X", Arg.String (fun s -> meth := Some s),
         "METHOD Request method (default GET, or POST with -d)");
        ("-H", Arg.String (fun s -> headers := s :: !headers),
         "'Name: value' Add a request header (repeatable)");
        ("-d", Arg.String (fun s -> data := Some s),
         "DATA Request body; @file reads it from a file, - from stdin");
        ("-o", Arg.String (fun s -> output := Some s),
         "FILE Write the body to FILE instead of stdout");
        ("-A", Arg.String (fun s -> user_agent := Some s),
         "AGENT User-Agent to send");
        ("-i", Arg.Set include_headers,
         " Include the status line and response headers in the output");
        ("--backend", Arg.String (fun s -> backend := Some s),
         "curl|macos Force a backend (also $FETCH_BACKEND)");
        ("--max-redirects", Arg.Int (fun n -> redirects := Some n),
         "N Redirect budget");
      ]
  in
  Arg.parse spec
    (fun a ->
       match !url with
       | None -> url := Some a
       | Some _ -> raise (Arg.Bad (Printf.sprintf "unexpected argument %S" a)))
    usage;
  let die fmt = Fmt.kstr (fun s -> Fmt.epr "fetch: %s@." s; exit 1) fmt in
  let url = match !url with Some u -> u | None -> die "no URL given" in
  let backend =
    Option.map
      (fun s ->
         match Fetch_main.of_string s with
         | Some b -> b
         | None -> die "unknown backend %S (expected \"curl\" or \"macos\")" s)
      !backend
  in
  let headers =
    List.fold_left
      (fun acc line ->
         match String.index_opt line ':' with
         | None -> die "malformed header %S (expected \"Name: value\")" line
         | Some i ->
           let cell =
             Fetch.Header.raw
               (String.trim (String.sub line 0 i))
               (String.trim
                  (String.sub line (i + 1) (String.length line - i - 1)))
           in
           Fetch.Header.(cell :: acc))
      Fetch.Header.[] !headers
    (* !headers is reversed already, so the fold restores command-line
       order. *)
  in
  let meth =
    match !meth, !data with
    | Some m, _ -> Http.Method.of_string (String.uppercase_ascii m)
    | None, Some _ -> `POST
    | None, None -> `GET
  in
  try
    Eio_main.run @@ fun env ->
    Eio.Switch.run @@ fun sw ->
    let body =
      match !data with
      | None -> None
      | Some "-" -> Some (Fetch.String (Eio.Flow.read_all (Eio.Stdenv.stdin env)))
      | Some s when String.length s > 0 && s.[0] = '@' ->
        let path = String.sub s 1 (String.length s - 1) in
        Some (Fetch.String
                Eio.Path.(load (Eio.Stdenv.fs env / path)))
      | Some s -> Some (Fetch.String s)
    in
    (* Mint only the authority the invocation needs: a safe method runs
       over the read-only client. *)
    let resp =
      match meth with
      | `GET | `HEAD | `OPTIONS ->
        let t = Fetch_main.std_ro ~sw ?backend ?user_agent:!user_agent env in
        Fetch.fetch ~sw ~headers ?body ?redirects:!redirects t meth url
      | _ ->
        let t = Fetch_main.std ~sw ?backend ?user_agent:!user_agent env in
        Fetch.fetch ~sw ~headers ?body ?redirects:!redirects t meth url
    in
    let write_to sink =
      if !include_headers then begin
        let hdrs =
          Http.Header.to_list (Fetch.headers resp)
          |> List.map (fun (k, v) -> Fmt.str "%s: %s\r\n" k v)
          |> String.concat ""
        in
        Eio.Flow.copy_string
          (Fmt.str "HTTP %d\r\n%s\r\n" (Fetch.status resp) hdrs) sink
      end;
      Eio.Flow.copy (Fetch.body resp) sink
    in
    (match !output with
     | None -> write_to (Eio.Stdenv.stdout env)
     | Some file ->
       Eio.Path.(with_open_out ~create:(`Or_truncate 0o644)
                   (Eio.Stdenv.fs env / file))
         write_to);
    (* A response with an error status is data, not a failure; still,
       a tool in a pipeline should not exit 0 on a 404. *)
    if Fetch.status resp >= 400 then exit 22
  with
  | Eio.Io _ as ex -> die "%a" Eio.Exn.pp ex
  | Invalid_argument msg -> die "%s" msg
