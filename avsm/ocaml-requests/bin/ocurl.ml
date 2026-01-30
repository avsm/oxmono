(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

open Eio
open Cmdliner

(* Command-line options *)
let http_method =
  let methods = [
    ("GET", `GET);
    ("POST", `POST);
    ("PUT", `PUT);
    ("DELETE", `DELETE);
    ("HEAD", `HEAD);
    ("OPTIONS", `OPTIONS);
    ("PATCH", `PATCH);
  ] in
  let doc = "HTTP method to use" in
  let env_info = Cmdliner.Cmd.Env.info "OCURL_METHOD" in
  Arg.(value & opt (enum methods) `GET & info ["X"; "request"] ~env:env_info ~docv:"METHOD" ~doc)

let urls =
  let doc = "URL(s) to fetch" in
  Arg.(non_empty & pos_all string [] & info [] ~docv:"URL" ~doc)

let headers =
  let doc = "Add custom HTTP header (can be used multiple times)" in
  Arg.(value & opt_all string [] & info ["H"; "header"] ~docv:"HEADER" ~doc)

let data =
  let doc = "HTTP POST/PUT data" in
  Arg.(value & opt (some string) None & info ["d"; "data"] ~docv:"DATA" ~doc)

let json_data =
  let doc = "HTTP POST/PUT JSON data" in
  Arg.(value & opt (some string) None & info ["json"] ~docv:"JSON" ~doc)

let output_file =
  let doc = "Write output to file instead of stdout" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~docv:"FILE" ~doc)

let include_headers =
  let doc = "Include response headers in output" in
  Arg.(value & flag & info ["i"; "include"] ~doc)

let head =
  let doc = "Show only response headers (no body)" in
  Arg.(value & flag & info ["I"; "head"] ~doc)

let auth =
  let doc = "Basic authentication in USER:PASSWORD format" in
  Arg.(value & opt (some string) None & info ["u"; "user"] ~docv:"USER:PASS" ~doc)

let allow_insecure_auth =
  let doc = "Allow basic authentication over HTTP (insecure, for testing only)" in
  Arg.(value & flag & info ["allow-insecure-auth"] ~doc)

let show_progress =
  let doc = "Show progress bar for downloads" in
  Arg.(value & flag & info ["progress-bar"] ~doc)

(* Logging setup *)
(* Setup logging using Logs_cli for standard logging options *)
let setup_log app_name =
  let setup style_renderer level verbose_http_ws =
    Fmt_tty.setup_std_outputs ?style_renderer ();
    Logs.set_level level;
    Logs.set_reporter (Logs_fmt.reporter ());
    (* Extract value from with_source wrapper *)
    Requests.Cmd.setup_log_sources ~verbose_http:verbose_http_ws.Requests.Cmd.value level
  in
  Term.(const setup $ Fmt_cli.style_renderer () $ Logs_cli.level () $
        Requests.Cmd.verbose_http_term app_name)

(* Parse authentication *)
let parse_auth auth_str =
  match String.split_on_char ':' auth_str with
  | [user; pass] -> Some (user, pass)
  | _ -> None

(* Parse headers *)
let parse_header header_str =
  match String.split_on_char ':' header_str with
  | [] -> None
  | [name] -> Some (String.trim name, "")
  | name :: rest ->
      Some (String.trim name, String.trim (String.concat ":" rest))

(* Pretty print response *)
let pp_response ppf response =
  let status = Requests.Response.status response in
  let status_code = Requests.Response.status_code response in
  let headers = Requests.Response.headers response in

  (* Color code status *)
  let status_style =
    if Requests.Status.is_success status then Fmt.(styled `Green)
    else if Requests.Status.is_client_error status then Fmt.(styled `Yellow)
    else if Requests.Status.is_server_error status then Fmt.(styled `Red)
    else Fmt.(styled `Blue)
  in

  (* Print status line *)
  Fmt.pf ppf "@[<v>HTTP/1.1 %d %a@]@."
    status_code
    (status_style Fmt.string) (Requests.Status.reason_phrase status);

  (* Print headers *)
  let header_list = Requests.Headers.to_list headers in
  List.iter (fun (k, v) ->
    Fmt.pf ppf "@[<h>%a: %s@]@."
      Fmt.(styled `Cyan string) k v
  ) header_list;

  Fmt.pf ppf "@."

(* Normalize URL to ensure it has a scheme, defaulting to http:// *)
let normalize_url url_str =
  let uri = Uri.of_string url_str in
  match Uri.scheme uri with
  | Some _ -> url_str  (* Already has a scheme *)
  | None ->
      (* No scheme - prepend http:// *)
      "http://" ^ url_str

(* Process a single URL and return result *)
let process_url env req method_ headers body include_headers head output url_str =
  let url_str = normalize_url url_str in
  let quiet = match Logs.level () with Some (Logs.Error | Logs.Warning) -> true | _ -> false in
  let uri = Uri.of_string url_str in

  if not quiet then begin
    let method_str = Requests.Method.to_string (method_ :> Requests.Method.t) in
    Fmt.pr "@[<v>%a %a@]@."
      Fmt.(styled `Bold string) method_str
      Fmt.(styled `Underline Uri.pp) uri;
  end;
  try
    (* Make request *)
    let response =
      match method_ with
      | `GET -> Requests.get req ~headers url_str
      | `POST -> Requests.post req ~headers ?body url_str
      | `PUT -> Requests.put req ~headers ?body url_str
      | `DELETE -> Requests.delete req ~headers url_str
      | `HEAD -> Requests.head req ~headers url_str
      | `OPTIONS -> Requests.options req ~headers url_str
      | `PATCH -> Requests.patch req ~headers ?body url_str
    in

    (* Print response headers if requested *)
    if (include_headers || head) && not quiet then
      pp_response Fmt.stdout response;

    (* If head flag is set, skip body processing *)
    if head then
      Ok (url_str, response)
    else begin
      (* Handle output *)
      let body_flow = Requests.Response.body response in

      begin match output with
    | Some file -> begin
        let filename =
          if List.length [url_str] > 1 then begin
            let base = Filename.remove_extension file in
            let ext = Filename.extension file in
            let url_hash =
              let full_hash = Digest.string url_str |> Digest.to_hex in
              String.sub full_hash (String.length full_hash - 8) 8 in
            Printf.sprintf "%s-%s%s" base url_hash ext
          end else file
        in
        let () =
          Eio.Path.with_open_out ~create:(`Or_truncate 0o644)
            Eio.Path.(env#fs / filename) @@ fun sink ->
            Eio.Flow.copy body_flow sink in
        let () = if not quiet then
            Fmt.pr "[%s] Saved to %s@." url_str filename else () in
        Ok (url_str, response)
    end
    | None ->
        (* Write to stdout *)
        let buf = Buffer.create 1024 in
        Eio.Flow.copy body_flow (Eio.Flow.buffer_sink buf);
        let body_str = Buffer.contents buf in

        (* Pretty-print JSON if applicable *)
        if String.length body_str > 0 &&
           (body_str.[0] = '{' || body_str.[0] = '[') then
          try
            match Jsont_bytesrw.decode_string' Jsont.json body_str with
            | Ok json ->
                (match Jsont_bytesrw.encode_string' ~format:Jsont.Indent Jsont.json json with
                | Ok pretty ->
                    if not quiet then Fmt.pr "[%s]:@." url_str;
                    print_string pretty
                | Error _ ->
                    if not quiet then Fmt.pr "[%s]:@." url_str;
                    print_string body_str)
            | Error _ ->
                if not quiet then Fmt.pr "[%s]:@." url_str;
                print_string body_str
          with _ ->
            if not quiet then Fmt.pr "[%s]:@." url_str;
            print_string body_str
        else begin
          if not quiet then Fmt.pr "[%s]:@." url_str;
          print_string body_str
        end;

        if not quiet && Requests.Response.ok response then
          Logs.app (fun m -> m "✓ Success for %s" url_str);

        Ok (url_str, response)
      end
    end
  with
  | exn ->
      Logs.err (fun m -> m "Request failed for %s: %a" url_str Eio.Exn.pp exn);
      Error (url_str, exn)

(* Main function using Requests with concurrent fetching *)
let run_request env sw persist_cookies verify_tls timeout follow_redirects max_redirects
    method_ urls headers data json_data output include_headers head
    auth allow_insecure_auth _show_progress () =

  (* Log levels are already set by setup_log via Logs_cli *)

  (* Create XDG paths *)
  let xdg = Xdge.create env#fs "ocurl" in

  (* Create requests instance with configuration *)
  let timeout_obj = Option.map (fun t -> Requests.Timeout.create ~total:t ()) timeout in
  let req = Requests.create ~sw ~xdg ~persist_cookies ~verify_tls
    ~follow_redirects ~max_redirects ~allow_insecure_auth ?timeout:timeout_obj env in

  (* Set authentication if provided *)
  let req = match Option.bind auth parse_auth with
    | Some (user, pass) ->
        Requests.set_auth req (Requests.Auth.basic ~username:user ~password:pass)
    | None ->
        (if Option.is_some auth then
           Logs.warn (fun m -> m "Invalid auth format, ignoring"));
        req
  in

  (* Build headers from command line *)
  let cmd_headers = List.fold_left (fun hdrs header_str ->
    match parse_header header_str with
    | Some (k, v) -> Requests.Headers.add_string k v hdrs
    | None -> hdrs
  ) Requests.Headers.empty headers in

  (* Prepare body based on data/json options *)
  let body = match json_data, data with
    | Some json_str, _ ->
        (* Use of_string with JSON mime type for raw JSON string *)
        Some (Requests.Body.of_string Requests.Mime.json json_str)
    | None, Some d -> Some (Requests.Body.text d)
    | None, None -> None
  in

  (* Process URLs concurrently or sequentially based on count *)
  match urls with
  | [] -> ()
  | [single_url] ->
      (* Single URL - process directly *)
      let _ = process_url env req method_ cmd_headers body include_headers head output single_url in
      ()
  | multiple_urls ->
      (* Multiple URLs - process concurrently *)
      let verbose = Logs.level () = Some Logs.Debug || Logs.level () = Some Logs.Info in
      if verbose then
        Fmt.pr "@[<v>Processing %d URLs concurrently...@]@." (List.length multiple_urls);

      (* Create promises for each URL *)
      let results =
        List.map (fun url_str ->
          let promise, resolver = Eio.Promise.create () in
          (* Fork a fiber for each URL *)
          Fiber.fork ~sw (fun () ->
            let result = process_url env req method_ cmd_headers body include_headers head output url_str in
            Eio.Promise.resolve resolver result
          );
          promise
        ) multiple_urls
      in

      (* Wait for all promises to complete *)
      let completed_results = List.map Eio.Promise.await results in

      (* Report summary *)
      let quiet = match Logs.level () with Some (Logs.Error | Logs.Warning) -> true | _ -> false in
      if not quiet then begin
        let successes = List.filter Result.is_ok completed_results |> List.length in
        let failures = List.filter Result.is_error completed_results |> List.length in
        Fmt.pr "@[<v>@.Summary: %d successful, %d failed out of %d total@]@."
          successes failures (List.length completed_results);

        (* Print failed URLs *)
        if failures > 0 then begin
          Fmt.pr "@[<v>Failed URLs:@]@.";
          List.iter (function
            | Error (url, _) -> Fmt.pr "  - %s@." url
            | Ok _ -> ()
          ) completed_results
        end
      end

(* Main entry point *)
let main method_ urls headers data json_data output include_headers head
    auth allow_insecure_auth show_progress persist_cookies_ws verify_tls_ws
    timeout_ws follow_redirects_ws max_redirects_ws () =

  (* Extract values from with_source wrappers *)
  let persist_cookies = persist_cookies_ws.Requests.Cmd.value in
  let verify_tls = verify_tls_ws.Requests.Cmd.value in
  let timeout = timeout_ws.Requests.Cmd.value in
  let follow_redirects = follow_redirects_ws.Requests.Cmd.value in
  let max_redirects = max_redirects_ws.Requests.Cmd.value in

  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_unix.use_default ();
  Switch.run @@ fun sw ->

  run_request env sw persist_cookies verify_tls timeout follow_redirects max_redirects
    method_ urls headers data json_data output include_headers head auth
    allow_insecure_auth show_progress ()

(* Command-line interface *)
let cmd =
  let doc = "OCaml HTTP client with concurrent fetching using the Requests library" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a command-line HTTP client written in OCaml that uses the \
        Requests library with stateful request management. It supports various HTTP methods, \
        custom headers, authentication, cookies, and JSON data. When multiple URLs are provided, \
        they are fetched concurrently using Eio fibers for maximum performance.";
    `S Manpage.s_examples;
    `P "Fetch a URL:";
    `Pre "  $(tname) https://api.github.com";
    `P "Fetch multiple URLs concurrently:";
    `Pre "  $(tname) https://api.github.com https://httpbin.org/get https://example.com";
    `P "Show only response headers (like HEAD request):";
    `Pre "  $(tname) -I https://api.github.com";
    `P "Include response headers with body:";
    `Pre "  $(tname) -i https://api.github.com";
    `P "POST JSON data:";
    `Pre "  $(tname) -X POST --json '{\"key\":\"value\"}' https://httpbin.org/post";
    `P "Download file:";
    `Pre "  $(tname) -o file.zip https://example.com/file.zip";
    `P "Download multiple files concurrently:";
    `Pre "  $(tname) -o output.json https://api1.example.com https://api2.example.com https://api3.example.com";
    `P "Basic authentication:";
    `Pre "  $(tname) -u user:pass https://httpbin.org/basic-auth/user/pass";
    `P "Custom headers:";
    `Pre "  $(tname) -H 'Accept: application/json' -H 'X-Api-Key: secret' https://api.example.com";
    `P "With persistent cookies:";
    `Pre "  $(tname) --persist-cookies https://example.com";
    `P "Disable TLS verification (insecure):";
    `Pre "  $(tname) --no-verify-tls https://self-signed.example.com";
    `S "LOGGING OPTIONS";
    `P "Control logging verbosity using standard options:";
    `P "Enable verbose logging (can be repeated):";
    `Pre "  $(tname) -v https://api.github.com          # info level";
    `Pre "  $(tname) -vv https://api.github.com         # debug level (application-level)";
    `P "Enable HTTP protocol-level verbose logging:";
    `Pre "  $(tname) -vv --verbose-http https://api.github.com  # includes TLS/TCP details";
    `P "Suppress output:";
    `Pre "  $(tname) -q https://api.github.com          # warnings and errors only";
    `P "Set specific log level:";
    `Pre "  $(tname) --verbosity=info https://api.github.com";
    `Pre "  $(tname) --verbosity=debug https://api.github.com";
    `Pre "  $(tname) --verbosity=error https://api.github.com";
    `P "Available verbosity levels: quiet, error, warning, info, debug";
    `P "The logging system provides detailed information about:";
    `P "- HTTP requests and responses (use -v or -vv for application-level logs)";
    `P "- Authentication and cookie handling";
    `P "- Retry attempts and backoff calculations";
    `P "- TLS/TCP connection details (use --verbose-http with -vv for protocol-level logs)";
  ] in

  (* Build the term with Requests configuration options *)
  let app_name = "ocurl" in
  let combined_term =
    Term.(const main $ http_method $ urls $ headers $ data $ json_data $
          output_file $ include_headers $ head $ auth $
          allow_insecure_auth $ show_progress $
          Requests.Cmd.persist_cookies_term app_name $
          Requests.Cmd.verify_tls_term app_name $
          Requests.Cmd.timeout_term app_name $
          Requests.Cmd.follow_redirects_term app_name $
          Requests.Cmd.max_redirects_term app_name $
          setup_log app_name)
  in

  let info = Cmd.info "ocurl" ~version:"2.0.0" ~doc ~man in
  Cmd.v info combined_term

let () = exit (Cmd.eval cmd)
