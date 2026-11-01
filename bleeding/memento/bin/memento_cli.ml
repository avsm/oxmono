(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Cmdliner
let text = Fetch.Media.sanitize_diagnostic
let json_string s = Jsont.Json.string s
let object_ fields = Jsont.Json.object' (List.map (fun (n, v) ->
  Jsont.Json.mem (n, Jsont.Meta.none) v) fields)
let emit_json rows =
  match Jsont_bytesrw.encode_string Jsont.json (Jsont.Json.list rows) with
  | Ok s -> print_endline s | Error e -> invalid_arg e
let emit_versions json versions =
  if json then emit_json (List.map (fun (v : Memento_wayback.version) ->
    object_ ["datetime", json_string (Memento.Datetime.to_json v.capture.datetime);
      "uri", json_string v.capture.uri; "original_uri", json_string v.original_uri;
      "status", (match v.status with None -> Jsont.Json.null ()
        | Some n -> Jsont.Json.int n);
      "media_type", json_string v.media_type]) versions)
  else List.iter (fun (v : Memento_wayback.version) ->
    Printf.printf "%s\t%s\t%s\t%s\n"
      (Memento.Datetime.to_json v.capture.datetime)
      (match v.status with Some n -> string_of_int n | None -> "-")
      (text v.media_type) (text v.capture.uri)) versions
let emit_urls json urls =
  if json then emit_json (List.map json_string urls)
  else List.iter (fun uri -> print_endline (text uri)) urls
let run timeout operation emit =
  if not (Float.is_finite timeout) || timeout <= 0. then
    Error "timeout must be a finite positive number"
  else try
    Eio_main.run @@ fun env ->
    let client = Fetch_httpz.std ~cookies:`Off
        ~retry:{ Fetch.Retry.default with max_retries = 0 } env in
    Eio.Time.Timeout.run_exn
      (Eio.Time.Timeout.seconds (Eio.Stdenv.mono_clock env) timeout) @@ fun () ->
    match operation client with
    | Error e -> Error (text e)
    | Ok values -> emit values; Ok ()
  with
  | Eio.Time.Timeout -> Error "Wayback query timed out"
  | Eio.Io _ as e -> Error (text (Printexc.to_string e))
  | Invalid_argument e | Failure e -> Error (text e)
let command timeout operation emit =
  Result.map_error (fun e -> `Msg e) (run timeout operation emit)
let url = Arg.(required & pos 0 (some string) None &
    info [] ~docv:"URL" ~doc:"Absolute HTTP(S) original URL or prefix to query.")
let limit = Arg.(value & opt int 100 & info ["limit"] ~docv:"N"
    ~doc:"Return at most $(docv) results (1 to 10000).")
let json = Arg.(value & flag & info ["json"] ~doc:"Emit a JSON array.")
let timeout = Arg.(value & opt float 30. & info ["timeout"] ~docv:"SECONDS"
    ~doc:"Bound the complete query, including reading its response.")
let bound name doc = Arg.(value & opt (some string) None &
    info [name] ~docv:"TIMESTAMP" ~doc)
let from = bound "from" "Include captures from this CDX timestamp (1 to 14 digits)."
let until = bound "until" "Include captures through this CDX timestamp (1 to 14 digits)."
let latest = Arg.(value & flag & info ["latest"] ~doc:"Return the last results instead of the first.")
let versions =
  let query url limit from until latest json timeout =
    command timeout (fun client -> Memento_wayback.versions
      ?from ?until ~latest ~limit client url) (emit_versions json) in
  Cmd.v (Cmd.info "versions" ~doc:"List archived versions of a URL."
    ~man:[`S Manpage.s_description;
      `P "Queries Wayback CDX. Prints UTC datetime, recorded HTTP status, media type and playback URL. All recorded statuses are included. Results may be limited and indexed captures may be unavailable at playback time."])
    Term.(term_result (const query $ url $ limit $ from $ until $ latest $ json $ timeout))
let urls =
  let query url limit json timeout =
    command timeout (fun client -> Memento_wayback.urls ~limit client url) (emit_urls json) in
  Cmd.v (Cmd.info "urls" ~doc:"List archived URLs under a prefix.")
    Term.(term_result (const query $ url $ limit $ json $ timeout))
let () = exit (Cmd.eval (Cmd.group
    (Cmd.info "memento" ~version:"0.1.0"
      ~doc:"Query URLs and versions in the Wayback archive.") [versions; urls]))
