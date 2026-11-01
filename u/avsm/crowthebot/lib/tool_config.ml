open Cmdliner

type t = { name : string; doc : string; term : (unit -> Jsont.json) Term.t }

let v ~name ~doc term =
  Secret_store.validate_name name;
  { name; doc; term }

let command ~profile ~run t =
  let name = Arg.(required & pos 0 (some string) None & info [] ~docv:"NAME") in
  let command name doc term = Cmd.v (Cmd.info name ~doc) term in
  let write action replace =
    command action
      "Store a complete named configuration. Restart Crow to apply it."
      Term.(
        const (fun profile name settings ->
            run ~profile (fun store ->
                Secret_store.validate_name name;
                Secret_store.put store ~tool:t.name ~name ~replace (settings ());
                print_endline "Configuration saved. Restart Crow to apply it."))
        $ profile $ name $ t.term)
  in
  let named action doc f =
    command action doc
      Term.(
        const (fun profile name ->
            run ~profile (fun store ->
                f store ~tool:t.name ~name;
                print_endline "Configuration updated. Restart Crow to apply it."))
        $ profile $ name)
  in
  Cmd.group
    (Cmd.info t.name ~doc:t.doc)
    [
      write "add" false;
      write "set" true;
      command "list" "List names and the selected default, without values."
        Term.(
          const (fun profile ->
              run ~profile (fun store ->
                  List.iter
                    (fun (name, selected) ->
                      Printf.printf "%s%s\n" name
                        (if selected then " (selected)" else ""))
                    (Secret_store.list store ~tool:t.name)))
          $ profile);
      named "remove" "Remove a named configuration." Secret_store.remove;
      named "select" "Select the default named configuration."
        Secret_store.select;
      command "rename"
        "Rename a configuration. Existing tool links retain the old name."
        Term.(
          const (fun profile name into ->
              run ~profile (fun store ->
                  Secret_store.rename store ~tool:t.name ~name ~into;
                  print_endline
                    "Configuration renamed. Update tool links and restart Crow."))
          $ profile $ name
          $ Arg.(required & pos 1 (some string) None & info [] ~docv:"NEW_NAME"));
    ]

let secret ~label = function
  | Some path -> Profile.read_secret path
  | None ->
      if not (Unix.isatty Unix.stdin) then
        invalid_arg "Use a secret file outside an interactive terminal.";
      let before = Unix.tcgetattr Unix.stdin in
      Fun.protect
        ~finally:(fun () ->
          Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH before;
          prerr_newline ())
        (fun () ->
          Unix.tcsetattr Unix.stdin Unix.TCSAFLUSH
            { before with c_echo = false };
          prerr_string (label ^ ": ");
          flush stderr;
          let value = read_line () in
          if
            value = ""
            || String.length value > 16384
            || String.contains value '\r' || String.contains value '\n'
          then
            invalid_arg
              "Secret must be a nonempty single line, at most 16384 bytes.";
          value)

let endpoint ~allow_http value =
  let uri = Uri.of_string value in
  if
    String.length value > 2048
    || String.exists (fun c -> Char.code c <= 32 || Char.code c = 127) value
    || (not
          (Uri.scheme uri = Some "https"
          || (allow_http && Uri.scheme uri = Some "http")))
    || Option.fold ~none:true ~some:(( = ) "") (Uri.host uri)
    || Uri.userinfo uri <> None
    || Uri.query uri <> []
    || Uri.fragment uri <> None
  then
    invalid_arg
      "Endpoint must be HTTPS with a host and no credentials, query or \
       fragment. Use --allow-http for a trusted HTTP endpoint.";
  Uri.to_string uri

let encode codec value =
  match Jsont_bytesrw.encode_string codec value with
  | Error _ -> invalid_arg "Cannot encode tool configuration."
  | Ok json -> Result.get_ok (Jsont_bytesrw.decode_string Jsont.json json)

let decode codec value =
  match Jsont_bytesrw.encode_string Jsont.json value with
  | Error _ -> invalid_arg "Invalid tool configuration."
  | Ok json -> (
      match Jsont_bytesrw.decode_string codec json with
      | Ok value -> value
      | Error _ -> invalid_arg "Invalid tool configuration.")
