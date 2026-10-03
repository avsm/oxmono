let fixture p = In_channel.with_open_bin ("fixtures/" ^ p) In_channel.input_all

let contains ~sub s =
  let n = String.length sub in
  let rec go i =
    i + n <= String.length s && (String.sub s i n = sub || go (i + 1))
  in
  go 0

(* The recorded response for a request target, or [] for a second page. *)
let route _ target =
  let path, query =
    match String.index_opt target '?' with
    | Some i ->
        (String.sub target 0 i,
         String.sub target (i + 1) (String.length target - i - 1))
    | None -> (target, "")
  in
  if contains ~sub:"page=2" query then (200, "", "[]")
  else
    match path with
    | "/registries" -> (200, "", fixture "registries.json")
    | "/registries/crates.io/packages/serde" ->
        (200, "", fixture "package.json")
    | "/registries/crates.io/packages/serde/versions" ->
        (200, "", fixture "versions.json")
    | "/registries/crates.io/packages/serde/versions/1.0.0" ->
        (200, "", fixture "version.json")
    | "/registries/crates.io/packages/serde/dependent_packages" ->
        (200, "", fixture "dependents.json")
    | "/registries/opam.ocaml.org/packages/eio" ->
        (200, "", fixture "opam_package.json")
    | "/packages/lookup" -> (200, "", fixture "lookup.json")
    | "/registries/crates.io/maintainers/slaxxarn" ->
        (200, "", fixture "maintainer.json")
    | "/keywords/rust" -> (200, "", fixture "keyword.json")
    | "/registries/npmjs.org/packages/minimist" ->
        (200, "", fixture "advisories.json")
    | _ -> (404, "", {|{"error":"not found"}|})

(* [run env args] is the exit code, stdout and stderr of [oecosystems args]
   against the loopback server, and the request targets seen. *)
let run ?base_url ?(route = route) ?(out_fun = None) env args =
  Loopback.with_server env route (fun ~sw:_ ~base_url:served seen ->
      let base_url = Option.value base_url ~default:served in
      let out = Buffer.create 256 and err = Buffer.create 256 in
      let fmt b = Format.formatter_of_buffer b in
      let o =
        match out_fun with
        | Some f -> Format.make_formatter f (fun () -> ())
        | None -> fmt out
      and e = fmt err in
      let cmd = Ecosystems_cli.main ~out:o ~err:e env in
      let argv =
        Array.of_list (("oecosystems" :: args) @ [ "--base-url"; base_url ])
      in
      let code = Cmdliner.Cmd.eval' ~argv ~err:e cmd in
      Format.pp_print_flush o ();
      Format.pp_print_flush e ();
      let target hs =
        match String.split_on_char ' ' (List.nth hs (List.length hs - 1)) with
        | _ :: t :: _ -> t
        | _ -> ""
      in
      ( code,
        Buffer.contents out,
        Buffer.contents err,
        List.rev_map target !seen ))

let lines s = List.filter (( <> ) "") (String.split_on_char (Char.chr 10) s)

let check env args ~expect =
  let code, out, err, _ = run env args in
  let missing = List.filter (fun sub -> not (contains ~sub out)) expect in
  if code <> 0 || missing <> [] then (
    Printf.eprintf "FAIL %s\nexit %d, missing [%s]\nstdout: %s\nstderr: %s\n"
      (String.concat " " args) code (String.concat "; " missing) out err;
    exit 1)

let () =
  Eio_main.run @@ fun env ->
  check env [ "registries" ] ~expect:[ "npmjs.org" ];
  check env [ "package"; "crates.io"; "serde" ] ~expect:[ "serde" ];
  check env [ "versions"; "crates.io"; "serde" ] ~expect:[ "1.0.229" ];
  check env [ "version"; "crates.io"; "serde"; "1.0.0" ] ~expect:[ "1.0.0" ];
  check env
    [ "dependents"; "crates.io"; "serde" ]
    ~expect:[ "image-color-service" ];
  check env
    [ "advisories"; "npmjs.org"; "minimist" ]
    ~expect:[ "Prototype Pollution in minimist"; "CVE-2021-44906" ];
  (* --limit 1 prints one item and asks for one page of one item. *)
  let code, out, _, targets =
    run env [ "versions"; "crates.io"; "serde"; "--limit"; "1" ]
  in
  assert (code = 0);
  assert (List.length (lines out) = 1);
  assert (List.length targets = 1);
  assert (contains ~sub:"per_page=1" (List.hd targets))

let () =
  Eio_main.run @@ fun env ->
  (* A scoped npm name is one path segment, so its separators are encoded. *)
  let _, _, _, targets = run env [ "package"; "npmjs.org"; "@types/node" ] in
  assert (targets = [ "/registries/npmjs.org/packages/%40types%2Fnode" ])

let () =
  Eio_main.run @@ fun env ->
  check env [ "maintainer"; "crates.io"; "slaxxarn" ] ~expect:[ "slaxxarn" ];
  check env [ "keyword"; "rust" ] ~expect:[ "rust" ];
  (* A purl and a repository URL select different query parameters. *)
  let purl_args = [ "lookup"; "pkg:npm/minimist" ] in
  check env purl_args ~expect:[ "minimist"; "npmjs.org" ];
  let _, _, _, targets = run env purl_args in
  assert (List.length targets = 1);
  assert (contains ~sub:"purl=" (List.hd targets));
  assert (not (contains ~sub:"repository_url=" (List.hd targets)));
  let _, _, _, targets =
    run env [ "lookup"; "https://github.com/minimistjs/minimist" ]
  in
  assert (contains ~sub:"repository_url=" (List.hd targets));
  assert (not (contains ~sub:"purl=" (List.hd targets)))

let () =
  Eio_main.run @@ fun env ->
  (* An API error is one line on stderr and exit code 1. *)
  let code, out, err, _ = run env [ "package"; "crates.io"; "missing" ] in
  assert (code = 1);
  assert (out = "");
  assert (List.length (lines err) = 1);
  assert (contains ~sub:"404" err);
  assert (not (contains ~sub:"Raised" err));
  (* So is a connection failure. *)
  let code, out, err, _ =
    run ~base_url:"http://127.0.0.1:1" env [ "registries" ]
  in
  assert (code = 1);
  assert (out = "");
  assert (List.length (lines err) = 1);
  (* --json prints the typed value, which decodes through its own codec. *)
  let code, out, _, _ = run env [ "package"; "crates.io"; "serde"; "--json" ] in
  assert (code = 0);
  let decode = Openapi.Runtime.Json.decode in
  assert (Result.is_ok (decode Ecosystems.Package.T.jsont out));
  let _, out, _, _ = run env [ "registries"; "--json" ] in
  assert (
    Result.is_ok
      (decode (Jsont.list Ecosystems.Registry.T.jsont) out));
  (* A null field is shown as a dash. *)
  let _, out, _, _ =
    run env [ "versions"; "crates.io"; "serde"; "--limit"; "1" ]
  in
  assert (String.ends_with ~suffix:" -" (String.trim out))

let () =
  Eio_main.run @@ fun env ->
  (* [dependents] stops at 100 items by default, even if the server keeps
     answering. *)
  let endless n _ =
    if n > 150 then (200, "", "[]") else (200, "", fixture "dependents.json")
  in
  let code, out, _, targets =
    run ~route:endless env [ "dependents"; "crates.io"; "serde" ]
  in
  assert (code = 0);
  assert (List.length (lines out) = 100);
  assert (List.length targets = 100);
  (* Inputs the client rejects are one-line errors, not backtraces. *)
  let one_line_error ?base_url args =
    let code, out, err, _ = run ?base_url env args in
    assert (code = 1);
    assert (out = "");
    assert (List.length (lines err) = 1)
  in
  one_line_error ~base_url:"notaurl" [ "registries" ];
  one_line_error [ "package"; "npmjs.org"; ".." ];
  (* A non-positive limit is a usage error. *)
  let code, _, _, targets = run env [ "registries"; "--limit=-1" ] in
  assert (code = Cmdliner.Cmd.Exit.cli_error);
  assert (targets = []);
  (* A closed pipe ends the command quietly. *)
  let broken _ _ _ = raise (Sys_error "Broken pipe") in
  let code, _, err, _ = run ~out_fun:(Some broken) env [ "registries" ] in
  assert (code = 0);
  assert (err = "")

let () =
  Eio_main.run @@ fun env ->
  (* Registries that do not count downloads leave the field null. *)
  check env
    [ "package"; "opam.ocaml.org"; "eio" ]
    ~expect:[ "eio"; "downloads    -" ]
