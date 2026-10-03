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
    | _ -> (404, "", {|{"error":"not found"}|})

(* [run env args] is the exit code, stdout and stderr of [oecosystems args]
   against the loopback server, and the request targets seen. *)
let run env args =
  Loopback.with_server env route (fun ~sw:_ ~base_url _ ->
      let out = Buffer.create 256 and err = Buffer.create 256 in
      let fmt b = Format.formatter_of_buffer b in
      let o = fmt out and e = fmt err in
      let cmd = Ecosystems_cli.main ~out:o ~err:e env in
      let argv =
        Array.of_list (("oecosystems" :: args) @ [ "--base-url"; base_url ])
      in
      let code = Cmdliner.Cmd.eval' ~argv ~err:e cmd in
      Format.pp_print_flush o ();
      Format.pp_print_flush e ();
      (code, Buffer.contents out, Buffer.contents err))

let () =
  Eio_main.run @@ fun env ->
  let code, out, _ = run env [ "registries" ] in
  assert (code = 0);
  assert (contains ~sub:"npmjs.org" out)
