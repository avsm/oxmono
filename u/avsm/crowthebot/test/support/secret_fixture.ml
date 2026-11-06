(* Reads and writes encrypted named configurations for CLI tests, using the
   same XDG directory, Sekrit key and encryption context as crowthebot. *)

open Crowthebot

let usage () =
  prerr_endline
    "usage: secret_fixture get PROFILE TOOL NAME | put PROFILE TOOL NAME JSON";
  exit 2

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let fs = Eio.Stdenv.fs env in
  let dir profile =
    let config = Xdge.config_dir (Xdge.create fs "crowthebot") in
    let secrets = Eio.Path.(config / "secrets") in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 secrets;
    Eio.Path.(secrets / profile)
  in
  let key_file = Eio.Path.(fs / Sekrit.Paths.key_file ()) in
  match Array.to_list Sys.argv |> List.tl with
  | [ "get"; profile; tool; name ] -> (
      Secret_store.with_dir ~sw ~key_file (dir profile) @@ fun store ->
      match Secret_store.get store ~tool ~name with
      | None ->
          prerr_endline "missing";
          exit 1
      | Some json ->
          print_endline
            (Result.get_ok (Jsont_bytesrw.encode_string Jsont.json json)))
  | [ "put"; profile; tool; name; json ] ->
      let json = Result.get_ok (Jsont_bytesrw.decode_string Jsont.json json) in
      Secret_store.with_dir ~sw ~key_file (dir profile) @@ fun store ->
      Secret_store.put store ~tool ~name ~replace:true json
  | _ -> usage ()
