(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

module M = Arod_setup.Manifest
module P = Arod_setup.Pull

(* Environment isolation happens before starting Eio or any worker domains. *)
[@@@alert "-unsafe_multidomain"]

let check condition message = if not condition then failwith message
let rejects f =
  match f () with
  | _ -> failwith "Expected rejection"
  | exception Failure _ -> ()

let contains text needle =
  let n = String.length needle in
  let rec loop i =
    i + n <= String.length text &&
    (String.sub text i n = needle || loop (i + 1)) in
  loop 0

let write path text =
  P.mkdir (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let run args =
  let pid = Unix.create_process (List.hd args) (Array.of_list args)
      Unix.stdin Unix.stdout Unix.stderr in
  let rec wait () = try snd (Unix.waitpid [] pid) with
    | Unix.Unix_error (Unix.EINTR, _, _) -> wait () in
  check (wait () = Unix.WEXITED 0) "command failed"

let git path args = run (["git"; "-C"; path] @ args)
let commit path message =
  git path ["add"; "."];
  git path ["-c"; "user.name=Test"; "-c"; "user.email=test@example.invalid";
    "commit"; "-m"; message]

let rec snapshot path =
  if not (P.exists path) then [] else
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR -> (path, "directory") ::
      (Sys.readdir path |> Array.to_list |> List.sort String.compare
       |> List.concat_map (fun name -> snapshot (Filename.concat path name)))
  | Unix.S_LNK -> [path, Unix.readlink path]
  | _ -> [path, M.read path]

let rec remove path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
      Array.iter (fun name -> remove (Filename.concat path name))
        (Sys.readdir path);
      Unix.rmdir path
  | _ -> Unix.unlink path

let () =
  let root = Filename.temp_file "arod-pull-test-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect ~finally:(fun () -> remove root) (fun () ->
    Unix.putenv "HOME" root;
    Unix.putenv "XDG_CONFIG_HOME" (root ^ "/config");
    Unix.putenv "XDG_DATA_HOME" (root ^ "/data");
    Unix.putenv "XDG_CACHE_HOME" (root ^ "/cache");
    Unix.putenv "XDG_STATE_HOME" (root ^ "/state");
    Unix.putenv "SORTAL_DATA_DIR" (root ^ "/data/sortal");
    let t = M.defaults () in
    let text = M.encode t in
    check (M.decode text = t) "manifest round trip";
    check (not (contains text root)) "absolute home leaked into export";
    check (contains text "~/bushel/data") "portable paths absent";
    let secret = "SYNTHETIC-SECRET-DO-NOT-EXPORT" in
    let arod = { t.arod with server =
      { t.arod.server with stats_password = Some secret } } in
    rejects (fun () -> M.encode { t with arod });
    rejects (fun () -> M.decode
      (Tomlt_bytesrw.encode_string M.codec { t with version = 99 }));
    let b = t.bushel in
    let embeddings = Bushel_config.{
      url = "https://embeddings.example/v1/embeddings";
      model = "fixture"; dimensions = 2;
      api_key_file = Some (root ^ "/embedding-key") } in
    let with_embeddings = { t with bushel =
      { b with embeddings = Some (Bushel_config.Http embeddings) } } in
    let embedding_text = M.encode with_embeddings in
    check (not (contains embedding_text root))
      "embedding key path leaked absolute home";
    check (contains embedding_text "~/embedding-key")
      "embedding key path is not portable";
    check (M.decode embedding_text = with_embeddings)
      "embedding configuration did not round trip";
    let invalid_embeddings = { t with bushel =
      { b with embeddings = Some (Bushel_config.Http { embeddings with
        url = "https://user:secret@embeddings.example/" }) } } in
    rejects (fun () -> M.encode invalid_embeddings);
    rejects (fun () -> M.decode
      (Tomlt_bytesrw.encode_string M.codec invalid_embeddings));
    let mlx = Bushel_config.{
      model_dir = root ^ "/models/qwen-embedding";
      dimensions = 1024; max_tokens = 8192;
      query_instruction = default_query_instruction } in
    let with_mlx = { t with bushel =
      { b with embeddings = Some (Bushel_config.Mlx mlx) } } in
    let mlx_text = M.encode with_mlx in
    check (not (contains mlx_text root))
      "MLX model directory leaked absolute home";
    check (contains mlx_text "~/models/qwen-embedding")
      "MLX model directory is not portable";
    check (M.decode mlx_text = with_mlx)
      "MLX configuration did not round trip";
    let vision = { mlx with
      model_dir = root ^ "/models/qwen-vl";
      dimensions = 2048;
      query_instruction = Bushel_config.default_image_query_instruction } in
    let with_vision = { with_mlx with bushel =
      { with_mlx.bushel with image_embeddings = Some vision } } in
    let vision_text = M.encode with_vision in
    check (not (contains vision_text root))
      "image model directory leaked absolute home";
    check (contains vision_text "~/models/qwen-vl")
      "image model directory is not portable";
    check (M.decode vision_text = with_vision)
      "image and text model configuration did not round trip independently";
    rejects (fun () -> M.encode { with_vision with bushel =
      { with_vision.bushel with image_embeddings = Some
        { vision with dimensions = 32 } } });
    let invalid_mlx = { t with bushel =
      { b with embeddings = Some (Bushel_config.Mlx
        { mlx with max_tokens = 32769 }) } } in
    rejects (fun () -> M.encode invalid_mlx);
    rejects (fun () -> M.decode
      (Tomlt_bytesrw.encode_string M.codec invalid_mlx));
    List.iter (fun remote ->
      rejects (fun () -> M.encode { t with bushel =
        { b with sync = { b.sync with remote } } }))
      ["https://user:password@example.com/repo";
       "https://example.com/repo?token=secret";
       "ssh://user:password@example.com/repo";
       "ext::sh -c evil"];
    let service = List.hd t.services in
    rejects (fun () -> M.encode { t with services =
      [{ service with profile = "../../elsewhere" }] });
    rejects (fun () -> M.encode { t with bushel =
      { b with images_output_dir = root ^ "/wrong" } });
    write (Arod.Config.config_file ())
      (Tomlt_bytesrw.encode_string Arod.Config.config_codec arod);
    write (Bushel_config.config_file ())
      (Tomlt_bytesrw.encode_string Bushel_config.config_codec b ^
       "\n[unused]\napi_key = \"" ^ secret ^ "\"\n");
    write (M.config_path "standard-site" ^ "/config.json")
      "{\"current_profile\":\"anil.recoil.org\"}";
    write (M.session_path service)
      ("{\"handle\":\"anil.recoil.org\",\"pds\":\"https://bsky.social\","
       ^ "\"access_jwt\":\"" ^ secret ^ "\",\"refresh_jwt\":\"" ^ secret ^ "\"}");
    Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
      let before = snapshot root in
      let scanned = M.scan ~fs:env#fs () in
      check (not (contains (M.encode scanned) secret)) "scan exported secret";
      check (before = snapshot root) "scan wrote files";
      check (P.run ~sw ~env ~dry_run:true ~non_interactive:true
        ~skip_auth:false ~skip_process:false scanned = 0) "dry run failed";
      check (before = snapshot root) "dry run wrote files";
      let unrelated = root ^ "/other-sortal" in
      P.mkdir unrelated;
      P.mkdir (Filename.dirname (P.sortal_path env#fs));
      Unix.symlink unrelated (P.sortal_path env#fs);
      rejects (fun () -> P.check_mapping ~fs:env#fs scanned);
      Unix.unlink (P.sortal_path env#fs);
      P.mkdir scanned.sortal_dir;
      P.map_sortal ~fs:env#fs scanned;
      P.check_mapping ~fs:env#fs scanned;
      check (Unix.realpath (P.sortal_path env#fs) =
        Unix.realpath scanned.sortal_dir)
        "Sortal mapping not restored"));
    let target = root ^ "/export.toml" in
    P.write_new target text;
    rejects (fun () -> P.write_new target "replacement");
    check (M.read target = text) "export overwritten";
    check ((Unix.stat target).st_perm land 0o777 = 0o600)
      "export permissions";
    let pdfs = root ^ "/pdf-check" in
    P.mkdir (pdfs ^ "/directory.pdf");
    check (not (P.pdfs_present pdfs)) "directory counted as a PDF";
    write (pdfs ^ "/paper.pdf") "%PDF-1.4\n";
    check (P.pdfs_present pdfs) "regular PDF not found";
    let source = root ^ "/source" and checkout = root ^ "/checkout" in
    run ["git"; "init"; "--initial-branch=main"; source];
    write (source ^ "/content") "first";
    commit source "first";
    let cfg = { b.sync with remote = source; branch = "main" } in
    P.restore_repo checkout cfg;
    check (M.read (checkout ^ "/content") = "first") "clone failed";
    write (source ^ "/content") "second";
    commit source "second";
    P.restore_repo checkout cfg;
    check (M.read (checkout ^ "/content") = "second") "pull failed";
    P.restore_repo checkout cfg;
    let before = snapshot checkout in
    P.check_repo checkout cfg;
    check (snapshot checkout = before) "repository inspection wrote files";
    Eio_main.run (fun env -> Eio.Switch.run (fun sw ->
      Unix.unlink (P.sortal_path env#fs);
      Unix.rmdir t.sortal_dir;
      let bootstrap = { t with
        bushel = { b with sync = cfg; images_sync = cfg };
        sortal = cfg; services = [List.nth t.services 1] } in
      check (P.run ~sw ~env ~dry_run:false ~non_interactive:true
        ~skip_auth:false ~skip_process:true bootstrap = 1)
        "incomplete setup reported success";
      check (not (P.exists (M.session_path (List.hd bootstrap.services))))
        "missing authentication created credentials";
      check (M.read (b.data_dir ^ "/content") = "second")
        "bootstrap did not restore content"));
    write (checkout ^ "/content") "local";
    rejects (fun () -> P.restore_repo checkout cfg);
    check (M.read (checkout ^ "/content") = "local") "local edit lost";
    commit checkout "local";
    write (source ^ "/content") "remote";
    commit source "remote";
    rejects (fun () -> P.restore_repo checkout cfg);
    check (M.read (checkout ^ "/content") = "local") "divergence lost";
    rejects (fun () -> P.check_repo checkout { cfg with remote = root });
    let nonrepo = root ^ "/nonrepo" in
    P.mkdir nonrepo;
    rejects (fun () -> P.restore_repo nonrepo cfg);
    print_endline "pull tests passed")
