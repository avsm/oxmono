let check message condition = if not condition then failwith message

let rec remove path =
  if (Unix.lstat path).Unix.st_kind = Unix.S_DIR then (
    Array.iter
      (fun name -> remove (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path)
  else Unix.unlink path

let refuses f =
  match f () with
  | _ -> failwith "expected Git failure"
  | exception Eio.Io (Gitops.Git _, _) -> ()

let () =
  let root = Filename.temp_file "gitops-test-" "" in
  Unix.unlink root;
  Unix.mkdir root 0o700;
  Fun.protect
    ~finally:(fun () -> remove root)
    (fun () ->
      Eio_main.run (fun env ->
          let git = Gitops.v ~dry_run:false env in
          let repo = Eio.Path.(env#fs / root) in
          let run args =
            let r =
              Eioext.Process.capture ~process_mgr:env#process_mgr
                ([ "git"; "-C"; root ] @ args)
            in
            check "test Git command" (r.status = `Exited 0)
          in
          refuses (fun () -> Gitops.status_files git ~repo);
          refuses (fun () -> Gitops.status_porcelain git ~repo);
          refuses (fun () -> Gitops.rev_parse_opt git ~repo "HEAD");
          refuses (fun () -> Gitops.current_branch git ~repo);
          refuses (fun () -> Gitops.remote_url git ~repo ~remote:"origin");
          run [ "init"; "--initial-branch=main" ];
          run [ "config"; "user.name"; "Test" ];
          run [ "config"; "user.email"; "test@example.invalid" ];
          check "missing reference"
            (Gitops.rev_parse_opt git ~repo "HEAD" = None);
          check "missing remote"
            (Gitops.remote_url git ~repo ~remote:"origin" = None);
          check "unborn branch" (Gitops.current_branch git ~repo = Some "main");
          let write relative data =
            Out_channel.with_open_bin (Filename.concat root relative)
              (fun out -> output_string out data)
          in
          write "note" "first";
          run [ "add"; "note" ];
          write ".git/hooks/pre-commit" "#!/bin/sh\nexit 1\n";
          Unix.chmod (Filename.concat root ".git/hooks/pre-commit") 0o700;
          refuses (fun () -> Gitops.commit git ~repo ~msg:"must fail");
          Unix.unlink (Filename.concat root ".git/hooks/pre-commit");
          Gitops.commit git ~repo ~msg:"initial";
          write "note" "changed";
          check "porcelain preserves first status column"
            (String.starts_with ~prefix:" M "
               (Gitops.status_porcelain git ~repo));
          run [ "checkout"; "--"; "note" ];
          run [ "checkout"; "--detach" ];
          check "detached branch" (Gitops.current_branch git ~repo = None);
          let config =
            { Gitops.Sync.Config.default with remote = root ^ "/absent.git" }
          in
          refuses (fun () -> Gitops.Sync.pull git ~config ~repo);
          refuses (fun () -> Gitops.Sync.push git ~config ~repo ())));
  print_endline "Gitops query, commit and synchronization failures passed"
