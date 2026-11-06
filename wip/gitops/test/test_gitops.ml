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
          refuses (fun () -> Gitops.Sync.push git ~config ~repo ());
          let head () = Gitops.rev_parse git ~repo "HEAD" in
          let before = head () in
          let dry = Gitops.v ~dry_run:true env in
          write "note" "dry";
          Gitops.add_all dry ~repo;
          Gitops.commit dry ~repo ~msg:"dry";
          check "dry run leaves HEAD" (head () = before);
          check "dry run leaves index" (Gitops.status git ~repo = `Dirty);
          check "status lists modified file"
            (Gitops.status_files git ~repo = [ " M note" ]);
          run [ "checkout"; "--"; "note" ];
          check "clean status" (Gitops.status git ~repo = `Clean);
          (* Round trip through a local bare remote. *)
          let remote = Filename.concat root "remote.git" in
          let a = Filename.concat root "a" and b = Filename.concat root "b" in
          let path p = Eio.Path.(env#fs / p) in
          run [ "init"; "--bare"; "--initial-branch=main"; remote ];
          run [ "init"; "--initial-branch=main"; a ];
          let config = { Gitops.Sync.Config.default with remote } in
          write "a/one" "1";
          check "first push publishes"
            (Gitops.Sync.push git ~config ~repo:(path a) ());
          check "clean push is a no-op"
            (not (Gitops.Sync.push git ~config ~repo:(path a) ()));
          Gitops.clone git ~url:remote ~target:(path b);
          check "clone is a repository" (Gitops.is_repo git ~repo:(path b));
          write "b/two" "2";
          check "second clone pushes"
            (Gitops.Sync.push git ~config ~repo:(path b) ());
          check "pull merges remote change"
            (Gitops.Sync.pull git ~config ~repo:(path a));
          check "pulled file present"
            (Sys.file_exists (Filename.concat a "two"));
          check "repeated pull is a no-op"
            (not (Gitops.Sync.pull git ~config ~repo:(path a)));
          check "heads agree"
            (Gitops.rev_parse git ~repo:(path a) "HEAD"
            = Gitops.rev_parse git ~repo:(path b) "HEAD")));
  print_endline "Gitops queries, commits, dry runs and synchronization passed"
