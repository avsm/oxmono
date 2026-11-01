open Crowthebot

let check name value = if not value then failwith name

let denied f =
  try
    ignore (f ());
    false
  with Eio.Io _ | Invalid_argument _ -> true

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let root = Eio.Path.(Eio.Stdenv.cwd env / "test-tool-profile") in
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 root;
  Eio.Path.save ~create:(`Or_truncate 0o600) Eio.Path.(root / "token") "private";
  let plugin =
    Plugin.with_workspace ~sw ~profile_dir:root (fun cwd ->
        Plugin.
          {
            name = "files";
            description = "sandbox fixture";
            run =
              (fun ~query:_ ->
                Eio.Path.save ~create:(`Or_truncate 0o600)
                  Eio.Path.(cwd / "note")
                  "safe";
                check "parent traversal denied"
                  (denied (fun () -> Eio.Path.load Eio.Path.(cwd / "../token")));
                check "absolute paths denied"
                  (denied (fun () ->
                       Eio.Path.load
                         Eio.Path.(cwd / Eio.Path.native_exn (root / "token"))));
                Eio.Path.symlink ~link_to:"../token" Eio.Path.(cwd / "escape");
                check "symlink escape denied"
                  (denied (fun () -> Eio.Path.load Eio.Path.(cwd / "escape")));
                Eio.Path.unlink Eio.Path.(cwd / "escape");
                Eio.Path.load Eio.Path.(cwd / "note"));
          })
  in
  check "file tool can use confined cwd" (plugin.run ~query:"" = "safe");
  Eio.Path.unlink Eio.Path.(root / "workspace/note");
  Eio.Path.rmdir Eio.Path.(root / "workspace");
  Eio.Path.symlink ~link_to:"." Eio.Path.(root / "workspace");
  check "workspace cannot alias profile credentials"
    (denied (fun () ->
         Plugin.with_workspace ~sw ~profile_dir:root (fun _ -> plugin)));
  Eio.Path.unlink Eio.Path.(root / "workspace");
  Eio.Path.unlink Eio.Path.(root / "token");
  Eio.Path.rmdir root;
  print_endline "crowthebot: tool cwd confinement and symlink boundaries passed"
