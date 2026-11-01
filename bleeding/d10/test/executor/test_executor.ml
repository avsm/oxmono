module Direct = D10ir.Direct
module Plan = D10ir.Plan

let ( / ) = Filename.concat
let quote = Filename.quote
let hash = D10ir.Layer_hash.of_string
let read p = In_channel.with_open_bin p In_channel.input_all
let write p s = Out_channel.with_open_bin p (fun ch -> output_string ch s)
let config = { D10ir.Config.default with inherit_path = false }

let with_cache f =
  Helpers.with_eio_temp_dir ~prefix:"executor" @@ fun ~fs ~clock ~dir env ->
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let sys = D10.Sysops.v ~proc_mgr ~fs ~net:(Eio.Stdenv.net env) ~clock () in
  let d10 =
    { D10.Config.sys; fs; clock; root = Eio.Path.(fs / dir); os_key = "test" }
  in
  let source = dir / "source" in
  Unix.mkdir source 0o755;
  f d10 proc_mgr dir source

let node name deps script : Plan.node =
  {
    package = { name; version = "1" };
    layer_hash = hash name;
    dep_layer_hashes = List.map hash deps;
    archive = { path = ""; sha256 = ""; strip_components = 0 };
    prefix = "/D10/PREFIX";
    script;
    env = [ "PATH=/usr/bin:/bin" ];
    substs = [];
    subst_vars = [];
    depexts = [];
    overlay = None;
    opam_file_sha256 = "test";
  }

let run d10 proc_mgr ?source_dir ?prepare n =
  match
    Direct.run_node ~config ~d10 ~proc_mgr ~prefix_policy:Permanent ?source_dir
      ?prepare n
  with
  | Ok status -> status
  | Error f -> Alcotest.failf "%s: %s" (Direct.string_of_phase f.phase) f.error

let test_permanent () =
  with_cache @@ fun d10 proc_mgr dir source ->
  let a_prefix = D10.Prefix.path d10 ~hash:"a" in
  let b_prefix = D10.Prefix.path d10 ~hash:"b" in
  let a =
    node "a" []
      "printf '#!/bin/sh\n\
       echo base\n\
       ' > /D10/PREFIX/bin/tool; chmod +x /D10/PREFIX/bin/tool"
  in
  ignore (run d10 proc_mgr ~source_dir:source a);
  let b =
    node "b" [ "a" ]
      "test \"$PATH\" = /d10/isolated; /bin/cp -p /D10/PREFIX/bin/tool saved; \
       printf changed > /D10/PREFIX/bin/tool; /usr/bin/touch -r saved \
       /D10/PREFIX/bin/tool; /bin/chmod 700 /D10/PREFIX/bin/tool"
  in
  let preparations = ref 0 in
  let prepare ~prefix ~build_dir (n : Plan.node) =
    incr preparations;
    Alcotest.(check string)
      "dependencies available before recipe preparation"
      "#!/bin/sh\necho base\n"
      (read (prefix / "bin/tool"));
    write (build_dir / "b-built")
      ("#!/bin/sh\nexec " ^ quote (a_prefix / "bin/tool") ^ "\n");
    Unix.chmod (build_dir / "b-built") 0o755;
    write (build_dir / "b.install") "bin: [\"b-built\" {\"b\"}]\n";
    { n with Plan.env = [ "PATH=/d10/isolated" ] }
  in
  ignore (run d10 proc_mgr ~source_dir:source ~prepare b);
  let a_store = dir / "layers/test/a/fs/bin/tool" in
  Alcotest.(check string)
    "installer did not mutate cached dependency" "#!/bin/sh\necho base\n"
    (read a_store);
  Alcotest.(check string)
    "preserved-mtime change stored" "changed"
    (read (dir / "layers/test/b/fs/bin/tool"));
  Alcotest.(check int)
    "permissions captured" 0o700
    (Unix.stat (dir / "layers/test/b/fs/bin/tool")).st_perm;
  let recipe = Option.get (D10.Layer.load_recipe_json d10 ~hash:"b") in
  let stored =
    match Plan.decode_node recipe with Ok n -> n | Error e -> Alcotest.fail e
  in
  Alcotest.(check bool)
    "prepared sources archived" true
    (Sys.file_exists stored.archive.path);
  Alcotest.(check string)
    "archive checksum" stored.archive.sha256
    (OpamHash.compute ~kind:`SHA256 stored.archive.path |> OpamHash.contents);
  Helpers.rm_rf (dir / "prefixes");
  Helpers.rm_rf source;
  assert (run d10 proc_mgr ~source_dir:source ~prepare b = `Cached);
  Alcotest.(check int) "cache hit skipped preparation" 1 !preparations;
  Alcotest.(check string)
    "absolute dependency prefix restored" "base\n"
    (Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all
       [ b_prefix / "bin/b" ]);
  (* Replay the recorded node with no source tree or recipe callback. *)
  Helpers.rm_rf (dir / "layers/test/b");
  Helpers.rm_rf b_prefix;
  let corrupt =
    { stored with archive = { stored.archive with sha256 = "wrong" } }
  in
  (match
     Direct.run_node ~config ~d10 ~proc_mgr ~prefix_policy:Permanent corrupt
   with
  | Error { phase = Unpack_archive; _ } -> ()
  | _ -> Alcotest.fail "replay must check its archive digest");
  assert (run d10 proc_mgr stored = `Built);
  Alcotest.(check string)
    "recorded recipe replay" "base\n"
    (Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all
       [ b_prefix / "bin/b" ])

let plan nodes : Plan.t =
  {
    schema_version = Plan.current_schema_version;
    os_key = "test";
    toolchain =
      { name = "fixture"; base_layer = (List.hd nodes).Plan.layer_hash };
    archive_root = ".";
    nodes;
    roots = List.map (fun n -> n.Plan.layer_hash) nodes;
    mounts = [];
    external_layers = [];
    metadata = { oi_version = "test"; generated_at = 0.; cli_invocation = [] };
  }

let test_staging () =
  with_cache @@ fun d10 proc_mgr dir source ->
  write (source / "config.in") "%{greeting}%";
  let archive = dir / "source.tar" in
  Eio.Process.run proc_mgr [ "tar"; "-cf"; archive; "-C"; source; "." ];
  let archive : D10ir.Archive.t =
    {
      path = archive;
      strip_components = 0;
      sha256 = OpamHash.compute ~kind:`SHA256 archive |> OpamHash.contents;
    }
  in
  let make name deps script =
    {
      (node name deps ("test \"$(cat config)\" = hello; " ^ script)) with
      archive;
      substs = [ "config" ];
      subst_vars = [ "greeting=hello" ];
    }
  in
  let base = make "base" [] "printf base > /D10/PREFIX/lib/order" in
  let middle =
    make "middle" [ "base" ] "printf middle > /D10/PREFIX/lib/order"
  in
  let top =
    make "top" [ "middle" ]
      "test \"$(cat /D10/PREFIX/lib/order)\" = middle; printf '#!/bin/sh\n\
       echo staged\n\
       ' > /D10/PREFIX/bin/run; chmod +x /D10/PREFIX/bin/run"
  in
  let p = plan [ top; middle; base ] in
  let run () =
    Direct.run ~config ~d10 ~fs:d10.fs ~proc_mgr ~clock:d10.clock p
  in
  let result = run () in
  Alcotest.(check int) "scheduled staging builds" 3 result.built;
  Alcotest.(check int) "staging failures" 0 result.failed;
  Alcotest.(check bool)
    "staging cleaned" false
    (Sys.file_exists (dir / "build/staging/top"));
  let relocated = dir / "elsewhere" in
  D10.Prefix.ensure d10 ~key:"top"
    ~layer_hashes:(D10.Prefix.closure d10 [ "top" ])
    ~dst:Eio.Path.(d10.fs / relocated);
  Alcotest.(check string)
    "dependency-first overlay order" "middle"
    (read (relocated / "lib/order"));
  Alcotest.(check string)
    "relocatable program runs elsewhere" "staged\n"
    (Eio.Process.parse_out proc_mgr Eio.Buf_read.take_all
       [ relocated / "bin/run" ]);
  Unix.unlink archive.path;
  let result = run () in
  Alcotest.(check int) "staging cache hit needs no archive" 3 result.cached

let test_permanent_scheduler () =
  with_cache @@ fun d10 proc_mgr dir source ->
  ignore
    (run d10 proc_mgr ~source_dir:source
       (node "base" [] "printf shared > /D10/PREFIX/lib/seed"));
  let archive = dir / "source.tar" in
  Eio.Process.run proc_mgr [ "tar"; "-cf"; archive; "-C"; source; "." ];
  let make name =
    {
      (node name [ "base" ]
         "test \"$(cat /D10/PREFIX/lib/seed)\" = shared; printf built > \
          /D10/PREFIX/lib/result")
      with
      archive = { path = archive; sha256 = ""; strip_components = 0 };
    }
  in
  let p = plan [ make "left"; make "right" ] in
  Helpers.rm_rf (dir / "prefixes");
  let result =
    Direct.run
      ~config:{ config with build_parallelism = 2 }
      ~d10 ~fs:d10.fs ~proc_mgr ~clock:d10.clock ~prefix_policy:Permanent p
  in
  Alcotest.(check int) "parallel permanent builds" 2 result.built;
  Alcotest.(check int) "shared prefix restored without races" 0 result.failed;
  Helpers.rm_rf (dir / "prefixes");
  Unix.unlink archive;
  let result =
    Direct.run ~config ~d10 ~fs:d10.fs ~proc_mgr ~clock:d10.clock
      ~prefix_policy:Permanent p
  in
  Alcotest.(check int) "parallel permanent cache hits" 2 result.cached;
  List.iter
    (fun name ->
      Alcotest.(check string)
        name "built"
        (read (D10.Prefix.path d10 ~hash:name / "lib/result")))
    [ "left"; "right" ]

let test_failed_capture () =
  with_cache @@ fun d10 proc_mgr dir source ->
  let a = node "a" [] "printf keep > /D10/PREFIX/lib/keep" in
  ignore (run d10 proc_mgr ~source_dir:source a);
  let bad = node "bad" [ "a" ] "rm /D10/PREFIX/lib/keep" in
  (match
     Direct.run_node ~config ~d10 ~proc_mgr ~prefix_policy:Permanent
       ~source_dir:source bad
   with
  | Error { phase = Diff_layer; _ } -> ()
  | _ -> Alcotest.fail "dependency deletion must fail capture");
  Alcotest.(check bool)
    "failed layer not published" false
    (D10.Layer.succeeded d10 ~hash:"bad");
  let prefix = D10.Prefix.path d10 ~hash:"bad" in
  Alcotest.(check bool)
    "failed prefix incomplete" false
    (D10.Prefix.ready ~fs:d10.fs ~key:"bad" prefix);
  let retry =
    {
      bad with
      script =
        "test -f /D10/PREFIX/lib/keep; printf retried > /D10/PREFIX/lib/retried";
    }
  in
  ignore (run d10 proc_mgr ~source_dir:source retry);
  Alcotest.(check string)
    "retry started from intact dependencies" "keep"
    (read (dir / "layers/test/a/fs/lib/keep"))

let test_snapshot () =
  with_cache @@ fun d10 _ _ source ->
  let a = source / "content"
  and b = source / "mode"
  and link = source / "link" in
  write a "before";
  write b "unchanged";
  Unix.symlink "content" link;
  let before = D10.Prefix.snapshot ~fs:d10.fs source in
  let time = (Unix.stat a).st_mtime in
  write a "after!";
  Unix.utimes a time time;
  Unix.chmod b 0o700;
  Unix.unlink link;
  Unix.symlink "mode" link;
  let changed =
    D10.Prefix.diff ~fs:d10.fs ~prefix:source ~before |> List.map fst
  in
  Alcotest.(check (list string))
    "contents, modes and symlink targets"
    [ "content"; "link"; "mode" ]
    changed;
  Alcotest.(check bool)
    "assembly key preserves overlay order" false
    (D10.Prefix.solve_hash [ "a"; "b" ] = D10.Prefix.solve_hash [ "b"; "a" ])

let test_install_to () =
  with_cache @@ fun d10 proc_mgr dir source ->
  let archive = dir / "source.tar" in
  Eio.Process.run proc_mgr [ "tar"; "-cf"; archive; "-C"; source; "." ];
  let n =
    {
      (node "uncached" [] "printf kept > /D10/PREFIX/kept") with
      archive = { path = archive; sha256 = ""; strip_components = 0 };
    }
  in
  let dst = dir / "user-prefix" in
  let result =
    Direct.run ~config ~d10 ~fs:d10.fs ~proc_mgr ~clock:d10.clock
      ~install_to:dst (plan [ n ])
  in
  Alcotest.(check int) "uncached install built" 1 result.built;
  Alcotest.(check string)
    "user-owned prefix retained" "kept"
    (read (dst / "kept"));
  Alcotest.(check bool)
    "uncached install did not store layer" false
    (D10.Layer.succeeded d10 ~hash:"uncached")

let () =
  Alcotest.run "day10 executor"
    [
      ( "execution",
        [
          Alcotest.test_case
            "permanent prefixes, isolation, restoration and replay" `Quick
            test_permanent;
          Alcotest.test_case "staging scheduler and relocation" `Quick
            test_staging;
          Alcotest.test_case
            "permanent scheduler with shared external dependencies" `Quick
            test_permanent_scheduler;
          Alcotest.test_case "failed capture and retry" `Quick
            test_failed_capture;
          Alcotest.test_case "content manifests and ordered unions" `Quick
            test_snapshot;
          Alcotest.test_case "uncached user prefix" `Quick test_install_to;
        ] );
    ]
