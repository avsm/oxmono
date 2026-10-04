(* Unit tests for the [dune-package] relocatability workaround in
   {!D10.Layer}. See the [prefix_sentinel] comment in [layer.ml] for
   the full motivation: dune ≥ 3 bakes the build-time staging-dir
   absolute path into [(sections …)] of every
   [lib/<pkg>/dune-package], which breaks layer reuse across staging
   hashes. [store] rewrites the path to a sentinel; [restore]
   substitutes the consumer's prefix back in. *)

let ( / ) = Filename.concat

let make_config ~fs ~clock ~dir env =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let sys = D10.Sysops.v ~proc_mgr ~fs ~net:(Eio.Stdenv.net env) ~clock () in
  { D10.Config.sys; fs; clock; root = Eio.Path.(fs / dir); os_key = "test_os" }

(* Sample [dune-package] mimicking what dune writes for ctypes:
   absolute paths under the build-time staging dir embedded in a
   [(sections …)] block. *)
let dune_package_sample ~prefix =
  Printf.sprintf
    {|(lang dune 3.23)
(name ctypes)
(version 0.24.0)
(sections
 (lib %s/lib/ctypes)
 (libexec %s/lib/ctypes)
 (doc %s/doc/ctypes)
 (stublibs %s/lib/stublibs))
(files (lib (META ctypes.cma)))
|}
    prefix prefix prefix prefix

let write_file path content =
  let dir = Filename.dirname path in
  let rec mkdir_p p =
    if Sys.file_exists p then ()
    else begin
      mkdir_p (Filename.dirname p);
      try Unix.mkdir p 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ()
    end
  in
  mkdir_p dir;
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc content)

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* End-to-end: store rewrites prefix → sentinel, restore rewrites
   sentinel → new prefix. *)
let test_store_restore_rewrites_dune_package () =
  Helpers.with_eio_temp_dir ~prefix:"layer_rewrite"
  @@ fun ~fs ~clock ~dir env ->
  let c = make_config ~fs ~clock ~dir env in
  let staging = dir / "staging" / "build_hash" in
  let dp_rel = "lib" / "ctypes" / "dune-package" in
  write_file (staging / dp_rel) (dune_package_sample ~prefix:staging);
  D10.Layer.store c ~hash:"testhash" ~prefix:staging ~files:[ dp_rel ]
    ~package:"ctypes.0.24.0" ~deps:[] ~parent_hashes:[] ~exit_status:0 ();
  let stored = dir / "layers" / "test_os" / "testhash" / "fs" / dp_rel in
  let stored_content = read_file stored in
  Alcotest.(check bool)
    "stored content contains sentinel" true
    (String.length stored_content > 0
    &&
    let needle = "__OI_PREFIX__/lib/ctypes" in
    let n = String.length stored_content and m = String.length needle in
    let found = ref false in
    for i = 0 to n - m do
      if (not !found) && String.sub stored_content i m = needle then
        found := true
    done;
    !found);
  Alcotest.(check bool)
    "stored content has no build-time prefix" false
    (let needle = staging in
     let n = String.length stored_content and m = String.length needle in
     let found = ref false in
     for i = 0 to n - m do
       if (not !found) && String.sub stored_content i m = needle then
         found := true
     done;
     !found);
  let consumer = dir / "staging" / "other_hash" in
  D10.Layer.restore c ~hash:"testhash" ~prefix:consumer;
  let restored = consumer / dp_rel in
  let restored_content = read_file restored in
  let needle = consumer ^ "/lib/ctypes" in
  Alcotest.(check bool)
    "restored content has consumer prefix" true
    (let n = String.length restored_content and m = String.length needle in
     let found = ref false in
     for i = 0 to n - m do
       if (not !found) && String.sub restored_content i m = needle then
         found := true
     done;
     !found);
  Alcotest.(check bool)
    "restored content has no sentinel" false
    (let needle = "__OI_PREFIX__" in
     let n = String.length restored_content and m = String.length needle in
     let found = ref false in
     for i = 0 to n - m do
       if (not !found) && String.sub restored_content i m = needle then
         found := true
     done;
     !found)

(* The sentinel-form layer file must survive successive restores into
   different prefixes. If [restore] mutated the layer's [fs/] copy in
   place (e.g. truncated through a hardlink instead of breaking it),
   the second consumer would inherit the first consumer's prefix. *)
let test_two_restores_to_different_prefixes () =
  Helpers.with_eio_temp_dir ~prefix:"layer_rewrite2"
  @@ fun ~fs ~clock ~dir env ->
  let c = make_config ~fs ~clock ~dir env in
  let staging = dir / "staging" / "build_hash" in
  let dp_rel = "lib" / "ctypes" / "dune-package" in
  write_file (staging / dp_rel) (dune_package_sample ~prefix:staging);
  D10.Layer.store c ~hash:"twohash" ~prefix:staging ~files:[ dp_rel ]
    ~package:"ctypes.0.24.0" ~deps:[] ~parent_hashes:[] ~exit_status:0 ();
  let consumer_a = dir / "staging" / "consumer_a" in
  let consumer_b = dir / "staging" / "consumer_b" in
  D10.Layer.restore c ~hash:"twohash" ~prefix:consumer_a;
  D10.Layer.restore c ~hash:"twohash" ~prefix:consumer_b;
  let content_a = read_file (consumer_a / dp_rel) in
  let content_b = read_file (consumer_b / dp_rel) in
  let contains s needle =
    let n = String.length s and m = String.length needle in
    let found = ref false in
    for i = 0 to n - m do
      if (not !found) && String.sub s i m = needle then found := true
    done;
    !found
  in
  Alcotest.(check bool)
    "consumer A sees its own prefix" true
    (contains content_a (consumer_a ^ "/lib/ctypes"));
  Alcotest.(check bool)
    "consumer A doesn't see B's prefix" false
    (contains content_a consumer_b);
  Alcotest.(check bool)
    "consumer B sees its own prefix" true
    (contains content_b (consumer_b ^ "/lib/ctypes"));
  Alcotest.(check bool)
    "consumer B doesn't see A's prefix" false
    (contains content_b consumer_a)

(* Files outside the [lib/<pkg>/dune-package] pattern must not be
   rewritten — they're hardlinked by [link_tree] and the predicate in
   [is_relocatable_dune_package] is what keeps the rewrite scope
   tiny. A regression that broadened the predicate would corrupt
   ordinary install files. *)
let test_non_dune_package_files_unchanged () =
  Helpers.with_eio_temp_dir ~prefix:"layer_rewrite3"
  @@ fun ~fs ~clock ~dir env ->
  let c = make_config ~fs ~clock ~dir env in
  let staging = dir / "staging" / "build_hash" in
  let payload = Printf.sprintf "absolute path here: %s/lib/foo\n" staging in
  let other_rel = "lib" / "ctypes" / "META" in
  write_file (staging / other_rel) payload;
  D10.Layer.store c ~hash:"plainhash" ~prefix:staging ~files:[ other_rel ]
    ~package:"ctypes.0.24.0" ~deps:[] ~parent_hashes:[] ~exit_status:0 ();
  let stored = dir / "layers" / "test_os" / "plainhash" / "fs" / other_rel in
  Alcotest.(check string)
    "non-dune-package file stored verbatim" payload (read_file stored)

let test_cached_prefix () =
  Helpers.with_eio_temp_dir ~prefix:"cached_prefix"
  @@ fun ~fs ~clock ~dir env ->
  let c = make_config ~fs ~clock ~dir env in
  let staging = dir / "staging" in
  let rel = "lib/ctypes/dune-package" in
  write_file (staging / rel) (dune_package_sample ~prefix:staging);
  D10.Layer.store c ~hash:"cachedhash" ~prefix:staging ~files:[ rel ]
    ~package:"ctypes.0.24.0" ~deps:[] ~parent_hashes:[] ~exit_status:0 ();
  let prefix = D10.Prefix.assemble_cached c ~layer_hashes:[ "cachedhash" ] in
  Alcotest.(check string) "assembled metadata is rebased"
    (dune_package_sample ~prefix) (read_file (prefix / rel));
  Alcotest.(check string) "cache hit"
    prefix (D10.Prefix.assemble_cached c ~layer_hashes:[ "cachedhash" ]);
  let other = { c with os_key = "other_os" } in
  let other_prefix =
    D10.Prefix.assemble_cached other ~layer_hashes:[ "cachedhash" ]
  in
  Alcotest.(check bool) "platforms do not share prefixes" false
    (prefix = other_prefix)

let suite =
  ( "layer-rewrite",
    [
      Alcotest.test_case "cached prefixes rebase and isolate platforms" `Quick
        test_cached_prefix;
      Alcotest.test_case "store sentinelises, restore rebases" `Quick
        test_store_restore_rewrites_dune_package;
      Alcotest.test_case "two restores don't cross-contaminate" `Quick
        test_two_restores_to_different_prefixes;
      Alcotest.test_case "non-dune-package files stored unchanged" `Quick
        test_non_dune_package_files_unchanged;
    ] )
