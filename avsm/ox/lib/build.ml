open Support

type built = { closure : string list; installed : string list }

type t = {
  d10 : D10.Config.t;
  proc : Support.proc;
  identity : string;
  jobs : int;
  refresh : bool;
}

let unique xs =
  let seen = Hashtbl.create 32 in
  List.filter
    (fun x ->
      if Hashtbl.mem seen x then false
      else (
        Hashtbl.add seen x ();
        true))
    xs

let layers built = unique (List.concat_map (fun b -> b.closure) built)

let run_with_test ~test t ~solution ~deps p =
  let layers = layers deps in
  let installed = unique (List.concat_map (fun d -> d.installed) deps) in
  let source, source_hash = Source.prepare ~refresh:t.refresh t.proc t.d10 p in
  let hash =
    hash_fields
      ([
         (if test then "ox-test-v1" else "ox-day10-v1");
         t.identity;
         OpamPackage.to_string p.Solve.id;
         OpamFile.OPAM.write_to_string p.opam;
         source_hash;
       ]
      @ layers)
  in
  let node : D10ir.Plan.node =
    {
      package = { name = Recipe.name p; version = Recipe.version p };
      layer_hash = D10ir.Layer_hash.of_string hash;
      dep_layer_hashes = List.map D10ir.Layer_hash.of_string layers;
      (* Directory inputs are archived by the executor after preparation. *)
      archive = { path = ""; sha256 = ""; strip_components = 0 };
      script = "";
      env = [];
      depexts = [];
      prefix = D10.Prefix.path t.d10 ~hash;
      substs = [];
      subst_vars = [];
      overlay = None;
      opam_file_sha256 = Support.hash (OpamFile.OPAM.write_to_string p.opam);
    }
  in
  log "%s %s"
    (if test then "Testing"
     else if D10.Layer.succeeded t.d10 ~hash then "Cached"
     else "Building")
    (OpamPackage.to_string p.id);
  let config = { D10ir.Config.default with inherit_path = false } in
  let install_to =
    if test then (
      D10.Prefix.prepare t.d10 ~layer_hashes:layers
        ~dst:Eio.Path.(t.d10.fs / node.prefix);
      Some node.prefix)
    else None
  in
  Fun.protect
    ~finally:(fun () -> if test then remove_tree node.prefix)
    (fun () ->
      match
        D10ir.Direct.run_node ~config ~d10:t.d10 ~proc_mgr:t.proc ?install_to
          ~prefix_policy:(if test then Staging else Permanent)
          ~source_dir:source
          ~prepare:(Recipe.prepare ~test ~solution ~installed ~jobs:t.jobs p)
          node
      with
      | Ok _ -> ()
      | Error failure ->
          log "Build failed; log: %s" failure.log_path;
          (if exists failure.log_path then
             let contents = read failure.log_path in
             let start = max 0 (String.length contents - 6000) in
             prerr_string
               (String.sub contents start (String.length contents - start)));
          fail "%s: %s"
            (D10ir.Direct.string_of_phase failure.phase)
            failure.error);
  {
    closure = (layers @ if test then [] else [ hash ]);
    installed =
      (if test then installed else unique (installed @ [ Recipe.name p ]));
  }

let run = run_with_test ~test:false

let test t ~solution ~deps p =
  ignore (run_with_test ~test:true t ~solution ~deps p)
