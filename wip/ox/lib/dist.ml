open Support

let targets tags arch =
  let tags = String.split_on_char ',' tags |> List.map String.trim in
  if tags = [] || List.mem "" tags then fail "Expected distribution tags";
  if not (List.mem arch [ "x86_64"; "aarch64" ]) then
    fail "Unsupported package architecture: %s" arch;
  List.sort_uniq String.compare tags
  |> List.map (fun tag ->
         match Osdist.Target.of_tag tag with
         | None -> fail "Unknown distribution target: %s" tag
         | Some t -> { t with arch })

let platform (t : Osdist.Target.t) jobs =
  let tag = Dockerfile_opam.Distro.tag_of_distro t.distro in
  let distro, version = Option.get (OpamStd.String.cut_at tag '-') in
  let kind, family =
    match distro with
    | "ubuntu" -> (`Ubuntu, "debian")
    | "debian" -> (`Debian, "debian")
    | "fedora" -> (`Fedora, "fedora")
    | "alpine" -> (`Alpine, "alpine")
    | _ -> fail "Unsupported distribution: %s" distro
  in
  {
    Osrel.arch = Osrel.Arch.of_string t.arch;
    os = { kind = `Linux kind; version; family };
    jobs;
  }

let depexts solution p =
  let env v =
    Solve.platform_value solution.Solve.platform (OpamVariable.Full.to_string v)
  in
  OpamFile.OPAM.depexts p.Solve.opam
  |> List.filter_map (fun (packages, filter) ->
         if OpamFilter.eval_to_bool ~default:false env filter then
           Some
             (OpamSysPkg.Set.elements packages |> List.map OpamSysPkg.to_string)
         else None)
  |> List.flatten
  |> List.sort_uniq String.compare

let version value =
  let value = String.map (fun c -> if c = '-' then '.' else c) value in
  if value = "" then "0"
  else if value.[0] >= '0' && value.[0] <= '9' then value
  else "0~" ^ value

let check_word label allowed value =
  if value = "" || not (String.for_all allowed value) then
    fail "Invalid %s: %S" label value

let spec p ~pkg_name ~pkg_version ~maintainer =
  let package = Option.value pkg_name ~default:(Recipe.name p) in
  let version =
    version (Option.value pkg_version ~default:(Recipe.version p))
  in
  check_word "package name (use --pkg-name)"
    (function 'a' .. 'z' | '0' .. '9' | '+' | '-' | '.' -> true | _ -> false)
    package;
  if
    String.length package < 2
    || not
         (match package.[0] with 'a' .. 'z' | '0' .. '9' -> true | _ -> false)
  then
    fail
      "Package names must start with a letter or digit and have two characters";
  check_word "package version"
    (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '.' | '~' | '_' -> true
      | _ -> false)
    version;
  let metadata =
    match
      Osdist.Spec.of_opam_file ~name:package ~version
        ~path:(p.directory / "opam")
    with
    | Ok s -> s
    | Error e -> fail "%s" e
  in
  Osdist.Spec.override ?maintainer metadata

let write_executable path contents =
  write path contents;
  Unix.chmod path 0o755

let bundle_build_sh =
  {|#!/bin/sh
set -eu
cd "$(dirname "$0")"
case "${1:-}" in
  build) exec make -j"${2:-2}" ;;
  install) exec make install PREFIX="${2:?prefix required}" DESTDIR="${3:?destdir required}" ;;
  *) echo "usage: $0 {build JOBS | install PREFIX DESTDIR}" >&2; exit 2 ;;
esac
|}

(* gzip and tar metadata are fixed so identical bundles have identical hashes
   on both BSD and GNU hosts. Python is needed only when exporting. *)
let archive_script =
  {|
import gzip, os, sys, tarfile
root, name, output = sys.argv[1:]
def normalize(info):
    info.uid = info.gid = 0
    info.uname = info.gname = ''
    info.mtime = 0
    info.pax_headers = {}
    return info
with open(output, 'wb') as raw:
    with gzip.GzipFile(filename='', fileobj=raw, mode='wb', mtime=0) as gz:
        with tarfile.open(fileobj=gz, mode='w|', format=tarfile.PAX_FORMAT) as tar:
            tar.add(os.path.join(root, name), arcname=name, filter=normalize)
|}

let bundle proc d10 config solution roots output =
  let hashes = Hashtbl.create 64 and closures = Hashtbl.create 64 in
  let config_vars = ref [] in
  let nodes =
    List.map
      (fun p ->
        let deps = Solve.dependencies solution p in
        let installed =
          List.concat_map (fun p -> Hashtbl.find closures (Recipe.name p)) deps
          |> List.sort_uniq String.compare
        in
        let dep_layer_hashes =
          List.map (fun p -> Hashtbl.find hashes (Recipe.name p)) deps
        in
        let source, source_hash =
          Source.prepare ~refresh:config.Runner.refresh proc d10 p
        in
        let layer_hash =
          hash_fields
            ([
               "ox-dist-v1";
               d10.os_key;
               string_of_int config.jobs;
               OpamFile.OPAM.write_to_string p.opam;
               source_hash;
             ]
            @ List.map D10ir.Layer_hash.to_string dep_layer_hashes)
          |> D10ir.Layer_hash.of_string
        in
        let source_dir =
          output / "sources" / D10ir.Layer_hash.to_string layer_hash
        in
        mkdir (Filename.dirname source_dir);
        command proc [ "cp"; "-R"; source; source_dir ];
        write_opam (output / "metadata" / (Recipe.name p ^ ".opam")) p.opam;
        let node : D10ir.Plan.node =
          {
            package = { name = Recipe.name p; version = Recipe.version p };
            layer_hash;
            dep_layer_hashes;
            archive =
              {
                path = "sources/" ^ source_hash;
                sha256 = source_hash;
                strip_components = 0;
              };
            script = "";
            env = [];
            depexts = depexts solution p;
            prefix = "/OX/PREFIX";
            substs = [];
            subst_vars = [];
            overlay = None;
            opam_file_sha256 = hash (OpamFile.OPAM.write_to_string p.opam);
          }
        in
        let node, bindings =
          try
            Recipe.export ~solution ~installed ~jobs:config.jobs ~source_dir p
              node
          with Failure message ->
            fail "Cannot export %s: %s" (OpamPackage.to_string p.id) message
        in
        config_vars :=
          (D10ir.Layer_hash.to_string layer_hash, bindings) :: !config_vars;
        let source_hash = tree_hash source_dir in
        let final_source = output / "sources" / source_hash in
        if exists final_source then remove_tree source_dir
        else Unix.rename source_dir final_source;
        let node =
          {
            node with
            archive =
              {
                node.archive with
                path = "sources/" ^ source_hash;
                sha256 = source_hash;
              };
          }
        in
        Hashtbl.add hashes (Recipe.name p) layer_hash;
        Hashtbl.add closures (Recipe.name p) (Recipe.name p :: installed);
        node)
      solution.Solve.packages
  in
  let root_hashes = List.map (fun name -> Hashtbl.find hashes name) roots in
  let toolchain_name =
    fst (OpamFormula.atom_of_string config.toolchain)
    |> OpamPackage.Name.to_string
  in
  let plan : D10ir.Plan.t =
    {
      schema_version = D10ir.Plan.current_schema_version;
      os_key = d10.os_key;
      toolchain =
        {
          name = toolchain_name;
          base_layer = Hashtbl.find hashes toolchain_name;
        };
      archive_root = ".";
      nodes;
      roots = List.map (fun n -> n.D10ir.Plan.layer_hash) nodes;
      mounts = [];
      external_layers = [];
      metadata =
        {
          oi_version = "ox-0.1.0";
          generated_at = 0.;
          cli_invocation = [ "ox"; "dist"; "pkg" ];
        };
    }
  in
  D10ir.Makefile.emit plan ~output ~config_vars:!config_vars
    ~bin_roots:(List.map D10ir.Layer_hash.to_string root_hashes)
    ();
  write_executable (output / "build.sh") bundle_build_sh;
  write (output / "README")
    "Run ./build.sh build JOBS, then ./build.sh install /usr DESTDIR.\n\
     Sources and resolved recipes are included. No ox or opam is needed.\n\
     Requires GNU make, a C/C++ toolchain and the target's system dependencies.\n\
     Installs the requested packages' bin, sbin and share files.\n";
  nodes
  |> List.concat_map (fun n -> n.D10ir.Plan.depexts)
  |> List.sort_uniq String.compare

let dates () =
  let time =
    match Sys.getenv_opt "SOURCE_DATE_EPOCH" with
    | None -> Unix.gettimeofday ()
    | Some value -> (
        try float_of_int (int_of_string value)
        with Failure _ -> fail "SOURCE_DATE_EPOCH must be integer seconds")
  in
  let tm = Unix.gmtime time in
  let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |] in
  let months =
    [|
      "Jan";
      "Feb";
      "Mar";
      "Apr";
      "May";
      "Jun";
      "Jul";
      "Aug";
      "Sep";
      "Oct";
      "Nov";
      "Dec";
    |]
  in
  let day = days.(tm.tm_wday) and month = months.(tm.tm_mon) in
  ( Printf.sprintf "%s, %02d %s %04d %02d:%02d:%02d +0000" day tm.tm_mday month
      (tm.tm_year + 1900) tm.tm_hour tm.tm_min tm.tm_sec,
    Printf.sprintf "%s %s %02d %04d" day month tm.tm_mday (tm.tm_year + 1900) )

let materialise dir archive (s : Osdist.Spec.t) (t : Osdist.Target.t) depexts
    (date_rfc2822, date_rpm) =
  mkdir dir;
  Unix.link archive (dir / Filename.basename archive);
  let emit path value = write (dir / path) value in
  let dockerfile =
    match t.family with
    | Deb ->
        emit "debian/control" (Osdist.Deb.control s t ~overlay_depexts:depexts);
        write_executable (dir / "debian/rules") (Osdist.Deb.rules s t);
        emit "debian/changelog" (Osdist.Deb.changelog s t ~date_rfc2822);
        emit "debian/copyright" (Osdist.Deb.copyright s);
        emit "debian/source/format" Osdist.Deb.source_format;
        Osdist.Deb.dockerfile s t ~overlay_depexts:depexts
    | Rpm ->
        emit (s.package ^ ".spec")
          (Osdist.Rpm.spec s t ~overlay_depexts:depexts ~date_rpm);
        Osdist.Rpm.dockerfile s t ~overlay_depexts:depexts
    | Static -> Osdist.Alpine_static.dockerfile s t ~overlay_depexts:depexts
  in
  emit "Dockerfile" (Dockerfile.string_of_t dockerfile)

let drivers output targets =
  let compose = Buffer.create 1024 and script = Buffer.create 1024 in
  Buffer.add_string compose "services:\n";
  Buffer.add_string script
    "#!/bin/sh\nset -eu\ncd \"$(dirname \"$0\")\"\nstatus=0\n";
  List.iter
    (fun (t : Osdist.Target.t) ->
      mkdir (output / "artefacts" / t.tag);
      Unix.chmod (output / "artefacts" / t.tag) 0o777;
      let service = String.map (fun c -> if c = '.' then '-' else c) t.tag in
      let platform =
        if t.arch = "x86_64" then "linux/amd64" else "linux/arm64"
      in
      Buffer.add_string compose
        (Printf.sprintf
           "  %s:\n\
           \    platform: %s\n\
           \    build:\n\
           \      context: ./%s\n\
           \    volumes:\n\
           \      - ./artefacts/%s:/artefacts\n"
           service platform t.tag t.tag);
      Buffer.add_string script
        (Printf.sprintf
           "docker compose -f compose.yaml build %s && docker compose -f \
            compose.yaml run --rm %s || status=1\n"
           service service))
    targets;
  Buffer.add_string script "exit \"$status\"\n";
  write (output / "compose.yaml") (Buffer.contents compose);
  write_executable (output / "build.sh") (Buffer.contents script)

let run proc ~clock ~fs ~sys (config : Runner.config) ~target ~with_packages
    ~targets ~pkg_name ~pkg_version ~maintainer ~output ~build =
  if config.jobs < 1 then fail "--jobs must be positive";
  if exists output then fail "Output directory already exists: %s" output;
  if targets = [] then fail "Select at least one distribution";
  let dates = dates () in
  mkdir config.data;
  mkdir config.cache;
  let config =
    {
      config with
      data = Unix.realpath config.data;
      cache = Unix.realpath config.cache;
    }
  in
  let output =
    if Filename.is_relative output then Sys.getcwd () / output else output
  in
  ( D10.Lock.with_lock ~clock ~fs ~path:(config.data / "metadata.lock")
  @@ fun _ ->
    D10.Lock.with_lock ~clock ~fs ~path:(config.cache / "lock") @@ fun _ ->
    let repos, _, solve_roots =
      Runner.repositories proc config ~target ~with_packages
    in
    let _, roots = Repository.resolve_binary repos target with_packages in
    let roots =
      List.map
        (fun atom ->
          fst (OpamFormula.atom_of_string atom) |> OpamPackage.Name.to_string)
        roots
    in
    publish_dir output (fun staging ->
        List.iter
          (fun (t : Osdist.Target.t) ->
            log "Preparing %s (%s)" t.tag t.arch;
            let solution =
              Solve.run ~platform:(platform t config.jobs) ~repos solve_roots
            in
            let root =
              List.find
                (fun p -> Recipe.name p = List.hd roots)
                solution.packages
            in
            let s = spec root ~pkg_name ~pkg_version ~maintainer in
            let d10 : D10.Config.t =
              {
                sys;
                fs;
                clock;
                root = Eio.Path.(fs / config.cache);
                os_key = t.tag ^ "-" ^ t.arch;
              }
            in
            let bundle_root = staging / "bundle" / t.tag in
            let name = s.package ^ "-" ^ s.version in
            let tree = bundle_root / name in
            mkdir tree;
            let depexts = bundle proc d10 config solution roots tree in
            let s =
              {
                s with
                depexts =
                  [ (Dockerfile_opam.Distro.tag_of_distro t.distro, depexts) ];
              }
            in
            let archive = bundle_root / (name ^ ".tar.gz") in
            command proc
              [ "python3"; "-c"; archive_script; bundle_root; name; archive ];
            write (archive ^ ".sha256")
              (hash_file archive ^ "  " ^ Filename.basename archive ^ "\n");
            Osdist.Spec.write_sidecar
              ~path:(Osdist.Spec.sidecar_path ~bundle_path:archive)
              s;
            remove_tree tree;
            materialise (staging / t.tag) archive s t depexts dates)
          targets;
        drivers staging targets) );
  log "Packaging tree: %s" output;
  if build then command proc [ "sh"; output / "build.sh" ]
