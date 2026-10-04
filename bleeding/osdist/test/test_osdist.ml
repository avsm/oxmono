(* Smoke tests for the [Osdist] pure-data generators: Target.t metadata,
   Spec.t sidecar round-trip, and the per-family string emitters. The CLI
   subcommands ([oi dist bundle / pkg / repo]) are integration-tested at
   the cram level — these unit tests only cover the OCaml surface. *)

let contains ~needle s =
  let nl = String.length needle and sl = String.length s in
  let rec go i =
    if i + nl > sl then false
    else if String.sub s i nl = needle then true
    else go (i + 1)
  in
  nl = 0 || go 0

let sample_spec : Osdist.Spec.t =
  {
    package = "oi";
    version = "0.13.5";
    epoch = Some 1;
    maintainer = "Anil <anil@example.org>";
    homepage = "https://github.com/avsm/oi";
    license = "ISC";
    prefix = "/usr";
    synopsis = "A stateless OCaml package builder";
    description = "Long form description.";
    binaries = [ "oi"; "oix" ];
    depexts =
      [
        ("ubuntu-26.04", [ "libsqlite3-dev"; "libgmp-dev" ]);
        ("fedora-44", [ "sqlite-devel"; "gmp-devel" ]);
      ];
  }

(* -- Target.t ----------------------------------------------------------- *)

let test_default_targets () =
  let tags =
    List.map (fun (t : Osdist.Target.t) -> t.tag) Osdist.Target.default_targets
  in
  Alcotest.(check (list string))
    "default tags"
    [
      "ubuntu-24.04"; "ubuntu-26.04"; "debian-13"; "fedora-44"; "alpine-static";
    ]
    tags

let test_of_tag () =
  Alcotest.(check bool)
    "ubuntu-26.04 found" true
    (Option.is_some (Osdist.Target.of_tag "ubuntu-26.04"));
  Alcotest.(check bool)
    "nonsense rejected" true
    (Option.is_none (Osdist.Target.of_tag "no-such-distro"))

let test_parse_list () =
  let ts = Osdist.Target.parse_list "ubuntu-26.04,fedora-44" in
  Alcotest.(check int) "two parsed" 2 (List.length ts);
  Alcotest.check_raises "all unknown rejected"
    (Failure
       "osdist: no known targets in \"none,zilch\" (known: ubuntu-24.04, \
        ubuntu-26.04, debian-13, fedora-44, alpine-static)") (fun () ->
      ignore (Osdist.Target.parse_list "none,zilch"))

(* -- Spec.t sidecar round-trip ----------------------------------------- *)

let test_sidecar_roundtrip () =
  let path = Filename.temp_file "osdist-sidecar-" ".json" in
  Osdist.Spec.write_sidecar ~path sample_spec;
  match Osdist.Spec.read_sidecar ~path with
  | Error e -> Alcotest.failf "read failed: %s" e
  | Ok r ->
      Alcotest.(check string) "package" sample_spec.package r.package;
      Alcotest.(check string) "version" sample_spec.version r.version;
      Alcotest.(check (option int)) "epoch" sample_spec.epoch r.epoch;
      Alcotest.(check string) "prefix" sample_spec.prefix r.prefix;
      (* depexts: order is StringMap-sorted on read, so compare canonically. *)
      let sort_pairs xs =
        List.sort (fun (a, _) (b, _) -> String.compare a b) xs
      in
      Alcotest.(check (list (pair string (list string))))
        "depexts"
        (sort_pairs sample_spec.depexts)
        (sort_pairs r.depexts);
      Sys.remove path

(* -- Sidecar path convention ------------------------------------------- *)

let test_sidecar_path () =
  Alcotest.(check string)
    "drops .tar.gz" "/x/oi-0.13.5.osdist.json"
    (Osdist.Spec.sidecar_path ~bundle_path:"/x/oi-0.13.5.tar.gz");
  Alcotest.(check string)
    "appends when no suffix" "/x/odd.osdist.json"
    (Osdist.Spec.sidecar_path ~bundle_path:"/x/odd")

(* -- Deb generators ---------------------------------------------------- *)

let test_deb_control () =
  let s =
    Osdist.Deb.control sample_spec Osdist.Target.ubuntu_26_04
      ~overlay_depexts:[ "libgmp-dev"; "libsqlite3-dev" ]
  in
  Alcotest.(check bool) "carries source" true (contains ~needle:"Source: oi" s);
  Alcotest.(check bool) "carries depexts" true (contains ~needle:"libgmp-dev" s);
  Alcotest.(check bool)
    "carries debhelper" true
    (contains ~needle:"debhelper-compat (= 13)" s);
  Alcotest.(check bool)
    "homepage present" true
    (contains ~needle:"Homepage: https://" s)

let test_deb_rules_dollar_at () =
  (* Regression: Fmt.str (via Format) treats [@] as a control character
     and silently eats it unless escaped as [@@]. An unescaped [$@] in
     the rules template produced [dh $], which dh rejected with "Unknown
     sequence $". Pin the emitted file to contain the literal [dh $@]. *)
  let r = Osdist.Deb.rules sample_spec Osdist.Target.debian_13 in
  Alcotest.(check bool) "dh \\$@ preserved" true (contains ~needle:"dh $@" r);
  Alcotest.(check bool)
    "build.sh build" true
    (contains ~needle:"./build.sh build" r);
  Alcotest.(check bool)
    "build.sh install" true
    (contains ~needle:"./build.sh install /usr" r)

let test_deb_changelog_epoch () =
  let s =
    Osdist.Deb.changelog sample_spec Osdist.Target.ubuntu_26_04
      ~date_rfc2822:"Wed, 21 May 2026 00:00:00 +0000"
  in
  Alcotest.(check bool)
    "epoch prefix" true
    (contains ~needle:"oi (1:0.13.5-1~resolute1)" s);
  Alcotest.(check bool) "codename" true (contains ~needle:"resolute" s)

let test_deb_dockerfile () =
  let df =
    Osdist.Deb.dockerfile sample_spec Osdist.Target.debian_13
      ~overlay_depexts:[ "libsqlite3-dev" ]
  in
  let s = Dockerfile.string_of_t df in
  Alcotest.(check bool) "base image" true (contains ~needle:"debian:13" s);
  (* [dpkg-buildpackage] lives in the runtime CMD, not a RUN — the
     compile happens during [docker compose up], not [docker build]. *)
  Alcotest.(check bool)
    "dpkg-buildpackage in CMD" true
    (contains ~needle:"dpkg-buildpackage" s);
  Alcotest.(check bool) "CMD line" true (contains ~needle:"CMD " s);
  Alcotest.(check bool)
    "writes /artefacts" true
    (contains ~needle:"/artefacts/" s);
  Alcotest.(check bool)
    "no final scratch" false
    (contains ~needle:"FROM scratch" s)

let test_deb_filename () =
  let s = Osdist.Deb.filename sample_spec Osdist.Target.ubuntu_26_04 in
  Alcotest.(check string) "fn" "oi_0.13.5-1~resolute1_amd64.deb" s

(* -- Rpm generators ---------------------------------------------------- *)

let test_rpm_spec () =
  let s =
    Osdist.Rpm.spec sample_spec Osdist.Target.fedora_44
      ~overlay_depexts:[ "sqlite-devel"; "gmp-devel" ]
      ~date_rpm:"Wed May 21 2026"
  in
  Alcotest.(check bool) "name" true (contains ~needle:"Name:           oi" s);
  Alcotest.(check bool)
    "epoch tag" true
    (contains ~needle:"Epoch:          1" s);
  Alcotest.(check bool)
    "buildrequires" true
    (contains ~needle:"BuildRequires:  sqlite-devel" s);
  Alcotest.(check bool)
    "autosetup" true
    (contains ~needle:"%autosetup -n oi-0.13.5" s)

let test_rpm_filename () =
  let s = Osdist.Rpm.filename sample_spec Osdist.Target.fedora_44 in
  Alcotest.(check string) "fn" "oi-0.13.5-1.fc44.x86_64.rpm" s

(* -- Alpine static ----------------------------------------------------- *)

let test_alpine_static () =
  let df =
    Osdist.Alpine_static.dockerfile sample_spec Osdist.Target.alpine_static
  in
  let s = Dockerfile.string_of_t df in
  Alcotest.(check bool) "musl base" true (contains ~needle:"alpine-3.22" s);
  Alcotest.(check bool) "OI_STATIC" true (contains ~needle:"OI_STATIC" s);
  (* /dist (DESTDIR) is opam-owned and /artefacts (volume mount) is
     0777 so the runtime [tar] from the unprivileged [opam] user can
     write regardless of the host bind-mount owner. *)
  Alcotest.(check bool)
    "dist + artefacts pre-create" true
    (contains
       ~needle:
         "mkdir -p /dist /artefacts && chown opam:opam /dist && chmod 0777 \
          /artefacts"
       s);
  (* No FROM scratch any more — the build now runs at container-run
     time and writes the tarball straight into the bind-mounted
     /artefacts. *)
  Alcotest.(check bool)
    "no final scratch" false
    (contains ~needle:"FROM scratch" s);
  Alcotest.(check bool)
    "tarball into /artefacts" true
    (contains ~needle:"/artefacts/oi-0.13.5-linux-x86_64-static.tar.gz" s);
  Alcotest.(check string)
    "tarball" "oi-0.13.5-linux-x86_64-static.tar.gz"
    (Osdist.Alpine_static.tarball_filename sample_spec
       Osdist.Target.alpine_static)

let test_alpine_static_overlay_depexts () =
  let df =
    Osdist.Alpine_static.dockerfile
      ~overlay_depexts:[ "linux-headers"; "libffi-dev" ]
      sample_spec Osdist.Target.alpine_static
  in
  let s = Dockerfile.string_of_t df in
  Alcotest.(check bool)
    "linux-headers on apk line" true
    (contains ~needle:"linux-headers" s);
  Alcotest.(check bool)
    "libffi-dev on apk line" true
    (contains ~needle:"libffi-dev" s)

(* -- Repo index -------------------------------------------------------- *)

let test_apt_distributions () =
  let cfg : Osdist.Repo_index.config =
    {
      baseurl = "https://oi.thicket.dev/repo";
      origin = "oi";
      label = "oi";
      description = "oi packages";
      gpg_key_id = "0xDEADBEEF";
      pubkey_filename = "oi.asc";
    }
  in
  let s =
    Osdist.Repo_index.apt_distributions cfg
      ~deb_targets:
        [
          Osdist.Target.ubuntu_24_04;
          Osdist.Target.ubuntu_26_04;
          Osdist.Target.debian_13;
        ]
  in
  Alcotest.(check bool)
    "carries noble" true
    (contains ~needle:"Codename: noble" s);
  Alcotest.(check bool)
    "carries resolute" true
    (contains ~needle:"Codename: resolute" s);
  Alcotest.(check bool)
    "carries trixie" true
    (contains ~needle:"Codename: trixie" s);
  Alcotest.(check bool)
    "SignWith key" true
    (contains ~needle:"SignWith: 0xDEADBEEF" s)

let suite =
  ( "osdist",
    [
      Alcotest.test_case "default targets" `Quick test_default_targets;
      Alcotest.test_case "of_tag" `Quick test_of_tag;
      Alcotest.test_case "parse_list" `Quick test_parse_list;
      Alcotest.test_case "sidecar round-trip" `Quick test_sidecar_roundtrip;
      Alcotest.test_case "sidecar_path" `Quick test_sidecar_path;
      Alcotest.test_case "deb control" `Quick test_deb_control;
      Alcotest.test_case "deb rules $@ preserved" `Quick
        test_deb_rules_dollar_at;
      Alcotest.test_case "deb changelog" `Quick test_deb_changelog_epoch;
      Alcotest.test_case "deb dockerfile" `Quick test_deb_dockerfile;
      Alcotest.test_case "deb filename" `Quick test_deb_filename;
      Alcotest.test_case "rpm spec" `Quick test_rpm_spec;
      Alcotest.test_case "rpm filename" `Quick test_rpm_filename;
      Alcotest.test_case "alpine static" `Quick test_alpine_static;
      Alcotest.test_case "alpine static overlay depexts" `Quick
        test_alpine_static_overlay_depexts;
      Alcotest.test_case "apt distributions" `Quick test_apt_distributions;
    ] )
