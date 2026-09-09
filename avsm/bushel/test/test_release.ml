(* The releases file is written by a sync that loads it, merges onto it and
   writes it back, so a codec that loses a field silently deletes history.
   These checks pin the round trip and the two shapes that are easy to get
   wrong: a version yaml would rather read as a number, and a file that
   fails to parse. *)

module R = Bushel.Release

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let roundtrip t = R.of_yaml (R.to_yaml t)

let sample =
  {
    R.repo = "mirage/mirage";
    forge = R.Github;
    project = Some "unikernels";
    synced_at = Some (2026, 8, 31);
    releases =
      [
        {
          R.source = R.Forge;
          version = "4.5.0";
          tag = Some "v4.5.0";
          date = (2026, 3, 4);
          name = Some "Mirage 4.5.0";
          url = Some "https://github.com/mirage/mirage/releases/tag/v4.5.0";
        };
        {
          R.source = R.Registry "nixpkgs-24.11";
          version = "4.4.1";
          tag = None;
          date = (2026, 2, 1);
          name = None;
          url = None;
        };
      ];
  }

let () =
  let back = roundtrip sample in
  check "the repository survives" (back.R.repo = sample.R.repo);
  check "the forge survives" (back.R.forge = R.Github);
  check "the project survives" (back.R.project = Some "unikernels");
  check "the sync date survives" (back.R.synced_at = Some (2026, 8, 31));
  check "both releases survive" (List.length back.R.releases = 2);
  check "the whole record survives" (back = { sample with R.releases = List.sort R.compare_release sample.R.releases });

  (* A release cut on the forge writes no source, so the default has to read
     back as [Forge] rather than as a registry called "forge". *)
  let own = List.find R.is_own back.R.releases in
  check "the forge release keeps its tag" (own.R.tag = Some "v4.5.0");
  check "the forge release keeps its name" (own.R.name = Some "Mirage 4.5.0");
  check "a registry release is not its own"
    (List.exists (fun r -> not (R.is_own r)) back.R.releases);
  check "the registry keeps its name"
    (List.exists (fun r -> r.R.source = R.Registry "nixpkgs-24.11")
       back.R.releases);

  (* Newest first, whatever order they were given in. *)
  check "releases come back newest first"
    (match back.R.releases with
     | a :: b :: _ -> a.R.date = (2026, 3, 4) && b.R.date = (2026, 2, 1)
     | _ -> false);
  check "latest is the newest"
    (match R.latest back with Some r -> r.R.version = "4.5.0" | None -> false);

  (* A two-component version is the dangerous one: bare 4.10 reads back as a
     float and hands over 4.1, which would turn mirage 4.10 into 4.1. yamlrw
     quotes what needs it, so this checks the round trip rather than the
     quoting, which is yamlrw's business and not ours. *)
  let risky v =
    let t = { sample with R.releases =
      [ { R.source = R.Forge; version = v; tag = None; date = (2026, 1, 2);
          name = None; url = None } ] }
    in
    match (roundtrip t).R.releases with [ r ] -> r.R.version | _ -> "?"
  in
  List.iter (fun v ->
    check ("version " ^ v ^ " survives the round trip") (risky v = v))
    [ "4.10"; "1.2"; "4.5.0"; "1"; "2.0"; "10.0"; "0.1.0" ];

  (* A file written by hand need not quote, so the reader has to take a
     version yaml has already turned into a number. *)
  let parsed = R.of_yaml (Yamlrw.of_string
    "repo: avsm/x\nforge: github\nreleases:\n  - version: 1.2\n    date: 2026-01-02\n") in
  check "an unquoted version still parses"
    (match parsed.R.releases with [ r ] -> r.R.version = "1.2" | _ -> false);

  (* A merge replaces a repository and keeps the ones it did not cover, so a
     partial sync cannot drop the rest of the file. *)
  let other = { sample with R.repo = "avsm/other"; releases = [] } in
  let merged = R.merge [ sample; other ] [ { sample with R.project = Some "moved" } ] in
  check "merge keeps a repository it did not cover"
    (List.exists (fun t -> t.R.repo = "avsm/other") merged);
  check "merge replaces the one it did"
    (List.exists
       (fun t -> t.R.repo = "mirage/mirage" && t.R.project = Some "moved")
       merged);
  check "merge does not duplicate" (List.length merged = 2);

  Printf.printf "test_release: %d checks ok\n" !checks
