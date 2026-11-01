(* The releases file is written by commands that load it, merge onto it and
   write it back, so a codec that loses a field silently deletes history.
   These checks pin the round trip, the versions YAML would rather read as
   numbers, and the rules for merging a release into what is already there. *)

module R = Bushel.Release

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

let reg name package url = { R.name; package; url }

let rel ?tag ?(registries = []) version date =
  {
    R.version;
    tag;
    date;
    summary = "Summary of " ^ version;
    url = "https://example.org/releases/" ^ version;
    registries;
  }

let sample =
  {
    R.repo = "ucam-eo/geotessera";
    forge = R.Github;
    project = Some "tessera";
    releases =
      [
        rel ~tag:"v0.10.2" "0.10.2" (2026, 9, 4)
          ~registries:
            [
              reg "pypi.org" "geotessera"
                "https://pypi.org/project/geotessera/0.10.2";
            ];
        rel ~tag:"v0.10.1" "0.10.1" (2026, 8, 27);
      ];
  }

let roundtrip t = R.of_yaml (R.to_yaml t)

let () =
  let back = roundtrip sample in
  check "the whole record survives" (back = sample);
  check "releases are newest first"
    (List.map (fun r -> r.R.version) back.R.releases = [ "0.10.2"; "0.10.1" ]);

  (* A version that YAML would read as a number stays a string. *)
  let t =
    {
      sample with
      R.releases = [ rel "4.10" (2026, 3, 4); rel "1.0" (2026, 3, 3) ];
    }
  in
  let back = roundtrip t in
  check "4.10 survives"
    (List.exists (fun r -> r.R.version = "4.10") back.R.releases);
  check "1.0 survives"
    (List.exists (fun r -> r.R.version = "1.0") back.R.releases);

  (* A hand-written file does not quote its versions. *)
  let hand =
    Yamlrw.of_string
      "- repo: a/b\n\
      \  releases:\n\
      \    - version: 4.10\n\
      \      date: 2026-03-04\n\
      \      summary: s\n\
      \      url: https://x\n"
  in
  (match hand with
  | `A [ v ] ->
      let t = R.of_yaml v in
      check "an unquoted number reads as a string"
        ((List.hd t.R.releases).R.version = "4.1");
      check "forge defaults to github" (t.R.forge = R.Github)
  | _ -> check "hand file shape" false);

  (* The ecosyste.ms page is derived from the registry and the version. *)
  let r = List.hd sample.R.releases in
  check "metadata url"
    (R.metadata_url (List.hd r.R.registries) r
    = "https://packages.ecosyste.ms/registries/pypi.org/packages/geotessera/\
       versions/0.10.2");
  check "metadata url encodes a scoped package"
    (R.metadata_url
       (reg "npmjs.org" "@types/node" "u")
       (rel "20.1.0" (2026, 1, 1))
    = "https://packages.ecosyste.ms/registries/npmjs.org/packages/\
       %40types%2Fnode/versions/20.1.0");

  (* Registries are only ever added. *)
  let r0 = rel "1.0.0" (2026, 1, 1) ~registries:[ reg "pypi.org" "p" "u1" ] in
  let r1 =
    R.add_registries r0
      [ reg "pypi.org" "p" "other"; reg "opam.ocaml.org" "p" "u2" ]
  in
  check "an existing registry is kept as it was"
    (List.exists
       (fun x -> x.R.name = "pypi.org" && x.R.url = "u1")
       r1.R.registries);
  check "a new registry is added" (List.length r1.R.registries = 2);

  (* A release registered twice is updated and the rest survive. *)
  let updated =
    { (rel ~tag:"v0.10.2" "0.10.2" (2026, 9, 4)) with R.summary = "Edited" }
  in
  let incoming = [ { sample with R.releases = [ updated ] } ] in
  let merged = R.merge [ sample ] incoming in
  check "one repository" (List.length merged = 1);
  let m = List.hd merged in
  check "both releases remain" (List.length m.R.releases = 2);
  check "the incoming release wins"
    ((List.find (fun r -> r.R.version = "0.10.2") m.R.releases).R.summary
    = "Edited");
  check "the project survives an incoming record without one"
    ((List.hd
        (R.merge [ sample ]
           [ { sample with R.project = None; releases = [ updated ] } ]))
       .R.project
    = Some "tessera");
  let other =
    { R.repo = "x/y"; forge = R.Github; project = None;
      releases = [ rel "1.0.0" (2026, 1, 1) ] }
  in
  check "a repository not in incoming is kept"
    (List.length (R.merge [ sample; other ] incoming) = 2);
  check "a new repository is added"
    (List.length (R.merge [ sample ] [ other ]) = 2);

  (* Files. *)
  let path = Filename.temp_file "releases" ".yml" in
  R.save_file path [ sample; other ];
  check "a file round trips" (R.load_file path = R.merge [] [ sample; other ]);
  (* The writer must keep a version that YAML would read as a number. *)
  let numeric =
    {
      other with
      R.releases = [ rel "4.10" (2026, 3, 4); rel "1.0" (2026, 3, 3) ];
    }
  in
  R.save_file path [ numeric ];
  let reread = List.hd (R.load_file path) in
  check "4.10 survives the file"
    (List.map (fun r -> r.R.version) reread.R.releases = [ "4.10"; "1.0" ]);
  Out_channel.with_open_bin path (fun oc -> output_string oc "{ not: a list");
  check "a malformed file is an error"
    (match R.load_file path with _ -> false | exception Failure _ -> true);
  Sys.remove path;
  check "a missing file is empty"
    (R.load_file "/nonexistent/releases.yml" = []);
  (* A write that cannot complete leaves the file as it was. The directory is
     made read-only so that nothing new can be created in it, which stops a
     writer that goes through a temporary file. *)
  let dir = Filename.temp_file "releases" ".d" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  let file = Filename.concat dir "releases.yml" in
  R.save_file file [ sample ];
  let before = In_channel.with_open_bin file In_channel.input_all in
  Unix.chmod dir 0o555;
  let failed =
    match R.save_file file [ other ] with
    | () -> false
    | exception Sys_error _ -> true
  in
  Unix.chmod dir 0o755;
  check "a write that cannot complete is an error" failed;
  check "the file is untouched after a failed write"
    (In_channel.with_open_bin file In_channel.input_all = before);
  R.save_file file [ other ];
  check "no temporary file is left behind"
    (Sys.readdir dir = [| "releases.yml" |]);
  Sys.remove file;
  Sys.rmdir dir;
  check "a path segment is encoded"
    (R.encode_segment "feature/x#1" = "feature%2Fx%231");
  check "unreserved characters stay"
    (R.encode_segment "v1.2-rc_3~x" = "v1.2-rc_3~x");
  Printf.printf "ok: %d checks\n" !checks
