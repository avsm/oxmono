let () =
  let cases =
    [
      {|opam-version: "2.0"
name: "demo"
version: "1.0"
synopsis: "Ignored metadata"
depends: ["ocaml" {>= "5.2"} "fmt" {with-test}]
build: [["dune" "build" "-p" name "-j" jobs]]
url { src: "https://example.invalid/demo.tar.gz"
      checksum: "sha256=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" }
|};
      {|opam-version: "2.0"
available: os != "win32"
flags: [conf]
|};
    ]
  in
  List.iter
    (fun s ->
      let opam = OpamFile.OPAM.read_from_string s in
      let canonical =
        OpamFile.OPAM.effective_part opam |> OpamFile.OPAM.write_to_string
      in
      print_string canonical;
      List.iter
        (fun kind ->
          print_endline
            (OpamHash.to_string (OpamHash.compute_from_string ~kind canonical)))
        [ `MD5; `SHA256; `SHA512 ])
    cases;
  let pid =
    OpamProcess.create_process_env "/bin/sh"
      [| "sh"; "-c"; "printf '%s\\n' \"$OX_PROBE\"" |]
      [| "OX_PROBE=child-env-ok" |]
      Unix.stdin Unix.stdout Unix.stderr
  in
  let _, status = Unix.waitpid [] pid in
  assert (status = Unix.WEXITED 0)
