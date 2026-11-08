(* SPDX-License-Identifier: ISC *)

let read file = In_channel.with_open_bin file In_channel.input_all

let report file =
  match Brief.of_string (read file) with
  | Ok t -> t
  | Error e -> failwith (file ^ ": " ^ e)

let check name b = if not b then failwith ("check failed: " ^ name)
let names l = List.map (fun (d : Brief.detection) -> d.name) l

let () =
  let t = report "ocaml.json" in
  check "version" (t.version = "0.15.0");
  check "languages" (names t.languages = [ "OCaml" ]);
  check "confidence"
    ((List.hd t.languages).confidence = Medium
    && (List.hd t.languages).command = None);
  check "package managers" (names t.package_managers = [ "opam" ]);
  check "command"
    (match Brief.package_manager t "OPAM" with
    | Some { command = Some c; config_files = [ "*.opam" ]; _ } ->
        c.run = "opam install . --deps-only"
        && c.source = Knowledge_base && c.alternatives = []
    | _ -> false);
  check "tool"
    (match Brief.tool t ~category:"build" "dune" with
    | Some { command = Some c; _ } ->
        c.alternatives = [ "dune clean"; "dune exec" ]
    | _ -> false);
  check "missing tool" (Brief.tool t ~category:"test" "Dune" = None);
  check "layout"
    (t.layout
    = Some
        {
          source_dirs = [ "lib" ];
          test_dirs = [ "test" ];
          mirrors_source = false;
        });
  check "resources"
    (match t.resources with
    | Some r -> r.readme = Some "README.md" && r.changelog = Some "CHANGES.md"
    | None -> false);
  check "git"
    (match t.git with
    | Some g ->
        g.branch = Some "main" && g.commit_count = 12
        && g.remotes = [ ("origin", "https://github.com/example/project") ]
    | None -> false);
  check "stats" (t.stats.files_checked = 540 && not t.stats.scan_truncated);
  check "no manifests" (t.manifests = [] && Brief.ecosystems t = []);
  check "roundtrip" (Brief.of_string (Brief.to_string t) = Ok t)

let () =
  let t = report "oxmono.json" in
  check "languages" (names t.languages = [ "OCaml"; "C"; "Python" ]);
  check "package managers" (names t.package_managers = [ "opam"; "Cargo" ]);
  check "tool categories"
    (List.map fst t.tools = [ "build"; "ci"; "docs"; "security"; "test" ]);
  check "ci tool" (Brief.tool t ~category:"ci" "GitHub Actions" <> None);
  check "ecosystems" (Brief.ecosystems t = [ "github-actions"; "cargo" ]);
  check "manifest kinds"
    (List.map (fun (m : Brief.manifest) -> m.kind) t.manifests
    = [ "manifest"; "manifest"; "lockfile" ]);
  check "dependency"
    (match t.dependencies with
    | d :: _ ->
        d.name = "actions/checkout"
        && d.version = Some "v4"
        && d.purl = "pkg:githubactions/actions/checkout"
        && d.direct && d.scope = Some "runtime" && d.source = None
    | [] -> false);
  check "style"
    (match t.style with
    | Some s -> s.indentation = Some "4-space" && s.trailing_newline = None
    | None -> false);
  check "agents"
    (match t.resources with
    | Some r -> r.agents = [ ("agents", "AGENTS.md") ]
    | None -> false);
  check "truncated" (t.stats.scan_truncated && t.git = None);
  check "roundtrip" (Brief.of_string (Brief.to_string t) = Ok t);
  let summary = Brief.summary t in
  check "summary"
    (String.starts_with ~prefix:"Languages:        OCaml, C, Python\n" summary
    && String.ends_with
         ~suffix:"The scan stopped at brief's limit on entries.\n" summary)

(* A report from another brief may carry members and values this module does
   not know. *)
let () =
  let json =
    {|{"version":"9.0.0","path":"/src","languages":[{"name":"Zig",
       "category":"language","confidence":"certain","novelty":true}],
       "package_managers":[],"tools":{},"stats":{"duration_ms":1},
       "unknown":{"deep":[1,2]}}|}
  in
  match Brief.of_string json with
  | Ok t ->
      check "other confidence"
        ((List.hd t.languages).confidence = Other "certain");
      check "defaults" (t.stats.files_checked = 0 && t.tools = []);
      check "no language" (Brief.language t "ocaml" = None)
  | Error e -> failwith e

let () =
  match Brief.of_string {|{"version":"0.15.0"}|} with
  | Ok _ -> failwith "a report without a path parsed"
  | Error _ -> ()

let () = print_endline "test_brief: ok"
