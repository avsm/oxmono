# brief

An OCaml reader and runner for [brief](https://github.com/git-pkgs/brief),
the project detector from git-pkgs. brief reads a project's files and reports
its languages, package managers, tools, manifests and dependencies as JSON.
`Brief.scan` runs the `brief` command on a directory, and `Brief.of_string`
reads a report it wrote, into a record whose optional members are options or
lists. Members the library does not know are ignored, so a report from a
newer brief still reads. The types follow brief 0.15.

```ocaml
Eio_main.run @@ fun env ->
match Brief.scan ~process:env#process_mgr ~clock:env#mono_clock "." with
| Ok report -> (
    print_string (Brief.summary report);
    match Brief.package_manager report "opam" with
    | Some _ -> print_endline "an OCaml project"
    | None -> ())
| Error e -> prerr_endline e
```

brief names languages and tools as it spells them, such as `OCaml`, `opam`
and `Dune`, and `Brief.language`, `Brief.package_manager` and `Brief.tool`
compare names without regard to case. `Brief.ecosystems` is the ecosystems of
the manifests found, such as `cargo` or `npm`.

`Brief.run` runs `brief -json DIR`, with a timeout of 60 seconds and at most
16 MiB of output, and is the JSON text, so a caller may keep brief's own
report alongside the parsed one. The command only reads the directory.

`test/scan.exe DIR` prints the summary of a directory, with `brief` on the
path.

brief is a single Go binary, released for Linux, macOS and Windows at
<https://github.com/git-pkgs/brief/releases>. It needs no checkout of its
own, and reports `git` only when the directory is a git checkout.
