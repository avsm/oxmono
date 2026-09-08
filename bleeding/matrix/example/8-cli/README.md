# `8-cli`

<br>

`cli` is the login-and-save program of `7-profile` rebuilt on
[`Matrix_cli`](../../lib/matrix_cli/matrix_cli.mli), the set of cmdliner
terms every command-line tool in this tree shares. It reads `--homeserver`,
`--username`, `--password-file`, `--profile` and `-v`, falls back to
`MATRIX_HOMESERVER`, `MATRIX_USERNAME` and `MATRIX_PASSWORD` when a flag is
absent, and takes no positional arguments.

<br>

Every command-line tool that talks to a homeserver needs the same handful
of settings, a homeserver URL, credentials or a stored session to reuse,
and how much to report about what it does, and repeating them as separate
flags in each program would obscure what that program is actually about.
A profile name lets several tools, and several accounts, share one machine
without colliding on the same session file, and a log level lets a run
stay quiet by default and turn talkative only when something needs
diagnosing.

<br>

```ocaml
let term =
  Cmdliner.Term.(
    const run $ Cli.verbosity_term $ Cli.homeserver_term $ Cli.username_opt_term
    $ Cli.password_opt_term $ Cli.profile_term)

let cmd =
  let doc = "keep a Matrix session on disk, logging in only when needed" in
  let man = [ `S Cmdliner.Manpage.s_description; `P "..." ] in
  let exits =
    Cmdliner.Cmd.Exit.defaults
    @ [
        Cmdliner.Cmd.Exit.info Cli.exit_usage
          ~doc:"there is no session, and no password to create one";
        Cmdliner.Cmd.Exit.info Cli.exit_internal ~doc:"the profile is damaged";
      ]
  in
  Cmdliner.Cmd.v (Cmdliner.Cmd.info "cli" ~doc ~man ~exits) term

let () = exit (Cmdliner.Cmd.eval cmd)
```

`run` loads or creates the session exactly as `7-profile`'s main function
does, with `Logs.app` in place of `Printf.printf` for its two lines of
output, and `man` is the paragraph cmdliner prints under `DESCRIPTION`.

<br>

[`Cli.homeserver_term`](../../lib/matrix_cli/matrix_cli.mli),
[`Cli.username_opt_term`](../../lib/matrix_cli/matrix_cli.mli) and
[`Cli.password_opt_term`](../../lib/matrix_cli/matrix_cli.mli) each parse one
flag and fall back to an environment variable when it is absent, which is
why the exports below still work without repeating them as flags. There is
no `--password` flag.
[`Cli.password_opt_term`](../../lib/matrix_cli/matrix_cli.mli) reads only
`--password-file` or `MATRIX_PASSWORD`, because a command line is readable
by every other process on the machine through `/proc` or `ps`.

<br>

[`Cli.verbosity_term`](../../lib/matrix_cli/matrix_cli.mli) runs first in the
applicative chain. Evaluating it installs the global `Logs` reporter, so it
must run before anything else tries to log. `-v` prints informational
messages, such as the login call succeeding. Without it only `Logs.app`
output, the two lines this program always prints, appears.

<br>

When no session is stored and `--username` or `MATRIX_PASSWORD` is missing,
the program exits with
[`Cli.exit_usage`](../../lib/matrix_cli/matrix_cli.mli), 124. When a stored
session exists but does not parse, it exits with
[`Cli.exit_internal`](../../lib/matrix_cli/matrix_cli.mli), 70. Listing both
with `Cmd.Exit.info` is what puts them under `EXIT STATUS` in `--help`.

<pre><code><b>$ dune exec -- example/8-cli/cli.exe --help</b>
NAME
       cli - keep a Matrix session on disk, logging in only when needed

OPTIONS
       -P NAME, --profile=NAME (absent=default)
           Profile name for session storage.

       -s URL, --homeserver=URL (required)
           Matrix homeserver URL. Can also be set via MATRIX_HOMESERVER.

EXIT STATUS
       cli exits with:

       0   on success.
       70  the profile is damaged
       123 on indiscriminate errors reported on standard error.
       124 on command line parsing errors.
       124 there is no session, and no password to create one

<b>$ export MATRIX_HOMESERVER=http://127.0.0.1:8008 MATRIX_USERNAME=alice-b-88f9a4 MATRIX_PASSWORD=pw12345</b>
<b>$ export XDG_DATA_HOME=$(mktemp -d)</b>
<b>$ dune exec -- example/8-cli/cli.exe -v --profile tutorial</b>
cli.exe: [INFO] Logging in as alice-b-88f9a4
cli.exe: [INFO] Login successful: user_id=@alice-b-88f9a4:localhost device_id=QBHSJUQURV
Logged in and saved session to profile "tutorial"
Logged in as @alice-b-88f9a4:localhost
<b>$ dune exec -- example/8-cli/cli.exe -v --profile tutorial</b>
Reusing session for profile "tutorial"
Logged in as @alice-b-88f9a4:localhost
</code></pre>

<br>

**Next:** [`9-encrypt`](../9-encrypt#folders-and-files) adds end-to-end
encryption to a profile like this one.

<br>

[Up to the example index](../#readme)
