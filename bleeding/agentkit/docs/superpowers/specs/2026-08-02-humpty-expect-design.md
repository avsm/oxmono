# humpty expect: deterministic tool transcripts for cram tests

A subcommand that drives humpty's tool layer exactly as the agent assembles
it, but from a script instead of a model, and prints a transcript whose text
is stable enough to sit in a dune cram test. It exists because a fault in the
tool layer inside the interface is invisible, as two okit incidents showed.
With it, the session that only said `dune: spawning server` becomes one cram
line that fails with the reason in the diff.

## What today's incidents demand of it

Two behaviours were found while chasing a hang, and the harness must be able
to reproduce both outside the interface.

A dune spawned while another dune instance is alive in the workspace does not
fail. It forwards the request to the running instance, exits zero, and never
creates the RPC socket, so okit waits its whole window for a socket that
cannot appear. And a dune spawned while a model has the machine's memory
wired can take longer than the window to come up, so okit gives up on a
server that then runs as an orphan.

The session therefore gains, ahead of the subcommand itself:

- During the socket wait, the spawned child is polled. A child that has
  exited ends the wait at once, and the error carries the first line of the
  child's output, which says that another dune instance holds the workspace.
- The wait traces its progress (`dune: waiting for socket 5s`), so a silent
  gap in the trace can no longer mean waiting. The window grows from 10 to
  30 seconds, affordable now that a dead child is detected at once and a
  live wait is visible.

## The subcommand

    humpty expect [--dir DIR] [--timeout SECS] [--raw] [SCRIPT]

`SCRIPT` is a file, or stdin when absent. `--dir` is the workspace, as in
`humpty agent`. `--timeout` bounds each tool call and defaults to 60
seconds. `--raw` turns the scrubber off for human debugging.

The script is line oriented. A line is blank, a `#` comment, or a tool call:
the tool's name, one space, and its arguments as the JSON object the model
would send.

    project {}
    write {"cap":"","path":"lib/x.ml","content":"let x = 1\n"}
    build {}

No model is loaded. The tool list is built by the same code path as
`humpty agent`, including okit's session start and the merlin probe, so an
expect run exercises the identical assembly, capability discipline, prompts
aside.

## Output

One transcript on stdout. The okit status prints first, in the words the
interface would show, or `okit: off (no dune-project)` when assembly was not
attempted. Each call then prints as:

    > build {}
    [dune: flush]
    [dune: build .]
    [dune: diagnostics]
    [dune: reply]
    build ok

The `>` line echoes the call. Bracketed lines are okit's trace, in order.
The remaining lines are the tool's result, exactly as the model would see
it. A tool error is output, not a failure of the run. A call that exceeds
`--timeout` prints `= timeout after 60s` and aborts the remaining script,
since a wedged session makes later output meaningless.

Exit status: zero when every call ran, nonzero on a parse error or a
timeout.

## Determinism

One scrubber, applied to results and trace alike unless `--raw`:

- The workspace root's absolute path becomes `$WS`.
- Durations in trace lines are dropped: `dune: reply 0.4s` prints as
  `dune: reply`.
- Seconds counts in wait-progress lines are dropped the same way.

Everything else must be deterministic at source, and the cram suite pins
what remains: compiler diagnostic text is stable for the switch's compiler,
and scenarios choose errors whose messages are short and stable. Merlin
presence is not assumed: cram stanzas that need it carry
`(enabled_if %{bin-available:ocamlmerlin})`.

## The cram suite

`test/expect/` holds `.t` files run by dune's cram support, with the cpu
humpty binary as a dependency, since expect never loads a model. Each test
scaffolds a fixture project in its sandbox, runs `humpty-cpu expect`, and
records the transcript. Scenarios:

- okit active: the status line, `project`, a clean `write`, `build`.
- a `write` that introduces a type error: the diagnostic arrives inline.
- `test` against a fixture with a failing expect test, then `promote`.
- a workspace with no dune-project: `okit: off (no dune-project)` and the
  plain write's behaviour.
- a workspace where a dune instance already serves: the fail-fast reason
  names the forwarding.
- merlin scenarios, gated: `outline`, `type_at`, `locate`.

## Placement

The subcommand lives beside the others in `bin/humpty.ml` with its terms in
the same style. The assembly shared with `agent` moves to one function
rather than being copied. The scrubber and the script reader are small
enough to live with the subcommand. `ARCH.md` gains a line for `test/expect`
and `CHANGES.md` records the subcommand and the session's new fail-fast.
