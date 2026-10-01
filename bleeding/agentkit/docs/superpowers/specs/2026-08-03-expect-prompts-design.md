# expect prompts: driving the real model from a script

`humpty expect` gains a second line form, a prompt, which sends its text to
the real model and runs the agent loop the interface runs, printing every
tool call, trace and result as it happens. It exists because a hang inside
the agent is invisible in the interface and hard to reproduce by hand. With
it, "describe this repository" becomes one script line whose transcript
shows exactly how far the exchange got.

## Script form

A script line is blank, a `#` comment, a tool call as today, or a prompt:
`? ` followed by the text to send.

    write {"cap":"","path":"lib/x.ml","content":"let x = 1\n"}
    ? describe this repository
    ? now fix the type error in lib/x.ml

Prompts and calls mix freely and run in order. All prompts share one
conversation, so a later prompt continues the exchange the earlier one
left, exactly as two requests typed into the interface would.

## Engine

A script with no prompt loads no model and runs as today. When the script
has at least one prompt, expect resolves the model before anything is
started and loads it after `assemble`, so okitd is spawned while the
process is still small, in the order `agent` uses. The engine runs on its
own domain, since the timeout fiber must keep running during generation.

expect gains the agent's model arguments: `--model`, `--think`, `--ctx`,
`--max-ctx` and `--system`. Its `--seed` defaults to 1 rather than to the
clock, so two runs of one script sample identically. The system prompt is
assembled as `agent` assembles it, from the default agent system, okit's
prompt and AGENTS.md.

## Transcript

A prompt echoes as its `? ` line. Each tool call the model makes prints as
a scripted call does, a `> ` line echoing the name and the arguments the
model sent, bracketed trace lines, then the result block, all scrubbed.
When the model answers with text alone, a `= reply` line introduces the
reply, printed verbatim and scrubbed, and the exchange is over.

Reasoning is dropped. Per-turn stats and context growth are timing
coloured, so they are dropped too. `--raw` prints all three, each
introduced by a marked line, for reading by eye.

## Timeout and failure

`--timeout` keeps bounding each scripted tool call. A new
`--prompt-timeout SECS`, defaulting to 600, bounds one whole prompt
exchange, generation and tool calls together. On expiry the transcript
ends with `= timeout after 600s`, standard error names the prompt's script
line, and the run exits nonzero by leaving the process, as a scripted
call's timeout does.

Where the transcript cuts off says which phase hung. A cut inside a `> `
block, after its traces, is a tool that did not answer. A cut directly
after the `? ` line is generation that did not finish. That split is the
diagnostic this design exists to provide.

A script with prompts and no loadable model fails before anything starts,
as a bad script line does.

## Determinism

A fixed seed and scripted prompts make a run reproducible on one machine
with one model. The tool trail is the stable spine of the transcript. The
reply is pinned only where a test chooses to pin it, since its text varies
across models and machines.

## Testing

The parser's new line form gets unit tests beside the existing script
tests. One live scenario follows the `live_okit_fork` gating and runs on
the Metal machine under `DS4_LIVE=1`, spawning the Metal humpty so the
runner itself holds nothing. Its script mixes the forms: a scripted call
writes a file the model has not seen, the first prompt is "describe this
repository", and a second prompt asks what the file says, which only a
tool can answer. The test asserts the run exits zero, that no prompt timed
out, that both prompts got a `= reply`, that some tool call appears, and
that the second reply names the file's content. The exact tool is not
pinned, since the model may legitimately change its mind, and the
transcript is printed for reading. The first prompt is the request that
hung the interface, pinned as a regression the moment it is fixed.

## Placement

`Script.parse` in `cmd/humpty_cmd.ml` returns one list of items, calls and
prompts. The expect function in `bin/humpty.ml` grows the engine branch
and the event printer. `assemble` is untouched. The man page, `ARCH.md`
and `CHANGES.md` record the new form.
