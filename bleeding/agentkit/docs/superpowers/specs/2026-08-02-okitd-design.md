# okitd: okit as its own process

okit moves out of humpty's address space. A `humpty okitd` child, spawned
before the engine exists, owns the dune session, merlin and every process
okit runs, and speaks a JSON protocol with the agent over its pipes. Humpty
itself never spawns a process once the engine is loaded.

## Why

`fork` of a process holding a hundred-gigabyte engine and its Metal threads
can block for minutes on macOS, and it blocks the calling domain, so the
interface freezes with it. Every okit spawn had this cost the moment it ran
beside a loaded model, which is why `humpty expect`, which loads no model,
never showed it. Reordering assembly before the engine fixed the spawns
made at startup. This design removes the rest: with okitd, the only fork
humpty ever performs is okitd itself, made while humpty is still small.

Two further properties fall out. Humpty can bound a tool call and kill a
wedged okitd without harming itself, so no okit fault can freeze the agent
again. And okitd treats end of file on its stdin as shutdown, so even a
force-quit humpty leaves no orphaned dune server behind.

## Processes

`humpty okitd --dir WS` is a subcommand of the same binary, documented as
internal. Assembly spawns it always, dune workspace or not, before
`V4.create`. Its stdin and stdout carry the protocol. Its stderr is drained
by humpty into a bounded tail, shown only when okitd dies.

okitd holds what okit holds today: the passive dune session and its server,
merlin, and the project map. It reads no workspace file of its own. File
content merlin needs arrives in the call, read by humpty through the same
capability the plain tools use, so the capability discipline stays where it
is.

The agent side keeps today's `Tool.t` values, codecs and schemas. Both ends
are the same binary, so the protocol carries no schemas and no versioning
beyond a hello. Handlers become thin calls to okitd. The fused `write`
saves through its capability as now, then asks okitd for the build report
alone.

`bash` routes through okitd too, since a shell spawned beside the engine
has the same fork cost as any other process. It keeps its authority story:
okitd is humpty's child with humpty's authority, and the tool is granted or
not exactly as before. In a workspace with no dune-project, okitd still
runs, serving `bash` alone, and the dune tools are absent as they are
today.

`project` returns to a live map. The describe runs in okitd, whose address
space is small, so the session-start snapshot compromise is withdrawn.

## Protocol

One JSON object per line, in both directions, encoded and decoded with
jsont codecs in the style dsml uses. Messages:

    <- {"hello": {"status": "okit: dune tools active, ocamlmerlin found",
                  "dune": true, "merlin": true}}
    -> {"call": {"id": 1, "op": "build", "targets": "."}}
    <- {"trace": {"id": 1, "line": "dune: build ."}}
    <- {"result": {"id": 1, "output": "build ok"}}
    -> {"shutdown": {}}

`hello` arrives once, after okitd has started its session, and carries the
status line the interface shows and which tool families are live. A `call`
names an operation: `build`, `test`, `promote`, `project`, `after_write`,
`outline`, `type_at`, `locate`, `bash`, each with the arguments that
operation needs (`after_write` takes the written path and returns the
diagnostic block `write` appends; the merlin operations carry the source
text). `trace` lines stream while a call runs and feed the same channel the
tools column reads. `result` carries the tool's text, ordinary output and
refusals alike, as today. Calls are sequential, one in flight.

A parse failure on either side is a protocol fault, not bad user input.
Each side reports the offending line and stops, since a same-binary peer
that speaks malformed JSON is broken, not mistaken.

## Failure

If okitd exits or its pipe closes, every okit-backed tool from then on
answers that the okit server died, with the tail of its stderr. The note the
interface shows is the greeting's, and says what okit made of the workspace
when the session started, so a death later on is told in the answers and the
traces rather than there. There is no respawn: a respawn would fork
the engine-holding process, which is the disease this design cures. If a
call exceeds the bound humpty sets, humpty kills okitd and degrades the
same way, stating the timeout. Degradation is loud, never silent.

## Code placement

    okit/proto.ml[i]   the jsont codecs and line framing
    okit/server.ml[i]  okitd's loop: read calls, dispatch, stream traces
    okit/client.ml[i]  spawn okitd, hello, call, timeout, degrade

The server dispatches into the existing `Dune_rpc`, `Merlin` and `Project`
modules unchanged. `Okit.Toolbox` becomes the client-side tool set built
over `Client`, keeping every tool name, argument codec and result format,
so the model sees no difference. `bin/humpty.ml` gains the `okitd`
subcommand and loses the in-process assembly.

## Testing

Proto codecs round-trip in unit tests. An integration test drives the real
`humpty-cpu okitd` over pipes: hello, a build against a fixture, streamed
traces, shutdown on stdin close, and the no-dune-project hello. A client
test kills okitd mid-call and asserts the degraded answers and the stderr
tail. The cram suite is the regression net for the whole move: `expect`
assembles through okitd like the agent, and the transcripts must not
change, beyond lines this design names. The live test becomes the
end-to-end pin: with a real engine loaded, one okit call through okitd
completes promptly, which is the property the whole design exists for.
