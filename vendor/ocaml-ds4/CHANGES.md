# unreleased

- Let `ds4.metal` link on non-macOS platforms and raise when the engine is used.

- Tool codecs gain composable objects, bounded arrays, optional parameters, and
  canonical argument encoding. Tools also gain safer handlers and formatters.

- Rename the engine log source from `deepseek.engine` to `ds4.engine` and make
  the package documentation build without odoc warnings.

- New `ds4f-vision-q2`, `ds4f-vision-q2-q4` and `ds4f-vision-mxfp4` download
  targets for DeepSeek V4 Flash Vision Experimental, a checkpoint frozen
  before the 0731 retrain, and `ds4f-vision-encoder` for the sidecar its
  three quants share.

- `agent --vision FILE` loads a vision sidecar and adds `view_image`, and
  `list`/`download` gain the small `glm53-vision`, `ds41-vision` and
  `qwen38-vision` targets that match it. `Ds4_cli.Coder.tools` takes an
  optional `~vision` to add the tool from a library caller too.

- Make live tests track model settings, serialise model loads and use Metal
  on macOS. Requested live tests fail when their model is missing.

- Update DS4 to 0aaea5a, including GLM attention, V4.1 routing and JPEG fixes.
  `V4.Session.checkpoint_valid` reports whether cached state can be used.

- A conversation that fills its context is compacted instead of stopping.
  `Agent.send` asks the model for a summary of itself and rebuilds the
  conversation from the system prompt, that summary, and a verbatim tail
  including the turn in progress. Reported as a `Compacted` event, and tried
  before a single tool result too large for the context is cut in the
  middle.

- A string argument may hold its own closing tag, written with `&lt;` for its
  `<` as upstream's agent reads it. `Dsml` unescapes that on decode, escapes
  it on encode in every dialect, and the tool prompts state the rule.

- `V4.create ~mtp` reads the model's metadata first and arms a draft head only
  where the model carries one, so the default `--mtp` no longer stops a
  DeepSeek model opening. `V4.has_draft_head` exposes the check.

- `grep` quotes matching lines as they are, so they can be pasted into `edit`,
  counts the files it did not search, and reports a missing path as an error.
  `write` and `edit` replace a file atomically and keep its mode, `edit` checks
  the file did not change under it, and answers with the changed lines
  numbered and how far the rest moved.

- `ds4-agent agent` is now `Ds4_cli.Coder`, whose system prompt, tools,
  printer, prompt loop and subcommand another command can reuse. It gains
  `--max-tokens` (default 16384) and `--temperature`, tells the model the date
  and the workspace's `AGENTS.md`, and Ctrl-C interrupts a reply rather than
  the process.

- Speculative decoding. `V4.create ~mtp` arms the draft head GLM 5.3 and
  Qwen3.8 carry, `V4.Session.eval_speculative` commits drafted tokens and
  `V4.Session.rewind` drops the ones a caller does not keep. `Agent` and
  `V4.generate` use it whenever a head is armed, which `chat` and `agent` do
  unless given `--no-mtp`. Qwen agents run about a quarter faster.

- Update the vendored DS4 engine to 8db1d1d, which batches Qwen3.8 Flash Next
  decoding across sessions on Metal and adds optimised Qwen CUDA kernels.

- `ds4-agent agent` shows a `?` prompt when it reads prompts from a terminal,
  so a loaded model waiting for input no longer looks hung.

- Engine diagnostics are no longer all warnings. One reporting a failure stays a
  warning, the device, mapping and buffer reports are info and appear under
  `-v`, and progress lines are debug.

- `humpty`, `numpty`, `dumpty`, the `agentkit` command and the `ds4.agentkit`
  library move to the ocaml-agentkit repository. This repository builds the
  `ds4` package alone.

- New `ds4-agent-metal`, `ds4-agent-cuda` and `ds4-agent-cpu` commands, which
  link nothing outside the `ds4` package: `list`, `download`, `chat`, and an
  `agent` over the capability file tools that reads prompts from its argument
  or from standard input. A `--model` naming no file is refused before the
  engine is created, in every command.

- New `ds4.cli` sublibrary holding the model catalogue, `Model.download`, and
  the `list`, `download` and `chat` subcommands every command shares.

- New download targets `ds41-q4`, `ds41-q2`, `qwen38-q4` and `qwen38-q2`.
  `humpty download` joins a model published in parts. With no `--model`, V4.1
  is preferred over V4 at the same quantisation.

- Run DeepSeek V4.1 Flash and Qwen3.8 Flash Next. `V4.family` reports them,
  `Dsml` gains the `Deepseek41` and `Qwen` dialects, and agents speak each
  model's tool-call markup and sampling defaults without being told.
- Agents open a thinking conversation with the model's native reasoning-effort
  instruction, which GLM agents previously omitted.

- Update the vendored DS4 engine to 9139e2a, which adds DeepSeek V4.1 Flash and
  Qwen3.8 Flash Next and builds the Engram n-gram reader without fast-math.

- Update the vendored DS4 engine to 9ab7053, fixing session rollback and GLM
  attention and improving Metal and CUDA inference. Vision sidecars now support
  DeepSeek V4 Vision Experimental as well as GLM 5.3.
- Include the CUDA quantised matrix kernels and DeepSeek vision header in the
  vendored sources and CUDA build.

- Update the vendored DS4 engine to 6cf658a. Bind live directional steering and
  GLM 5.3 vision support. Humpty `agent --vision FILE` adds a confined
  `view_image` tool whose observations remain valid across agent rollback.

- Tool-using agents retain an exact token transcript, use stable call IDs and
  stop at a complete call. Malformed calls are reported and retried.

- Tool protocol structure samples greedily without changing argument sampling.

- Session work can be cancelled cooperatively. GLM agents use native sampling
  defaults, and tool results are bounded against the context ceiling.

- Agent journals now take an exclusive writer lock before recovering their run
  and sequence numbers, so concurrent Humpty or Dumpty runs cannot corrupt them.

- A failed model open no longer prevents a corrected `V4.create` retry in the
  same process. Empty agent replies are retried and then reported as failures.

- Agent elapsed times use the Eio monotonic clock. Engine default diagnostics
  remain visible at warning level instead of disappearing at normal verbosity.

- The model-facing Okit toolset no longer exposes a shell that bypasses file
  capabilities. Humpty and Dumpty share system-prompt assembly through Okit.

- New `agentkit` command, which holds no model and browses every agent's
  journal: `list` for what exists, `runs` for one line per run, `log` with
  `--since`, `--kind` and `--run`, and `show`, which lays one run out as the
  conversation it was. A source may be an agent's name or the path of a
  journal directory copied off another machine (@avsm)

- Every agent says where its account went when it exits: `humpty agent` names
  its journal as the interface closes, `numpty` names its store however the
  run ends, and `dumpty`'s result object gains a `journal` member (@avsm)

- `dumpty` appends its records to a journal store under the XDG state
  directory as it streams them, with `run` and `seq` recovered from the store
  rather than taken from the process id, so a run whose standard error nobody
  kept can still be read back (@avsm)

- Journal records render through one `Agentkit.Show` wherever they are
  printed. `humpty log` had drifted from `numpty log`, hiding unknown record
  kinds the journal is designed to carry, and read `--since` timestamps in
  local time where the journal writes UTC. The shared reader corrects both
  (@avsm)

- The `deepseek` package and library are renamed `ds4`, after the engine they
  bind, which now runs GLM beside DeepSeek. Depend on `ds4` and its
  sublibraries `ds4.metal`, `ds4.cpu`, `ds4.cuda`, `ds4.agentkit` and
  `ds4.dsml`, and write `Ds4` where `Deepseek` was written (@avsm)

- The vendored engine is upstream's `glm-5.3-flash` branch, which adds
  inference for GLM 5.3 Flash beside the DeepSeek models (@avsm)

- GLM 5.3 uses upstream's host-aware memory guard, so the Q4 quant runs resident
  on a high-memory host without a local engine override (@avsm)

- `Dsml` speaks GLM as well as DSML. A dialect selects the GLM rendering,
  with role-marker turns, observation-wrapped tool results and the
  `<tool_call>` grammar, a streaming decoder for it, and a neutraliser for
  its markers. The agent reads the loaded model's family from the engine and
  picks the dialect itself, so `humpty agent`, `numpty` and `dumpty` run a
  GLM model with tools unchanged (@avsm)

- New `glm53-q2` and `glm53-q4` download targets for the GLM 5.3 Flash quants.
  Each target now names the Hugging Face repository it comes from, since the
  GLM quants are published in a repository of their own (@avsm)

- Generation stops at any token the engine calls a stop, not only at
  end-of-sequence. A GLM model ends its turns with role markers, which the old
  comparison ran straight through into a simulated next turn (@avsm)

- `humpty agent` records each session into a journal under the XDG state
  directory, one JSON record per line, so every prompt, reasoning, reply, tool
  call and result can be read back after the session. The new `humpty log`
  subcommand prints the journal, one line per record, with `--kind`, `--run`
  and `--since` filters (@avsm)

- A file too long for one tool result is now read a page at a time rather than
  handed over with its middle removed. `read` answers a long file with the
  lines that fit and the number of the line to continue from, `read_lines`
  bounds a window by the same budget as well as by the count asked for, and
  both say which lines they hold and how many the file has. Every line of a
  file of any length is reachable, where what an oversized result lost was not
  (@avsm)

- `outline` and `complete` are bounded by what one result carries as well as by
  a count, and each says how much of the file or the module it stands for and
  where the rest is. A window that ran out because the caller asked for that
  many lines no longer reads as one cut short (@avsm)

- A tool call the model could not finish inside one reply is no longer passed
  off as its answer. Generation reaching the `max_tokens` ceiling and the model
  ending its own reply are now distinct: the agent reports a `Cut_off` event
  and a journal record of the same name, discards the half-written call, tells
  the model what happened so it can answer with a smaller one, and raises
  `Tool_call_cut_off` after three such turns running. Before this the call was
  silently dropped, its markup became the reply, and every command reported the
  run as a success (@avsm)

- New `append` tool, which adds to the end of a file and creates it where it is
  absent. `write` takes a file as one argument, so a file longer than one reply
  could not be written at all before it (@avsm)

- `dumpty` prints `cut_off` beside `squeezed`, saying whether the reply stopped
  at the ceiling rather than where the model meant to stop, and exits nonzero
  when the model could not write a tool call inside one reply on three turns
  running. `numpty` records the same failure and still hands over, so the
  memory writes it did make survive it (@avsm)

- A result the agent shortens says what to do about it, rather than only how
  many characters went (@avsm)

- An `AGENTS.md` too long to join every prompt is cut at a line boundary
  rather than mid-sentence, and what is left in its place names the file as
  where the rest of it is (@avsm)

- New `dumpty` package and command, a one-shot agent. It gives a model the
  tools `humpty agent` has, runs one prompt to completion and exits, which is
  what a script or a build step wants. Standard output carries exactly one
  line, a JSON object holding the final turn's reply, the turn and tool-call
  counts, the context used and whether that reply was squeezed. Standard error
  carries the account as it happens, one journal record per line in the format
  `numpty log` reads. A run that could not do the work prints nothing at all on
  standard output and exits nonzero, and `--timeout` bounds the exchange at ten
  minutes so that a command left in a script cannot hang the script (@avsm)

- `dumpty` puts a workspace okit could not serve on the record as an `error`
  record, whether okitd would not start or its dune session was refused, and
  refuses a negative `--timeout`, `0` being how the bound is removed (@avsm)

- The dune RPC client moved to a repository of its own and is now the external
  `dune-rpc-eio` opam package. This repository no longer builds it (@avsm)

- New download target `pro-q2-imatrix-0813`, the PRO 0813 rebuild's 2-bit
  imatrix quant. The `pro-q2` alias moves to it, and the original build stays
  reachable as `pro-q2-imatrix`, deprecated (@avsm)

- Vendor DS4 at 84cc882 (2026-08-09), bringing upstream's Metal and CUDA
  decode and prefill speedups, native MXFP4 inference on CUDA, and the
  server-side parser hardening (@avsm)

- `numpty jobs` no longer lists the running task as due and waiting beside the
  job it already is. A task leaves the due list when it fires and returns at
  the next tick with a fresh next fire time (@avsm)

- `numpty log` says why it printed nothing, naming the journal directory it
  read and whether no run has written to that store or the filters matched
  none of what is there. A `--kind` that no record has is refused with the
  kinds there are, since a misspelling would otherwise match nothing and read
  as an empty journal (@avsm)

- New `numpty status`, `numpty jobs` and `numpty follow`, which ask a running
  daemon over a unix socket at `control` under the store. It answers only what
  the files cannot: which job is running, how full its context is, how far a
  prefill has got, which tool call is in flight, when each task fires next and
  whether the network child is alive. The agent loop publishes an immutable
  snapshot at each state change, so a status is answered while a turn is
  blocked without ever entering the engine. With no daemon listening the three
  say so and print what the journal says the last run was doing (@avsm)

- New `numpty run`, the daemon. It loads the model once, holds the network child
  for the life of the run, and wakes for each task the schedule asks for, one at
  a time. `SIGHUP` rereads the schedule at once, `SIGTERM` lets the turn in
  flight finish and takes the handover before exiting zero, and a network child
  that faulted exits nonzero for a supervisor to restart (@avsm)
- New `numpty task list|check|add|rm|enable|disable|run`, which reads and writes
  the schedule file and needs no daemon. `run` raises a serial rather than
  setting a flag, so a task asked for twice while numpty was stopped fires once
  at startup, and a file that does not parse is left exactly as it was (@avsm)
- New `numpty log` and `numpty memory`, which read the store with the codecs
  that wrote it and print. Both work while numpty runs, while it is stopped and
  on a store copied off the machine. `log` filters on time, kind, task and run,
  and shows a record of a kind it does not know rather than dropping it (@avsm)

- New `numpty once "PROMPT"`, one wake-up on the store with no daemon and no
  socket. It builds a brief from memory, works, asks the agent what should
  survive, and closes the session, and the journal holds every step of it. The
  exit status is nonzero if the network child faulted, since there is no
  respawn and healing is a supervisor's restart (@avsm)
- A wake-up starts from a brief rather than from a conversation. The system
  prompt tells the model that its context does not survive and that its memory
  does, and the first message carries the memory in force, the unfinished work,
  the task a person scheduled and a digest of the journal since the last
  handover. A scheduled task and an open item are kept in separate sections, so
  nothing lets the agent edit its own orders (@avsm)
- A session that fills three quarters of its largest context hands over and a
  fresh one carries on from the new memory, with a `continued` record linking
  the two, so a task spanning five sessions reads as one task in the journal
  (@avsm)

- New `numpty.daemon` library, holding the store a numpty run owns. One run
  takes the lock and a second is refused with the first one's process id, while
  a lock file a crashed run left behind refuses nobody. Opening recovers what a
  crash left: the next sequence and run numbers come from the journal, a memory
  snapshot no record names is removed, one a record does name is adopted, and
  each is said in the journal (@avsm)
- New memory tools, `memory_list`, `memory_read`, `memory_write` and
  `memory_forget`, which are how the agent changes what survives a wake-up.
  Each mutation mints one version and appends one journal record naming both
  versions, the entry and the reason the model gave, so the trace lines up with
  the model's actions one to one (@avsm)

- New `numpty netd`, the child a numpty reaches the network from, and the three
  tools it serves. `fetch` reports a status, the final URL, the content type
  and the byte count before the body, and a non-2xx is reported as itself with
  the server's page after it rather than returned as though it were the
  document. A body over the bound is refused with the size it would have been,
  never cut down to fit. `head` is the same without a body, and `run` runs a
  named program with arguments (@avsm)
- `fetch` takes a `render` of `text` or `raw`. `text` reduces an HTML page to
  the words in it, dropping script and style content, removing tags, decoding
  the common entities and collapsing whitespace. It is a reduction and not a
  renderer, and both the tool description and the interface say so (@avsm)
- New `Numpty_net.Client`, which holds one numptyd for the life of a run.
  There is no respawn, since respawning would fork the process holding the
  model, so a death, a protocol fault or a call over its bound answers at once
  with the tail of numptyd's standard error and every later call answers the
  same. `fault` is how the daemon sees that the run is over, and a stop it
  asked for is not one (@avsm)

- New `numpty` package, a headless agent over the same engine, and its first
  piece: `Numpty_net.Proto`, the line protocol between numpty and the child
  that reaches the network for it. It carries `fetch`, `head` and `run` over
  the framing `Agentkit.Line` supplies (@avsm)

- New `Agentkit.Schedule`, the file a person writes to ask for work. Tasks
  repeat on a duration, fire at a local wall clock time on chosen weekdays, or
  fire once, and a file that does not parse is an error naming the problem
  rather than an empty schedule. Missed fires collapse to one or to none, and
  the rewrite the CLI does renames a complete file over the old one (@avsm)

- New `Agentkit.Memory`, a store of what an agent knows between one context and
  the next. Each mutation mints an immutable version holding the whole set, so
  a version is readable on its own and a forgotten entry is still there at
  every version it was in. A crash leaves either a snapshot the journal never
  named, which recovery removes, or one it did, which recovery adopts (@avsm)

- New `Agentkit.Journal`, an append-only account of what an agent did. One JSON
  record per line, in segments named by the UTC date, with the next sequence
  and run numbers recovered from the last line rather than from a counter file
  that could disagree with it. A record that cannot be written raises, and a
  kind a later build wrote is read back whole rather than dropped (@avsm)

- New `Agent.close`, which releases the session's KV cache at once and leaves
  the agent refusing further calls. A program that works through one agent
  after another no longer waits on the collector to give the memory back
  (@avsm)

- The model catalogue is `Agentkit.Model` rather than `Humpty_cmd.Model`, so a
  command that is not humpty can find a downloaded model without depending on
  the humpty package. `Humpty_cmd` keeps the expect script reader (@avsm)

- New `deepseek.agentkit` library, holding the framing under the line protocols
  in this repository as `Agentkit.Line`. `Okit.Proto` keeps its codecs, and
  `Okit.Proto.max_line` is now `Agentkit.Line.max_line` (@avsm)

- New `edit` tool, which replaces one passage of a file and leaves the rest of
  it alone. In a dune workspace it builds and reports the diagnostics as `write`
  does. A passage that is absent, or that occurs more than once, changes nothing
  and says which it was, rather than editing the first of several look-alikes
  and reporting success (@avsm)

- `write` is described as the tool for creating a file or replacing the whole of
  one, and the system prompt sends a change to part of a file to `edit`. A model
  writing whole files spends its context re-sending the lines it is not
  changing (@avsm)

- The merlin part of the system prompt now names the question rather than the
  tool, and gives `read`, `read_lines` and `grep` their smaller jobs explicitly.
  The base prompt sends the model to `tree`, `grep` and `read_lines`, and a
  preference stated after it lost to the habit of opening the file (@avsm)

- The agent's system prompt and tool descriptions now state a decision rule for
  finding where an identifier is used: `occurrences`, not `grep`, since it knows
  the identifier rather than the text and finds no comment or unrelated name.
  `grep` is described as for text that is not an OCaml identifier.

- Four merlin tools for an agent in a dune workspace. `occurrences` lists every
  use of the name at a position across the workspace, which a search of the
  text cannot do without also matching comments and unrelated names. `errors`
  reports what the compiler makes of one file without building anything.
  `search` finds values by their type, and `complete` lists what can be named
  at a position, which is how to read what an installed library offers when its
  source is not in the workspace (@avsm)

- `locate` answers with the definition it found and not only its address, read
  through the same capability the query was made under. A definition in an
  installed library is named with what to do to reach it, rather than read by
  okitd, which holds the whole of humpty's authority (@avsm)

- `outline` of an implementation names the interface beside it, so a model that
  asked about the `.ml` is told where the module's shape is stated once (@avsm)

- `project` takes a module name and reports the one component that holds it,
  its directory and what it requires. The whole map is cut at a bound, and the
  module lists are the first thing to go, so asking about a module used to mean
  reading a truncated map and then searching for the name (@avsm)

- Tell the agent to learn a module from its `.mli` before its `.ml`, and to
  reach for `outline` to do it. An interface states a module's shape once,
  where the implementation restates it among everything else, so reading the
  implementation first costs several times the context for the same answer
  (@avsm)

- Show how far a prefill has got. A turn that follows a context growth has to
  prefill the whole conversation again, which took minutes with nothing on
  screen but a spinner. The interface now counts the tokens as they go in, and
  `V4.Session.prefill_progress` and `Agent.prefill_progress` report the same
  counters to any caller (@avsm)

- An agent grows its context once a turn no longer leaves room for a full
  reply, rather than once the prompt no longer fits, so a reply near the
  context bound is no longer cut off with no `Agent.Expanded` event. A turn
  that cannot grow any further runs in the room it has and reports a new
  `Agent.Squeezed` event carrying its token budget. The context gauge moves to
  the new window as soon as it grows rather than at the end of the turn (@avsm)

- The dune RPC client is now its own library and opam package, `dune-rpc-eio`,
  so any Eio program can drive a dune server. It is a thin adapter over the
  `dune-rpc` package, as `dune-rpc-lwt` is for Lwt, and exports `Client`,
  `Where` and `connect` rather than a protocol of its own. The hand-written
  s-expression reader, message renderer and packet codecs are gone (@avsm)

- `Dune_rpc_eio.Where.get` and `Where.default` take `build_dir` as an
  `Eio.Path.t` rather than a string, so the build directory is read through a
  capability instead of ambient authority. A path outside that capability is
  refused rather than resolved (@avsm)
- A build directory with no socket now fails with
  `Eio.Io (Eio.Fs.E (Not_found _), _)` where it used to fail with
  `Unix.Unix_error`. Code matching on the old exception must be changed (@avsm)
- `Dune_rpc_eio.connect` resolves the host of an `` `Ip `` address, so a name
  works as well as a literal address, and each address a name resolves to is
  tried in turn. With that the library no longer depends on `unix` or
  `eio.unix` (@avsm)

- The session that spawns or attaches to a dune server is now `Okit.Session`,
  and `Session.start` takes a `?client` naming the caller in the handshake. A
  reply that dune cannot match to a request no longer poisons the session: it
  is reported like any other refusal (@avsm)

- Build an alias with the `build` tool, written as dune's command line writes
  it: `@check` for the alias in a directory and every one below it, `@@check`
  for that directory alone, and a directory in the name as in `@lib/runtest`.
  Aliases and paths mix in one call, and a dep-spec written by hand, such as
  `(alias check)`, is refused with the spelling to write (@avsm)

- Answer a prompt whose first move is a tool call with no arguments. `Dsml`
  refused the very block its own encoder writes for a call with no parameters,
  so an exchange that began with one, `project` in a dune workspace, ended with
  an empty reply (@avsm)
- Report a tool-call block that the decoder refuses, or that the model left
  open, as the text it was. `Dsml.Stream` dropped both, leaving no trace of what
  the model had said (@avsm)
- Keep reading a reply after a refused tool-call block, and make the special
  tokens in the text surfaced from one inert, writing the DSML token as
  `|DSML|` and `</think>` as `[/think]`. An answer the model wrote after such a
  block was discarded, and the block, once recorded in the conversation, was
  read back as markup by every later prompt (@avsm)
- Record an assistant turn, and a tool result, with the model's own markers made
  inert, so a reply or a file that quotes `<｜User｜>`, `</think>` or a piece of
  tool-call markup is read back as the text it was rather than as the shape of
  the conversation. Reading a file that documents DSML no longer puts real
  markup into the next prompt. Both are still shown exactly as they came (@avsm)

- Take prompt lines in a `humpty expect` script, written `? text`. A script with
  a prompt resolves and loads the model, then runs each prompt through the same
  agent loop `humpty agent` runs, printing the model's tool calls as scripted
  calls print and its reply under a `= reply` line. All prompts share one
  conversation, and the seed defaults to a fixed value so that two runs of one
  script sample identically. A script with no prompt loads no model, as before
  (@avsm)
- Bound one prompt's whole exchange, generation and tool calls together, with
  `humpty expect --prompt-timeout` (@avsm)
- Name the prompt form in the message a bad script line is refused with, which
  states in full every form a line may take (@avsm)
- A live Metal test pins that an expect prompt exchange completes against a
  real model (@avsm)

- Run okit in a process of its own, `humpty okitd`, spawned by `humpty agent`
  and `humpty expect` before the model is loaded. Every dune build, merlin query
  and shell command now runs there rather than in the process holding the model,
  so a tool call no longer freezes the interface for minutes while the engine's
  address space is forked. The tools keep their names, arguments and answers,
  and the model sees no difference (@avsm)
- Run `bash` through okit's server too, whenever it is running. It is the same
  tool with the same authority, since the server is humpty's own child, and it
  is granted or withheld exactly as before. A workspace with no `dune-project`
  now has a server for that alone, and its dune tools are absent as they were
  (@avsm)
- Answer `project` from a fresh `dune describe` on every call, so a workspace
  that has changed under a session is reported as it is. The tool no longer says
  it is describing the workspace as it was when the session started (@avsm)
- Say that okit's server has died, and what it last wrote to standard error, as
  the answer to every tool call from then on. There is no respawn, since that
  would fork the process holding the model, and a call that outlasts its bound,
  five minutes for a build, a test or a shell command and one minute for the
  rest, kills the server and degrades the same way. A workspace whose server
  will not start at all keeps every plain tool, `bash` among them, and says why
  (@avsm)
- Stop okit's server, and the dune server it started, when humpty exits by any
  means. The server reads the end of its standard input as a shutdown and
  catches the signal humpty sends when it gives up on one, so even a force-quit
  leaves no dune behind (@avsm)
- Give every program okit runs an empty standard input rather than the pipe the
  protocol arrives on. A `bash` command that reads, such as `cat` or a git that
  prompts, would otherwise have taken the agent's next tool call for its own
  input and left that call unanswered (@avsm)

- Give `humpty agent` dune-backed tools when it is started in an OCaml
  workspace: `build`, `test`, `promote`, `project`, and a `write` that answers
  with what dune then says about the file it wrote, so an edit reports its own
  errors without a build being asked for. They appear when the workspace has a
  `dune-project` and dune 3.24 or later serves it, and the agent keeps the
  plain tools and says why when no session can be started (@avsm)
- Add `outline`, `type_at` and `locate`, which answer from merlin, when
  `ocamlmerlin` is on the PATH (@avsm)
- Tell the agent in its system prompt to start with `project` rather than
  `tree` in a dune workspace, to read the diagnostics `write` returns rather
  than building through `bash`, and to reach for `outline` before reading a
  long file. The paragraph is added only when the tools are there (@avsm)
- Add `humpty.okit`, the library behind those tools. It speaks dune's RPC
  protocol and drives `dune describe` and `ocamlmerlin single` (@avsm)
- Take a dune socket that refuses a connection for one its server left behind
  when it died, and start a server in its place, rather than leaving the dune
  tools off until someone deletes the file. Start that server, and the merlin
  the tools query, without the `INSIDE_DUNE`, `DUNE_BUILD_DIR` and `DUNE_RPC`
  of whatever started humpty, which had sent the server's socket elsewhere
  (@avsm)
- Say in `humpty agent` whether the dune tools are there, and why they are not
  when they are absent, as a note in the interface rather than as a log line
  the interface then covers (@avsm)

- Leave another humpty's dune server running when two of them start on the same
  stale socket and this one's dune lost the race for the build lock. A session
  now owns the server only when the dune it spawned is the one still running
  (@avsm)
- Keep the greeting in `humpty agent` when a note is all that has been said, so
  the banner is still there in a workspace with dune tools (@avsm)
- Report each step okit takes through a `?trace` callback on
  `Okit.Dune_rpc.start`, `Okit.Merlin.find`, `Okit.Project.describe` and
  `Okit.Toolbox.project`, and show the latest of them at the top of the tools
  column of `humpty agent`, so a tool call that has stopped answering says what
  it is waiting on. A step is named before it begins, and the steps taken before
  the interface starts go to standard error as they happen, so the wait for a
  dune socket and the handshake after it are both visible. The four now take a
  final `unit` argument (@avsm)
- Add `?default` to `Dsml.Codec.Invoke.param`, which leaves a parameter out of
  the schema's required list and decodes a call that omits it as the default
  (@avsm)

- Report a failing test from the `test` tool, which had answered `tests ok`
  whatever the tests said. The runtest request named no directory, and dune
  runs no test for an empty list while still reporting success (@avsm)
- Add `humpty expect`, which runs a script of tool calls against the tools
  `humpty agent` would assemble for a workspace and prints what each one
  returned, with no model loaded. The workspace's path prints as `$WS` and
  durations are dropped, so a transcript can be recorded in a test, and
  `--raw` leaves both in. A call that outlasts `--timeout` ends the run
  (@avsm)

- Record what `humpty expect` prints as cram tests in `test/expect`: the dune
  tools of a workspace, the diagnostic a write that breaks the build answers
  with, a failing test and the promotion that fixes it, a workspace with no
  `dune-project`, the refusal when another dune already holds the workspace,
  and the merlin queries (@avsm)

- Name the workspace and the socket under it without a doubled separator in
  okit's errors, which had reported the socket as `/ws//_build/.rpc/dune`
  (@avsm)

- Assemble the dune and merlin tools before `humpty agent` creates the engine,
  rather than after it. Every program okit runs is started by forking humpty,
  and forking one that has a model mapped into it takes minutes on macOS while
  the domain that asked is blocked, which showed as an interface that never
  appeared and a trace that stopped at the dune server (@avsm)
- Answer `project` from a map of the workspace taken while the session was
  starting, and say so on the tool's first line, rather than running `dune
  describe` for every call. That describe forks humpty too, and by then humpty
  holds the model. `Okit.Toolbox.project` takes the map rather than a process
  manager and a root (@avsm)
- Add `live_okit_fork`, a `DS4_LIVE` test that times a fork and exec and a
  `dune describe` in a process that holds an engine (@avsm)

- Name each okit tool and its main argument on the trace as the call begins,
  such as `write: lib/x.ml` or `type_at: lib/x.ml:1:4`, so a call that has not
  answered says which call it is (@avsm)
- Trace that a dune server has been spawned as well as that one is being
  spawned, so a fork that is stuck is told apart from a server that is slow to
  open its socket (@avsm)

- Give up two seconds after the dune `Okit.Dune_rpc.start` spawned exits without
  opening its RPC socket, and say what it said, which for a workspace another
  dune instance already holds names that instance. It used to wait its whole
  window for a socket that could not appear. The window for a dune that is still
  running grows from 10 to 30 seconds, for a machine whose memory a model has
  wired, and each second of the wait is now traced as
  `dune: waiting for socket 5s` (@avsm)

- Add a CUDA backend, `deepseek.cuda`, and the `humpty-cuda` executable that
  links it. Off unless `DS4_CUDA=yes` is set, so a host without a CUDA toolkit
  builds exactly as before (@avsm)
- Vendor upstream's `ds4_cuda.cu`, with its diagnostics routed through the same
  sink as the rest of the engine (@avsm)

- Clear the prompt in `humpty agent` as soon as it is sent, and queue anything
  typed while the model is busy, so a thought can be written down without
  waiting for the reply. A queued prompt is shown where it was typed (@avsm)
- Recall earlier prompts in `humpty agent` with the up and down arrows.
  Whatever was half typed comes back on stepping past the newest (@avsm)
- Lay `humpty agent` out for the width it has, and again when that changes.
  The tool column takes a share of the width rather than a fixed slice, and
  goes entirely below 76 columns rather than starving the conversation, which
  a narrow window had squeezed to a couple of characters. The vitals and the
  key hints drop their least important items instead of being cut off
  mid-word (@avsm)
- Scroll the tool column of `humpty agent` once it fills, rather than letting
  it push the rest of the screen out of the frame, and widen it so an argument
  fits on its line (@avsm)
- List the keys `humpty agent` answers to along the bottom of the screen, each
  set on a block of colour so it reads as a key. Ctrl-C always quits, and
  ctrl-D quits when the prompt is empty, since the field itself takes every
  printable key (@avsm)

- Split the repository into two packages. `deepseek` is the library, and
  `humpty` is the command line and its interface (@avsm)
- Give `humpty agent` a terminal interface, built on Mosaic. The conversation
  is one column and the tool calls another, so machinery no longer interrupts
  the prose. Replies are rendered as markdown, which is what the model writes.
  A line of vitals shows context use as a bar and the token rate as a
  sparkline, and tab shows what each tool returned (@avsm)

- Allow one engine per process, since the backend's kernels are located through
  the process environment and a second engine would race on it. Sessions and
  agents are not limited (@avsm)

- Add the workspace's `AGENTS.md` to the agent's instructions, if it has one,
  and say so in the startup banner (@avsm)
- Add `AGENTS.md` describing the conventions this repository follows, with
  `CLAUDE.md` as a symlink to it (@avsm)

- Report a closed session as an error instead of crashing. The engine reads
  through the pointer without checking, so using one after `V4.Session.close`
  dereferenced null (@avsm)
- Release the session in `V4.generate` when the reply ends, rather than leaving
  its KV cache for the collector (@avsm)
- Recover if an agent cannot allocate a larger context, by returning to the
  size that was working (@avsm)
- Let `Out_of_memory` and `Stack_overflow` out of a tool rather than reporting
  them to the model as ordinary tool failures (@avsm)
- Log when the reasoning effort is reduced because the context is too small
  (@avsm)

- Grow an agent's context when a conversation outgrows it, up to
  `Agent.create ?max_ctx_size`, rather than stopping. The model is not
  reloaded, an `Agent.Expanded` event reports the new size, and the context
  doubles rather than creeping, since each move costs one pass over the
  conversation (@avsm)
- Add `V4.Session.close`, and report a session's KV cache size to the garbage
  collector, so that replacing a session releases the old one promptly instead
  of holding both (@avsm)
- Split the README into a guide to using humpty and `ARCH.md` for maintaining
  it (@avsm)

- Greet with the camel at startup, giving the version, the model target and
  backend, the workspace and its capability, and the tools available. The model
  is named by its target, such as `q4-imatrix-0731`, so which build is loaded
  is plain, and a superseded one is marked (@avsm)
- Draw the camel in orange, leaving its words in the terminal's own colour
  (@avsm)

- Name each model target for the build it carries, and move the short aliases
  onto the newest one. `q4` now means `q4-imatrix-0731`, while the first Flash
  release stays reachable as `q4-imatrix-preview` (@avsm)
- Mark superseded targets as deprecated and dim them in `humpty list`. The PRO
  models are not marked, having had no rebuild (@avsm)

- Tell the agent in its system prompt to mint a capability with `open_dir`
  before using any other file tool, and to prefer the file tools to `bash`.
  Left unsaid, a model reaches for the shell, which needs no capability and so
  never requests one (@avsm)
- Say in `Toolbox.open_dir` that a leading `~` means the home directory, which
  a model otherwise guesses wrongly before falling back to a shell (@avsm)

- Always report an engine error, whatever the verbosity. A fatal diagnostic was
  dropped under `--quiet`, so the process exited without saying why (@avsm)
- Name the model in the error when it cannot be opened, and say that one of
  that size will not load twice at once (@avsm)

- Refuse an absolute path in a filesystem tool and say how to ask for it,
  rather than failing inside Eio and reporting an empty directory. That silence
  was what sent a model looking for a shell instead of requesting access
  (@avsm)
- Let `Toolbox.open_dir` request a directory outside every capability held,
  subject to the `approve` callback given to `Toolbox.Caps.create`. It allows
  everything for now and is where a person would be asked. `humpty agent`
  prints each grant (@avsm)
- Expand a leading `~` in a directory requested through `Toolbox.open_dir`
  (@avsm)
- Lay a reply out beside the camel in `Deepseek.Camel` rather than beneath it,
  with the camel facing its own words (@avsm)

- Add `Deepseek.Camel`, which presents a reply as a camel saying it. `humpty
  agent` uses it for the model's replies (@avsm)
- Mark directories that already have a capability in `Toolbox.list` and
  `Toolbox.tree` output, so it is plain which parts of a tree can be reached
  without opening anything (@avsm)
- Report build directories such as `_build` in `Toolbox.tree` and
  `Toolbox.find` but do not descend into them. A build tree mirrors the source,
  so walking it buried the summary an agent was asking for (@avsm)
- Order `Toolbox.tree` so that names beginning with a dot or an underscore come
  last, leaving the source to be shown first when the entry limit is reached
  (@avsm)

- Name the capabilities an agent holds, and require each filesystem tool call
  to say which one it is using. `Toolbox.open_dir` confines a new capability to
  a directory and returns its name, `Toolbox.caps` lists those held, and a name
  is what `Eio.Path.pp` calls the directory. A capability can only be narrowed,
  since a new one is always taken from one already held (@avsm)
- Add `Toolbox.tree`, which summarises a directory tree to a given depth in one
  call rather than the repeated listings an agent otherwise makes to find its
  way around. Each directory reports how many entries it holds, so one that was
  cut short by the depth or entry limit is distinguishable from an empty one
  (@avsm)

- Add filesystem tools so an agent can explore a tree without a shell:
  `Toolbox.list`, `Toolbox.read_lines`, `Toolbox.find`, `Toolbox.grep` and
  `Toolbox.stat`. Each bounds its own output and reports when it truncates
  (@avsm)
- Confine the agent's filesystem tools with `Eio.Path.with_subtree`, which
  rejects `..` and symlinks leading out of the workspace (@avsm)
- Show context use, token rate and tool calls after each model turn, so that a
  request needing several rounds of tool calls reports as it goes. The figures
  arrive as an `Agent.Stats` event and are also available from `Agent.stats`
  (@avsm)
- Release the OCaml runtime lock while tokenising and sampling. Holding it
  stalled garbage collection on every other domain (@avsm)
- Keep an agent session alive when a turn overruns the context.
  `Agent.send` now raises `Agent.Context_exhausted` and restores the
  conversation, where the process used to exit and lose the loaded model
  (@avsm)
- Cap tool results added to an agent's conversation at 4000 characters, set by
  `Agent.create ?tool_result_limit`. The full result is still passed to
  `on_event` (@avsm)
- Raise the agent's default context to 32768 tokens and add `--ctx` to
  `humpty chat` and `humpty agent` (@avsm)
- Fix a use-after-free when the garbage collector finalized an engine before a
  session belonging to it (@avsm)
- Keep an exception from a `Logs` reporter from propagating through the engine
  and corrupting its state (@avsm)
- Fix the FFI bookkeeping that tracked the runtime lock, which was shared
  between engines and could let one engine call into OCaml without the lock
  (@avsm)
- Report the engine's real size to the garbage collector so that a model is
  released promptly rather than held indefinitely (@avsm)
- Document that a `V4.Session` must be driven by one fiber at a time (@avsm)
- Add model targets for `DeepSeek-V4-Flash-0731`: `q2-imatrix-0731`,
  `q2-q4-imatrix-0731`, `q4-imatrix-0731` with aliases `0731` and `q4-0731`,
  and `mxfp4-0731`. Use `q4-imatrix-0731` on a machine with 256 GB or more
  (@avsm)
- Use the best downloaded 0731 model when no `--model` is given, rather than
  the first GGUF in the data directory (@avsm)
- Describe in the README which model to use for a given amount of memory, and
  how to quantise a new DeepSeek release before a GGUF is published (@avsm)
- Widen the alias column in `humpty list` (@avsm)
- Update the vendored DS4 engine to upstream `54b36ed` of 2026-07-28. This adds
  tensor parallelism, multi-GPU placement, DSpark speculative decoding, batched
  decode and GLM 5.2 support. The OCaml API is unchanged (@avsm)
- Add `csrc/vendor.sh`, which re-vendors the engine from upstream and reapplies
  the local patch that routes engine diagnostics through `Logs` (@avsm)

# v0.1.0

- Initial public release (@avsm)
