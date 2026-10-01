# Tool needs for faster OCaml repository navigation

This document records gaps in the agent's tools that slow navigation of an
OCaml repository, with the tool that would close each gap. The aim is fewer
tool invocations per question and less context spent per answer.

The current toolset gives the model the shape of files (`outline`, `tree`,
`project`) and the location of things (`locate`, `grep`, `find`). It does not
give the content at that location, and it does not use merlin's OCaml-aware
queries beyond outline, type_at and locate. The suggestions below are ordered
by value.

## 1. `locate` should return the definition text

The most common navigation pattern is "I see `foo` here, what is it?"
Currently that takes three calls: `locate` to find `file:line:col`, `open_dir`
to reach the file's directory, and `read_lines` to see the definition.

Make `locate` return the source text at the found position alongside the
location. Merlin's `locate` query already knows the exact span, so the tool
can read the file through the capability and include the lines. This collapses
`locate` + `open_dir` + `read` into one call.

## 2. New `occurrences` tool for finding all uses of a name

The second most common pattern is "where is `foo` used?" Currently that is a
`grep` across the whole tree, which returns noise from comments, unrelated
identifiers, and files outside the compilation. Merlin's `occurrences -scope
project` answers this precisely: it knows the exact identifier and returns
every use with file and position.

This is a new okit merlin tool. It takes `(cap, path, line, col)` like
`locate`, runs merlin's `occurrences -scope project`, and returns a list of
`file:line:col` for every use of the identifier at that position.

## 3. New `errors` tool for type checking without building

Sometimes the question is "does this file type-check?" Building the workspace
is slow, and reading the whole file to spot errors is context-expensive.
Merlin's `errors` query answers in one exchange: it type-checks the file and
returns every error with position and message.

This is a new okit merlin tool. It takes `(cap, path)` like `outline`, runs
merlin's `errors`, and returns the list of errors with position and message.

## 4. New `search_by_type` tool for finding functions by signature

OCaml code is type-driven. A common need is "find a function that maps over a
list with an int-to-string transformation." Currently that is a `grep` for
likely names followed by `type_at` on each candidate.

Merlin's `search-by-type` query finds functions by type signature. The tool
takes `(cap, path, line, col, query, limit)` where the position supplies the
environment to search from and the query is a type expression such as
`int -> string`. It returns name, file, type, and a constructible expression
for each match.

## 5. New `jump` tool for crossing the interface boundary

A common navigation pattern is "I am looking at `foo.ml` and want its
interface." Currently that is `find` to locate the `.mli`, `open_dir` if
needed, and `read`. Merlin's `locate -look-for interface` can find the
interface directly.

The tool takes `(cap, path)` and returns the path of the sibling `.mli` (or
`.ml` when given a `.mli`). If the sibling does not exist, it says so.

## 6. `project` should answer "which library owns this module?"

`project` lists each library with its modules, but the output is truncated at
about 8 KB, and for a large workspace the module lists are the first thing
cut. When the question is "which library owns `Foo`", the model has to run
`project`, see a truncated list, then `grep` for the module name.

Add a `module` query to the project tool, or a separate tool, that takes a
module name and returns which library or executable owns it, its source
files, and the component's dependencies.

## 7. Batch merlin queries per file

The merlin tools read the file through the capability and send its full text
to okitd on every call. A 10,000-line file is sent every time the model asks
about it, and asking outline then type_at then locate on the same file sends
it three times.

Add a `batch` op that takes a file and a list of queries (outline, type_at,
locate, occurrences) and answers all of them in one exchange. The source is
sent once and the answers come back together.

## 8. New `complete_prefix` tool for discovering available functions

When exploring an unfamiliar library or module, a common need is "what
functions are available here?" `outline` on the `.mli` covers this for files
in the workspace, but merlin's `complete-prefix` also completes inside an
expression context, suggesting what is available at a specific point in a
file, including from the current module and its opens.

This is a new okit merlin tool. It takes `(cap, path, line, col, prefix)` and
returns name, kind, and type for each completion.

## Where each tool lives

Tools that run programs or need merlin go in `okit/toolbox.ml` with a
matching `Proto.op` and a dispatch arm in `okit/server.ml`. Tools that only
read the filesystem through a capability go in `lib/toolbox.ml`. The client
side of a new okit tool is a `Tool.v` with a `Dsml.Codec.Invoke` parameter
list and a handler that calls `ask client ~timeout:short (Proto.New_op ...)`.

The merlin queries are already implemented in `okit/merlin.ml` for outline,
type_at, locate, and errors. `occurrences`, `search-by-type`, and
`complete-prefix` are new merlin queries to add there, following the pattern
of the existing ones: build the argument list, call `query`, and decode the
JSON reply.

The `Proto.op` type, its encoder and decoder in `okit/proto.ml`, and the
dispatch arm in `okit/server.ml` all need a new case for each new tool.
