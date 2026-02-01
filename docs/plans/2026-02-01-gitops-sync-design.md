# Gitops Library Design

Git-based data sync for bushel and sortal via SSH remotes.

## Goals

- Sync bushel and sortal data directories to deployment hosts using git
- Simple `bushel sync` / `sortal sync` commands (pull then push)
- Shared library with consistent error handling and CLI integration
- Dry-run mode to preview operations
- Verbose logging via Logs/Logs_cli

## Library Structure

```
avsm/gitops/
├── dune-project
├── gitops.opam
└── lib/
    ├── dune
    ├── gitops.ml              # Re-exports
    ├── gitops_repo.ml         # Repo type and path handling
    ├── gitops_cmd.ml          # Git command types
    ├── gitops_exec.ml         # Execution via Eio.Process
    ├── gitops_error.ml        # Eio.Io error types
    ├── gitops_sync.ml         # High-level sync operation
    ├── gitops_sync_config.ml  # TOML codec for [sync]
    └── gitops_sync_cmd.ml     # Cmdliner terms
```

**Dependencies:** eio, tomlt, cmdliner, logs

## Context-Based API

```ocaml
type t = {
  proc_mgr : Eio.Process.mgr;
  fs : Eio.Fs.dir_ty Eio.Path.t;
  dry_run : bool;
}

val v : dry_run:bool -> Eio.Stdenv.t -> t
```

All operations take context first. In dry-run mode, mutating operations log
what they would do. Query operations always execute since control flow may
depend on their results.

## Error Handling

Errors use `Eio.Io` with `Eio.Exn.reraise_with_context`:

```ocaml
type git_error =
  | Exit_code of int
  | Signaled of int
  | Not_a_repo
  | No_remote of string
  | Merge_conflict
  | Nothing_to_commit
  | Push_rejected

type Eio.Exn.err += Git of git_error
```

Each operation adds context, producing readable error chains:

```
Error: Git operation failed (exit code 128)
  fetching from remote "origin"
  running git fetch origin in /home/anil/.local/share/sortal
```

## Core Operations

Query (always execute):
- `rev_parse : t -> repo:path -> string -> string`
- `status : t -> repo:path -> [`Clean | `Dirty]`
- `remote_url : t -> repo:path -> remote:string -> string option`
- `branch_exists : t -> repo:path -> string -> bool`
- `is_repo : t -> repo:path -> bool`

Mutating (logged in dry-run):
- `init : t -> repo:path -> unit`
- `fetch : t -> repo:path -> remote:string -> unit`
- `pull : t -> repo:path -> remote:string -> unit`
- `merge : t -> repo:path -> ref_:string -> unit`
- `add : t -> repo:path -> paths:string list -> unit`
- `add_all : t -> repo:path -> unit`
- `commit : t -> repo:path -> msg:string -> unit`
- `push : t -> repo:path -> remote:string -> unit`
- `remote_add : t -> repo:path -> name:string -> url:string -> unit`
- `clone : t -> url:string -> target:path -> unit`

## Sync Config (TOML)

```toml
[sync]
remote = "ssh://server.example.com/var/data/bushel.git"
branch = "main"
auto_commit = true
commit_message = "sync"
```

```ocaml
type t = {
  remote : string;
  branch : string;
  auto_commit : bool;
  commit_message : string;
}

val codec : t Tomlt.Table.obj_codec
```

## Sync Flow

```ocaml
let run git ~config ~repo =
  (* 1. Ensure repo exists with remote configured *)
  if not (is_repo git ~repo) then init git ~repo;
  ensure_remote git ~repo ~url:config.remote;

  (* 2. Pull: fetch + merge *)
  fetch git ~repo ~remote:"origin";
  let remote_ref = sprintf "origin/%s" config.branch in
  merge git ~repo ~ref_:remote_ref;

  (* 3. Auto-commit local changes *)
  if config.auto_commit && status git ~repo = `Dirty then begin
    add_all git ~repo;
    commit git ~repo ~msg:config.commit_message
  end;

  (* 4. Push *)
  push git ~repo ~remote:"origin"
```

## Cmdliner Integration

```ocaml
val dry_run_term : bool Cmdliner.Term.t
val verbose_term : bool Cmdliner.Term.t
val setup_term : bool Cmdliner.Term.t  (* returns dry_run, sets up logging *)
val remote_term : string option Cmdliner.Term.t
```

## Tool Integration

**Bushel config** (`~/.config/bushel/config.toml`):
```toml
[data]
local_dir = "~/bushel"

[sync]
remote = "ssh://myserver.com/var/data/bushel.git"
```

**Bushel CLI:**
```
bushel sync [--dry-run] [--verbose] [--remote URL]
```

**Sortal** follows same pattern using `Xdge.data_dir` as repo path.

## Implementation Order

1. Create `avsm/gitops` library with core operations
2. Add error handling with Eio.Io context
3. Implement `Gitops.Sync` with config and cmdliner
4. Integrate with bushel
5. Integrate with sortal
6. (Future) Migrate oxmono and sortal_git_store to use gitops
