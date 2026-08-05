# xdge - XDG Base Directory Specification for Eio

This library implements the [XDG Base Directory
Specification](https://specifications.freedesktop.org/basedir-spec/latest/) for
OCaml applications using the [Eio](https://github.com/ocaml-multicore/eio)
effects-based I/O library.

## What is XDG?

The XDG Base Directory Specification defines standard locations for user-specific files on Unix-like systems, keeping home directories clean and organized:

- Config (`~/.config/app`): User preferences and settings
- Data (`~/.local/share/app`): Persistent application data
- Cache (`~/.cache/app`): Non-essential cached data (safe to delete)
- State (`~/.local/state/app`): Logs, history, and runtime state
- Runtime (`$XDG_RUNTIME_DIR/app`): Sockets, pipes, and session-bound files

The specification also defines system-wide search paths (`/etc/xdg`,
`/usr/share`) and a precedence system using environment variables
(`XDG_CONFIG_HOME`, `XDG_DATA_HOME`, and so on).

## Why Eio?

Eio uses a **capability-based** approach to I/O where filesystem access must be
explicitly passed to functions. This design aligns naturally with XDG directory
management. For example:

```ocaml
(* Filesystem access is an explicit capability *)
let xdg = Xdge.create env#fs "myapp"
```

The capability model provides the benefit that code that needs filesystem
access must receive the `fs` capability, with no hidden global state or ambient
authority. The `Eio.Path.t` type returned by xdge encapsulates both the
filesystem capability and the path, preventing path traversal outside the
granted capability. Applications can restrict filesystem access by passing a
sandboxed `fs` capability, and xdge respects those boundaries.

## Usage

```ocaml
Eio_main.run @@ fun env ->
  let xdg = Xdge.create env#fs "myapp" in

  (* Access XDG directories as Eio paths *)
  let config = Xdge.config_dir xdg in
  let data = Xdge.data_dir xdg in

  (* Search for files following XDG precedence *)
  match Xdge.find_config_file xdg "settings.json" with
  | Some path -> (* use path *)
  | None -> (* use defaults *)
```

For CLI applications, xdge provides Cmdliner terms that handle environment
variable precedence and command-line overrides:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let term = Xdge.Cmd.term "myapp" env#fs () in
  (* Generates --config-dir, --data-dir, etc. flags *)
  (* Respects MYAPP_CONFIG_DIR > XDG_CONFIG_HOME > default *)
```

## Installation

```
opam install xdge
```

## License

ISC
