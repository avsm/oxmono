(** Plugins receive only capabilities supplied by their owner. *)

type t = { name : string; description : string; run : query:string -> string }

val with_workspace :
  sw:Eio.Switch.t ->
  profile_dir:_ Eio.Path.t ->
  (Eio.Fs.dir_ty Eio.Path.t -> t) ->
  t
(** [with_workspace ~sw ~profile_dir build] constructs a file plugin with an Eio
    cwd confined to the profile's private [workspace/] subtree. Traversal and
    symlink escapes are refused, and a symlink used as the workspace is
    rejected. The plugin lives under [sw]. It receives no filesystem root,
    process manager, environment or credentials. File plugins should use this
    factory and Eio paths, not native paths or process-wide [chdir]. Compiled
    plugin code remains trusted; this does not sandbox arbitrary native code.
    Tools without file operations need no workspace capability. *)

val tool : t -> Openrouter.Tool.t

val invoke : t -> string -> string
(** [invoke plugin arguments] validates the JSON query argument and bounds tool
    results to 4096 bytes plus a truncation marker. *)

val invoke_result : t -> string -> (string, string) result
(** [invoke_result plugin arguments] distinguishes rejected arguments from a
    successful result for the tool-use log. Plugin exceptions propagate. *)

val clip : bytes:int -> string -> string
