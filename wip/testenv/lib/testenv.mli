(** Scoped process environment changes for single-domain tests.

    Changing the process environment while another domain reads it is a data
    race. Every function in this module must be called from the main domain
    while no other domain is running, and before anything that reads the
    environment from another domain starts. *)

type binding = string * string option
(** The type for environment bindings. A binding of [None] unsets the variable. *)

val set : string -> string option -> unit
(** [set name value] binds [name] to [value] in the process environment, or
    unsets [name] if [value] is [None].

    @raise Invalid_argument if [name] or [value] contains a null byte. *)

val with_bindings : binding list -> (unit -> 'a) -> 'a
(** [with_bindings bindings f] is [f ()] with each binding in [bindings] applied
    in order. The previous values are restored, or the variables unset, when [f]
    returns or raises. *)

val with_xdg : ?prefix:string -> (string -> 'a) -> 'a
(** [with_xdg f] is [f root], where [root] is a fresh temporary directory bound
    as the home and XDG base directories for the call. [HOME] is [root], and
    [XDG_CONFIG_HOME], [XDG_DATA_HOME], [XDG_CACHE_HOME], [XDG_STATE_HOME] and
    [XDG_RUNTIME_DIR] are the created subdirectories [config], [data], [cache],
    [state] and [runtime] of [root]. The runtime directory has mode [0o700].
    [XDG_CONFIG_DIRS] and [XDG_DATA_DIRS] are unset. The previous environment is
    restored and [root] is deleted when [f] returns or raises. [prefix] names
    the temporary directory and defaults to ["testenv-"]. *)

val xdg_variables : string list
(** [xdg_variables] is the list of variables [with_xdg] binds. *)
