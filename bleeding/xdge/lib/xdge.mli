(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** XDG Base Directory Specification support with Eio capabilities

    This library provides an OCaml implementation of the XDG Base Directory
    Specification with Eio filesystem integration. The XDG specification defines
    standard locations for user-specific and system-wide application files,
    helping to keep user home directories clean and organized.

    The specification is available at:
    {{:https://specifications.freedesktop.org/basedir-spec/latest/} XDG Base
     Directory Specification}

    {b Key Concepts:}

    The XDG specification defines several types of directories:
    - {b User directories}: Store user-specific files (config, data, cache,
      state, runtime)
    - {b System directories}: Store system-wide files shared across users
    - {b Precedence}: User directories take precedence over system directories
    - {b Application isolation}: Each application gets its own subdirectory

    {b Environment Variable Precedence:}

    This library follows a three-level precedence system:
    + Application-specific variables (e.g., [MYAPP_CONFIG_DIR]) - highest
      priority
    + XDG standard variables (e.g., [XDG_CONFIG_HOME])
    + Default paths (e.g., [$HOME/.config]) - lowest priority

    This allows fine-grained control over directory locations without affecting
    other XDG-compliant applications.

    {b Directory Creation:}

    All directories are automatically created with appropriate permissions
    (0o755) when accessed, except for runtime directories which require stricter
    permissions as per the specification.

    @see <https://specifications.freedesktop.org/basedir-spec/latest/>
      XDG Base Directory Specification

    {2 Related Libraries}

    This library is used by:

    {ul
    {- [Requests] - HTTP client that uses Xdge for cookie persistence paths}
    {- [Cookeio_jar] - Cookie jar with XDG-compliant storage}} *)

type t
(** The main XDG context type containing all directory paths for an application.

    A value of type [t] represents the complete XDG directory structure for a
    specific application, including both user-specific and system-wide
    directories. All paths are resolved at creation time and are absolute paths
    within the Eio filesystem. *)

(** {1 Exceptions} *)

exception Invalid_xdg_path of string
(** Exception raised when XDG environment variables contain invalid paths.

    The XDG specification requires all paths in environment variables to be
    absolute. This exception is raised when a relative path is found. *)

(** {1 Construction} *)

val create : Eio.Fs.dir_ty Eio.Path.t -> string -> t
(** [create fs app_name] creates an XDG context for the given application.

    This function initializes the complete XDG directory structure for your
    application, resolving all paths according to the environment variables and
    creating directories as needed.

    @param fs The Eio filesystem providing filesystem access
    @param app_name The name of your application (used as subdirectory name)

    {b Path Resolution:}

    For each directory type, the following precedence is used:
    + Application-specific environment variable (e.g., [MYAPP_CONFIG_DIR])
    + XDG standard environment variable (e.g., [XDG_CONFIG_HOME])
    + Default path as specified in the XDG specification

    {b Example:}
    {[
      let xdg = Xdge.create env#fs "myapp" in
      let config = Xdge.config_dir xdg in
      (* config is now <fs:$HOME/.config/myapp> or the overridden path *)
    ]}

    All directories are created with permissions 0o755 if they don't exist,
    except for runtime directories which are created with 0o700 permissions and
    validated according to the XDG specification.

    @raise Invalid_xdg_path if any environment variable contains a relative path
*)

(** {1 Accessors} *)

val app_name : t -> string
(** [app_name t] returns the application name used when creating this XDG
    context.

    This is the name that was passed to {!create} and is used as the
    subdirectory name within each XDG base directory. *)

(** {1 Base Directories} *)

val config_dir : t -> Eio.Fs.dir_ty Eio.Path.t
(** [config_dir t] returns the path to user-specific configuration files.

    {b Purpose:} Store user preferences, settings, and configuration files.
    Configuration files should be human-readable when possible.

    {b Environment Variables:}
    - [${APP_NAME}_CONFIG_DIR]: Application-specific override (highest priority)
    - [XDG_CONFIG_HOME]: XDG standard variable
    - Default: [$HOME/.config/{app_name}]

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_CONFIG_HOME specification *)

val data_dir : t -> Eio.Fs.dir_ty Eio.Path.t
(** [data_dir t] returns the path to user-specific data files.

    {b Purpose:} Store persistent application data that should be preserved
    across application restarts and system reboots. This data is typically not
    modified by users directly.

    {b Environment Variables:}
    - [${APP_NAME}_DATA_DIR]: Application-specific override (highest priority)
    - [XDG_DATA_HOME]: XDG standard variable
    - Default: [$HOME/.local/share/{app_name}]

    {b Example Files:}
    - Application databases
    - User-generated content (documents, projects)
    - Downloaded resources
    - Application plugins or extensions

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_DATA_HOME specification *)

val cache_dir : t -> Eio.Fs.dir_ty Eio.Path.t
(** [cache_dir t] returns the path to user-specific cache files.

    {b Purpose:} Store non-essential cached data that can be regenerated if
    deleted. The application should remain functional if this directory is
    cleared, though performance may be temporarily impacted.

    {b Environment Variables:}
    - [${APP_NAME}_CACHE_DIR]: Application-specific override (highest priority)
    - [XDG_CACHE_HOME]: XDG standard variable
    - Default: [$HOME/.cache/{app_name}]

    {b Example Files:}
    - Downloaded thumbnails and previews
    - Compiled bytecode or object files
    - Network response caches
    - Temporary computation results

    Users may clear cache directories to free disk space, so always check for
    cache validity and be prepared to regenerate data.

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_CACHE_HOME specification *)

val state_dir : t -> Eio.Fs.dir_ty Eio.Path.t
(** [state_dir t] returns the path to user-specific state files.

    {b Purpose:} Store persistent state data that should be preserved between
    application restarts but is not important enough to be user data. This
    includes application state that can be regenerated but would impact the user
    experience if lost.

    {b Environment Variables:}
    - [${APP_NAME}_STATE_DIR]: Application-specific override (highest priority)
    - [XDG_STATE_HOME]: XDG standard variable
    - Default: [$HOME/.local/state/{app_name}]

    {b Example Files:}
    - Application history (recently used files, command history)
    - Current application state (window positions, open tabs)
    - Logs and journal files
    - Undo/redo history

    {b Comparison with other directories:}
    - Unlike cache: State should persist between reboots
    - Unlike data: State can be regenerated (though inconvenient)
    - Unlike config: State changes frequently during normal use

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_STATE_HOME specification *)

val runtime_dir : t -> Eio.Fs.dir_ty Eio.Path.t option
(** [runtime_dir t] returns the path to user-specific runtime files.

    {b Purpose:} Store runtime files such as sockets, named pipes, and process
    IDs. These files are only valid for the duration of the user's login
    session.

    {b Environment Variables:}
    - [${APP_NAME}_RUNTIME_DIR]: Application-specific override (highest
      priority)
    - [XDG_RUNTIME_DIR]: XDG standard variable
    - Default: None (returns [None] if not set)

    {b Required Properties (per specification):}
    - Owned by the user with access mode 0700
    - Bound to the user login session lifetime
    - Located on a local filesystem (not networked)
    - Fully-featured by the OS (supporting proper locking, etc.)

    {b Example Files:}
    - Unix domain sockets
    - Named pipes (FIFOs)
    - Lock files
    - Small process communication files

    This may return [None] if no suitable runtime directory is available.
    Applications should handle this gracefully, perhaps by falling back to
    [/tmp] with appropriate security measures.

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_RUNTIME_DIR specification *)

(** {1 System Directories} *)

val config_dirs : t -> Eio.Fs.dir_ty Eio.Path.t list
(** [config_dirs t] returns search paths for system-wide configuration files.

    {b Purpose:} Provide a search path for configuration files that are shared
    between multiple users. Files in user-specific {!config_dir} take precedence
    over these system directories.

    {b Environment Variables:}
    - [${APP_NAME}_CONFIG_DIRS]: Application-specific override (highest
      priority)
    - [XDG_CONFIG_DIRS]: XDG standard variable (colon-separated list)
    - Default: [[/etc/xdg/{app_name}]]

    {b Search Order:} Directories are ordered by preference, with earlier
    entries taking precedence over later ones. When looking for a configuration
    file, search {!config_dir} first, then each directory in this list.

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_CONFIG_DIRS specification *)

val data_dirs : t -> Eio.Fs.dir_ty Eio.Path.t list
(** [data_dirs t] returns search paths for system-wide data files.

    {b Purpose:} Provide a search path for data files that are shared between
    multiple users. Files in user-specific {!data_dir} take precedence over
    these system directories.

    {b Environment Variables:}
    - [${APP_NAME}_DATA_DIRS]: Application-specific override (highest priority)
    - [XDG_DATA_DIRS]: XDG standard variable (colon-separated list)
    - Default: [[/usr/local/share/{app_name}; /usr/share/{app_name}]]

    {b Search Order:} Directories are ordered by preference, with earlier
    entries taking precedence over later ones. When looking for a data file,
    search {!data_dir} first, then each directory in this list.

    {b Example Files:}
    - Application icons and themes
    - Desktop files
    - Shared application resources
    - Documentation files
    - Default templates

    @see <https://specifications.freedesktop.org/basedir-spec/latest/#variables>
      XDG_DATA_DIRS specification *)

(** {1 File Search} *)

val find_config_file : t -> string -> Eio.Fs.dir_ty Eio.Path.t option
(** [find_config_file t filename] searches for a configuration file following
    XDG precedence.

    This function searches for the given filename in the user configuration
    directory first, then in system configuration directories in order of
    preference. Files that are inaccessible (due to permissions, non-existence,
    etc.) are silently skipped as per the XDG specification.

    @param t The XDG context
    @param filename The name of the file to search for
    @return [Some path] if found, [None] if not found in any directory

    {b Search Order:} 1. User config directory ({!config_dir}) 2. System config
    directories ({!config_dirs}) in preference order *)

val find_data_file : t -> string -> Eio.Fs.dir_ty Eio.Path.t option
(** [find_data_file t filename] searches for a data file following XDG
    precedence.

    This function searches for the given filename in the user data directory
    first, then in system data directories in order of preference. Files that
    are inaccessible (due to permissions, non-existence, etc.) are silently
    skipped as per the XDG specification.

    @param t The XDG context
    @param filename The name of the file to search for
    @return [Some path] if found, [None] if not found in any directory

    {b Search Order:} 1. User data directory ({!data_dir}) 2. System data
    directories ({!data_dirs}) in preference order *)

(** {1 Pretty Printing} *)

val pp : ?brief:bool -> ?sources:bool -> Format.formatter -> t -> unit
(** [pp ?brief ?sources ppf t] pretty prints the XDG directory configuration.

    @param brief If [true], prints a compact one-line summary (default: [false])
    @param sources
      If [true], shows the source of each directory value, indicating whether it
      came from defaults, environment variables, or command line (default:
      [false])
    @param ppf The formatter to print to
    @param t The XDG context to print

    {b Output formats:}
    - Normal: Multi-line detailed view of all directories
    - Brief: Single line showing app name and key directories
    - With sources: Adds annotations showing where each path came from *)

(** {1 Cmdliner Integration} *)

module Cmd : sig
  (** The type of the outer XDG context *)
  type xdg_t = t
  (** Cmdliner integration for XDG directory configuration.

      This module provides integration with the Cmdliner library, allowing XDG
      directories to be configured via command-line arguments while respecting
      the precedence of environment variables. *)

  type t
  (** Type of XDG configuration gathered from command-line and environment.

      This contains all XDG directory paths along with their sources, as
      determined by command-line arguments and environment variables. *)

  type dir =
    [ `Config  (** User configuration files *)
    | `Cache  (** User-specific cached data *)
    | `Data  (** User-specific application data *)
    | `State  (** User-specific state data (logs, history, etc.) *)
    | `Runtime  (** User-specific runtime files (sockets, pipes, etc.) *) ]
  (** XDG directory types for specifying which directories an application needs.

      These allow applications to declare which XDG directories they use,
      enabling runtime systems to only provide the requested directories. *)

  val term :
    string ->
    Eio.Fs.dir_ty Eio.Path.t ->
    ?dirs:dir list ->
    unit ->
    (xdg_t * t) Cmdliner.Term.t
  (** [term app_name fs ?dirs ()] creates a Cmdliner term for XDG directory
      configuration.

      This function generates a Cmdliner term that handles XDG directory
      configuration through both command-line flags and environment variables,
      and directly returns the XDG context. Only command-line flags for the
      requested directories are generated.

      @param app_name
        The application name (used for environment variable prefixes)
      @param fs The Eio filesystem to use for path resolution
      @param dirs
        List of directories to include flags for (default: all directories)

      {b Generated Command-line Flags:} Only the flags for requested directories
      are generated:
      - [--config-dir DIR]: Override configuration directory (if [`Config] in
        dirs)
      - [--data-dir DIR]: Override data directory (if [`Data] in dirs)
      - [--cache-dir DIR]: Override cache directory (if [`Cache] in dirs)
      - [--state-dir DIR]: Override state directory (if [`State] in dirs)
      - [--runtime-dir DIR]: Override runtime directory (if [`Runtime] in dirs)

      {b Environment Variable Precedence:} For each directory type, the
      following precedence applies:
      + Command-line flag (e.g., [--config-dir]) - if enabled
      + Application-specific variable (e.g., [MYAPP_CONFIG_DIR])
      + XDG standard variable (e.g., [XDG_CONFIG_HOME])
      + Default value *)

  val cache_term : string -> string Cmdliner.Term.t
  (** [cache_term app_name] creates a Cmdliner term that provides just the cache
      directory path as a string, respecting XDG precedence.

      This is a convenience function for applications that only need cache
      directory configuration. It returns the resolved cache directory path
      directly as a string, suitable for use in other Cmdliner terms.

      @param app_name
        The application name (used for environment variable prefixes)

      {b Generated Command-line Flag:}
      - [--cache-dir DIR]: Override cache directory

      {b Environment Variable Precedence:}
      + Command-line flag ([--cache-dir])
      + Application-specific variable (e.g., [MYAPP_CACHE_DIR])
      + XDG standard variable ([XDG_CACHE_HOME])
      + Default value ([$HOME/.cache/{app_name}]) *)

  val env_docs : string -> string
  (** [env_docs app_name] generates documentation for environment variables.

      Returns a formatted string documenting all environment variables that
      affect XDG directory configuration for the given application. This is
      useful for generating man pages or help text.

      @param app_name The application name
      @return A formatted documentation string

      {b Included Information:}
      - Configuration precedence rules
      - Application-specific environment variables
      - XDG standard environment variables
      - Default values for each directory type *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf config] pretty prints a Cmdliner configuration.

      This function formats the configuration showing each directory path along
      with its source, which is helpful for debugging configuration issues or
      displaying the current configuration to users.

      @param ppf The formatter to print to
      @param config The configuration to print *)
end
