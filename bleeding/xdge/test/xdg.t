Test with default directories:

  $ export HOME=./test_home
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./test_home/.config/xdg_example> [default]
    data: <fs:./test_home/./test_home/.local/share/xdg_example> [default]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

No command-line args or env vars are set, so all directories use defaults.
Config shows empty (no overrides), and directories show [default] source.  User
directories follow XDG spec: ~/.config, ~/.local/share, ~/.cache,
~/.local/state.  Runtime dir is <none> since XDG_RUNTIME_DIR has no default.
System dirs use XDG spec defaults: /etc/xdg for config, /usr/{local/,}share for
data.

Test with all command line arguments specified
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ ./xdg_example.exe \
  >   --config-dir ./test-config \
  >   --data-dir ./test-data \
  >   --cache-dir ./test-cache \
  >   --state-dir ./test-state \
  >   --runtime-dir ./test-runtime
  === Cmdliner Config ===
  XDG config:
  config_dir: ./test-config [cmdline]
  data_dir: ./test-data [cmdline]
  cache_dir: ./test-cache [cmdline]
  state_dir: ./test-state [cmdline]
  runtime_dir: ./test-runtime [cmdline]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./test-config> [cmdline]
    data: <fs:./test_home/./test-data> [cmdline]
    cache: <fs:./test_home/./test-cache> [cmdline]
    state: <fs:./test_home/./test-state> [cmdline]
    runtime: <fs:./test_home/./test-runtime> [cmdline]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

All user directories are overridden by command-line arguments, showing
[cmdline] as the source. The config section shows all overrides with their
values and [cmdline] sources. System directories remain at their defaults since
they cannot be overridden by user directories command-line options.

Test with environment variables (app-specific)
  $ XDG_EXAMPLE_CONFIG_DIR=./env-config \
  > XDG_EXAMPLE_DATA_DIR=./env-data \
  > XDG_EXAMPLE_CACHE_DIR=./env-cache \
  > XDG_EXAMPLE_STATE_DIR=./env-state \
  > XDG_EXAMPLE_RUNTIME_DIR=./env-runtime \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  config_dir: ./env-config [env(XDG_EXAMPLE_CONFIG_DIR)]
  data_dir: ./env-data [env(XDG_EXAMPLE_DATA_DIR)]
  cache_dir: ./env-cache [env(XDG_EXAMPLE_CACHE_DIR)]
  state_dir: ./env-state [env(XDG_EXAMPLE_STATE_DIR)]
  runtime_dir: ./env-runtime [env(XDG_EXAMPLE_RUNTIME_DIR)]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./env-config> [env(XDG_EXAMPLE_CONFIG_DIR)]
    data: <fs:./test_home/./env-data> [env(XDG_EXAMPLE_DATA_DIR)]
    cache: <fs:./test_home/./env-cache> [env(XDG_EXAMPLE_CACHE_DIR)]
    state: <fs:./test_home/./env-state> [env(XDG_EXAMPLE_STATE_DIR)]
    runtime: <fs:./test_home/./env-runtime> [env(XDG_EXAMPLE_RUNTIME_DIR)]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

App-specific environment variables (XDG_EXAMPLE_*) override the defaults. The
source correctly shows [env(XDG_EXAMPLE_*)] for each variable.  These
app-specific variables take precedence over XDG standard variables when both
are available, allowing per-application customization.

Test with standard XDG environment variables:

  $ XDG_CONFIG_HOME=/tmp/xdge/xdg-config \
  > XDG_DATA_HOME=/tmp/xdge/xdg-data \
  > XDG_CACHE_HOME=/tmp/xdge/xdg-cache \
  > XDG_STATE_HOME=/tmp/xdge/xdg-state \
  > XDG_RUNTIME_DIR=/tmp/xdge/xdg-runtime \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  config_dir: /tmp/xdge/xdg-config [env(XDG_CONFIG_HOME)]
  data_dir: /tmp/xdge/xdg-data [env(XDG_DATA_HOME)]
  cache_dir: /tmp/xdge/xdg-cache [env(XDG_CACHE_HOME)]
  state_dir: /tmp/xdge/xdg-state [env(XDG_STATE_HOME)]
  runtime_dir: /tmp/xdge/xdg-runtime [env(XDG_RUNTIME_DIR)]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:/tmp/xdge/xdg-config> [env(XDG_CONFIG_HOME)]
    data: <fs:/tmp/xdge/xdg-data> [env(XDG_DATA_HOME)]
    cache: <fs:/tmp/xdge/xdg-cache> [env(XDG_CACHE_HOME)]
    state: <fs:/tmp/xdge/xdg-state> [env(XDG_STATE_HOME)]
    runtime: <fs:/tmp/xdge/xdg-runtime> [env(XDG_RUNTIME_DIR)]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

Standard XDG environment variables (XDG_*_HOME, XDG_RUNTIME_DIR) override the
defaults. The source correctly shows [env(XDG_*)] for each variable.  Note that
the user directories use the raw paths from env vars (not app-specific subdirs)
since XDG_CONFIG_HOME etc. are intended to be the base directories for the
user.

Test command line overrides environment variables:

  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ XDG_EXAMPLE_CONFIG_DIR=./env-config \
  > ./xdg_example.exe --config-dir ./cli-config
  === Cmdliner Config ===
  XDG config:
  config_dir: ./cli-config [cmdline]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./cli-config> [cmdline]
    data: <fs:./test_home/./test_home/.local/share/xdg_example> [default]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

Command-line arguments have highest precedence, overriding environment
variables. Only config_dir is shown in the config section since it is the only
one explicitly set. The config_dir shows [cmdline] source while other
directories fall back to defaults, demonstrating the precedence hierarchy: of
cmdline then app env vars then XDG env vars then defaults.

Test mixed environment variable precedence (app-specific overrides XDG
standard):

  $ export HOME=./test_home
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ XDG_CONFIG_HOME=/tmp/xdge/xdg-config \
  > XDG_EXAMPLE_CONFIG_DIR=./app-config \
  > XDG_DATA_HOME=/tmp/xdge/xdg-data \
  > XDG_EXAMPLE_DATA_DIR=./app-data \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  config_dir: ./app-config [env(XDG_EXAMPLE_CONFIG_DIR)]
  data_dir: ./app-data [env(XDG_EXAMPLE_DATA_DIR)]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./app-config> [env(XDG_EXAMPLE_CONFIG_DIR)]
    data: <fs:./test_home/./app-data> [env(XDG_EXAMPLE_DATA_DIR)]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

Demonstrates app-specific environment variables taking precedence over XDG
standard ones. Both XDG_CONFIG_HOME and XDG_EXAMPLE_CONFIG_DIR are set, but the
app-specific one wins. Same for data directories. Cache, state, and runtime
fall back to defaults since no variables are set for them.

Test partial environment variable override:

  $ export HOME=./test_home
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ XDG_EXAMPLE_CONFIG_DIR=./app-config \
  > XDG_DATA_HOME=/tmp/xdge/xdg-data \
  > XDG_CACHE_HOME=/tmp/xdge/xdg-cache \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  config_dir: ./app-config [env(XDG_EXAMPLE_CONFIG_DIR)]
  data_dir: /tmp/xdge/xdg-data [env(XDG_DATA_HOME)]
  cache_dir: /tmp/xdge/xdg-cache [env(XDG_CACHE_HOME)]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./app-config> [env(XDG_EXAMPLE_CONFIG_DIR)]
    data: <fs:/tmp/xdge/xdg-data> [env(XDG_DATA_HOME)]
    cache: <fs:/tmp/xdge/xdg-cache> [env(XDG_CACHE_HOME)]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

Shows mixed sources working together. Config uses app-specific env var (highest
priority among env vars), data and cache use XDG standard env vars (no
app-specific ones set), and state uses default (no env vars set). Each
directory gets its value from the highest-priority available source.

Test command line overrides mixed environment variables:

  $ export HOME=./test_home
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ XDG_CONFIG_HOME=/tmp/xdge/xdg-config \
  > XDG_EXAMPLE_CONFIG_DIR=./app-config \
  > ./xdg_example.exe --config-dir ./cli-config
  === Cmdliner Config ===
  XDG config:
  config_dir: ./cli-config [cmdline]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./cli-config> [cmdline]
    data: <fs:./test_home/./test_home/.local/share/xdg_example> [default]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

Command-line argument overrides both types of environment variables. Even
though both XDG_CONFIG_HOME and XDG_EXAMPLE_CONFIG_DIR are set, the
--config-dir flag takes precedence and shows [cmdline] source. Other
directories fall back to defaults since no other command-line args are
provided.

Test empty environment variable handling:
  $ export HOME=./test_home
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ XDG_EXAMPLE_CONFIG_DIR="" \
  > XDG_CONFIG_HOME=/tmp/xdge/xdg-config \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  config_dir: /tmp/xdge/xdg-config [env(XDG_CONFIG_HOME)]
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:/tmp/xdge/xdg-config> [env(XDG_CONFIG_HOME)]
    data: <fs:./test_home/./test_home/.local/share/xdg_example> [default]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/etc/xdg/xdg_example>]
    data_dirs: [<fs:/usr/local/share/xdg_example>; <fs:/usr/share/xdg_example>]

When an app-specific env var is empty (""), it falls back to the XDG standard
variable. XDG_EXAMPLE_CONFIG_DIR="" is ignored, so XDG_CONFIG_HOME is used
instead, correctly showing [env(XDG_CONFIG_HOME)] as the source.  This behavior
ensures that empty app-specific variables do not override useful XDG standard
settings.

Test system directory environment variables:

  $ export HOME=./test_home
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ XDG_CONFIG_DIRS=/tmp/xdge/sys1:/tmp/xdge/sys2 \
  > XDG_DATA_DIRS=/tmp/xdge/data1:/tmp/xdge/data2 \
  > ./xdg_example.exe
  === Cmdliner Config ===
  XDG config:
  
  === XDG Directories ===
  XDG directories for 'xdg_example':
  User directories:
    config: <fs:./test_home/./test_home/.config/xdg_example> [default]
    data: <fs:./test_home/./test_home/.local/share/xdg_example> [default]
    cache: <fs:./test_home/./test_home/.cache/xdg_example> [default]
    state: <fs:./test_home/./test_home/.local/state/xdg_example> [default]
    runtime: <none> [default]
  System directories:
    config_dirs: [<fs:/tmp/xdge/sys1/xdg_example>;
                  <fs:/tmp/xdge/sys2/xdg_example>]
    data_dirs: [<fs:/tmp/xdge/data1/xdg_example>;
                <fs:/tmp/xdge/data2/xdg_example>]

XDG_CONFIG_DIRS and XDG_DATA_DIRS environment variables override the default
system directories. The colon-separated paths are parsed and the app name is
appended to each path. User directories remain at defaults since no user-level
overrides are provided. System directory env vars only affect the system
directories, not user directories.

Test _path functions do not create directories but can access files within them:

  $ export HOME=/tmp/xdge/xdg_path_test
  $ mkdir -p /tmp/xdge/xdg_path_test
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
Create config subdirectory manually and write a test file:
  $ mkdir -p "/tmp/xdge/xdg_path_test/.config/path_test/profiles"
  $ echo "test profile content" > "/tmp/xdge/xdg_path_test/.config/path_test/profiles/default.json"
Create data subdirectory manually and write a test file:
  $ mkdir -p "/tmp/xdge/xdg_path_test/.local/share/path_test/databases"
  $ echo "test database content" > "/tmp/xdge/xdg_path_test/.local/share/path_test/databases/main.db"
Create cache subdirectory manually and write a test file:
  $ mkdir -p "/tmp/xdge/xdg_path_test/.cache/path_test/thumbnails"
  $ echo "test cache content" > "/tmp/xdge/xdg_path_test/.cache/path_test/thumbnails/thumb1.png"
Create state subdirectory manually and write a test file:  
  $ mkdir -p "/tmp/xdge/xdg_path_test/.local/state/path_test/logs"
  $ echo "test log content" > "/tmp/xdge/xdg_path_test/.local/state/path_test/logs/app.log"

Now test that we can read the files through the XDG _path functions:
  $ ./test_paths.exe
  config file content: test profile content
  data file content: test database content
  cache file content: test cache content
  state file content: test log content

This test verifies that the _path functions return correct paths that can be used to access
files within XDG subdirectories, without the functions automatically creating those directories.

Test path resolution with --show-paths:

Test with a preset HOME to verify correct path resolution:
  $ export HOME=./home_testuser
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ ./xdg_example.exe --show-paths
  config_dir: ./home_testuser/./home_testuser/.config/xdg_example
  data_dir: ./home_testuser/./home_testuser/.local/share/xdg_example
  cache_dir: ./home_testuser/./home_testuser/.cache/xdg_example
  state_dir: ./home_testuser/./home_testuser/.local/state/xdg_example
  runtime_dir: <none>
  config_dirs: /etc/xdg/xdg_example
  data_dirs: /usr/local/share/xdg_example:/usr/share/xdg_example

Test with environment variables set:
  $ export HOME=./home_testuser
  $ export XDG_CONFIG_HOME=/tmp/xdge/config
  $ export XDG_DATA_HOME=/tmp/xdge/data
  $ export XDG_CACHE_HOME=/tmp/xdge/cache
  $ export XDG_STATE_HOME=/tmp/xdge/state
  $ export XDG_CONFIG_DIRS=/tmp/xdge/config1:/tmp/xdge/config2
  $ export XDG_DATA_DIRS=/tmp/xdge/data1:/tmp/xdge/data2
  $ ./xdg_example.exe --show-paths
  config_dir: /tmp/xdge/config
  data_dir: /tmp/xdge/data
  cache_dir: /tmp/xdge/cache
  state_dir: /tmp/xdge/state
  runtime_dir: <none>
  config_dirs: /tmp/xdge/config1/xdg_example:/tmp/xdge/config2/xdg_example
  data_dirs: /tmp/xdge/data1/xdg_example:/tmp/xdge/data2/xdg_example

Test with command-line overrides:
  $ export HOME=./home_testuser
  $ unset XDG_CONFIG_HOME XDG_DATA_HOME XDG_CACHE_HOME XDG_STATE_HOME XDG_RUNTIME_DIR
  $ unset XDG_CONFIG_DIRS XDG_DATA_DIRS
  $ ./xdg_example.exe --show-paths --config-dir ./override/config --data-dir ./override/data
  config_dir: ./home_testuser/./override/config
  data_dir: ./home_testuser/./override/data
  cache_dir: ./home_testuser/./home_testuser/.cache/xdg_example
  state_dir: ./home_testuser/./home_testuser/.local/state/xdg_example
  runtime_dir: <none>
  config_dirs: /etc/xdg/xdg_example
  data_dirs: /usr/local/share/xdg_example:/usr/share/xdg_example

