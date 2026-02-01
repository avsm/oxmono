# Oxmono: An OxCaml monorepo with batteries included

This OxCaml monorepo is a standalone repository which contains a fully
Dune-buildable set of packages for working with [OxCaml](https://oxcaml.org).
These packages include not only the Jane Street released libraries, but also
community libraries that have be [adapted](#packages) to work with OxCaml's
extensions.

In some cases, these are mechanical changes like porting to Dune from other
build systems, but we also do larger-scale changes to type signatures in order
to take advantage of OxCaml's extensions like stack allocation, unboxed types
or data-race parallel freedom. You can see the [set of available packages
below](#packages).

As such, this repository is an unstable moving target, but should provide a
convenient basis for use in open source infrastructure. We're using it in the
[Energy and Environment Group](https://www.cst.cam.ac.uk/research/eeg) to deploy
systems infrastructure for planetary computing, and also for personal projects
like [Anil's website](https://anil.recoil.org). The purpose is to 'eat our own dogfood'
and get familiar with using OxCaml for real projects outside of Jane Street's walls.

This repository is maintained using its own `oxmono` tool that's part of the
repository, with Claude helping to analyze changes and generate documentation.
The workflow is designed to be self-documenting: when packages are updated, the
tooling automatically categorizes modifications and regenerates the package
listings below. You should be able to use it yourself for your own projects
easily enough if you are up for a bit of [maintenance](#maintenance).

## Usage

All you need to get started is the OxCaml compiler (i.e. via an opam switch)
and a recent Dune and then everything else in here will build as a dune switch.
Install [opam](https://opam.ocaml.org) and:

```
opam init
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
```

The majority of packages are hidden behind a `(vendored_dirs)` directive which
means that they will only be compiled if there is a dependency on them from a
package in the workspace. We put packages that were originally upstream into the
[opam/](/opam) directory, and the [bleeding/](/bleeding) repository for
work-in-progress new libraries, and user-specific directories like [avsm/](/avsm)
for binaries that are deployed.

To setup a devcontainer so you can run this in a sandboxed environment with Claude
available, just do:

```
# initial setup or update
npx @devcontainers/cli up --workspace-folder . --remove-existing-container
# get a shell
npx @devcontainers/cli exec --workspace-folder . bash -l
```

## Custom Code

I'm sticking my own oxcaml code into `avsm/` to leave room for other users
to also contribute. This is a fluid repository so get in touch if you're
using it so I know to not break your usecase.

## Maintenance

This repository uses a two-directory model: `sources/` contains pristine
upstream code (it is actually a git worktree to the `sources` branch in this
repository), while `opam/` contains the OxCaml-adapted versions. The `oxmono`
CLI tool manages synchronization and analysis.

> **Note:** If `oxmono` isn't globally installed, prefix commands with `dune
> exec --` to run it directly from the monorepo (e.g., `dune exec -- oxmono
> diff dune`).

### How do I add a new package?

```bash
# Ensure the package is installed in your current opam switch
opam install <package-name>

# Add it to the monorepo
oxmono add <package-name>
```

This fetches the source, records it in `sources.yaml`, and commits the pristine copy to the `sources/` directory.

### How do I update a package from upstream?

Edit `sources.yaml` to update the git commit or archive URL, then:

```bash
oxmono sync <package-name>
```

This fetches the new version into `sources/`. You then manually merge changes into `opam/` as needed.

### How do I see what modifications were made to a package?

```bash
oxmono diff <package-name>
```

This shows the diff between `sources/<package>` (pristine upstream) and `opam/<package>` (local modifications).

### How do I understand why a package was modified?

Each package has analysis metadata in `changes/<package>.json`. To regenerate this after making changes:

```bash
oxmono changes <package-name>
```

This analyzes the diff and categorizes the changes (OxCaml extensions, dune port, compatibility fix, etc.). The README's package listings are generated from these files:

```bash
oxmono readme
```

### What's the relationship between sources/ and opam/?

| Directory | Purpose |
|-----------|---------|
| `sources/` | Pristine upstream copies (read-only reference) |
| `opam/` | Modified packages with OxCaml adaptations (what gets built) |
| `changes/` | JSON metadata describing modifications |

Only `opam/` is compiled by dune. The `sources/` directory exists purely for diff analysis and tracking upstream.

# Packages

## OxCaml Extensions

Packages using OxCaml features (unboxed types, stack allocation, modes, etc.)

- **backoff (0.1.1)**: Add runtime5 detection for Domain API compatibility and mark interface as portable.
  <details><summary>Details</summary>

  - Add `%runtime5` external to detect whether running on OCaml 5 runtime
  - Define `cpu_relax` and `recommended_domain_count` wrappers that provide no-op/fallback behavior on non-runtime5
  - Replace direct `Domain.cpu_relax` and `Domain.recommended_domain_count` calls with the wrapper functions
  - Add `@@ portable` annotation to `backoff.mli` interface
  - Include `backoff.patch` file recording these modifications
  </details>
- **bytesrw (0.3.0)**: Add dune build support, use OxCaml local types/mutable/zero_alloc features, and add Base dependency for local-aware Slice operations.
  <details><summary>Details</summary>

  - **Build system**: Port from ocamlbuild/topkg to dune build system
    - Add dune-project and dune files throughout src/ directories
    - Add config/discover.ml and config/dune for each C library binding (blake3, crypto, md, tls, xxhash, zlib, zstd)
    - Remove _tags, myocamlbuild.ml, pkg/pkg.ml, and .mllib files
  
  - **OxCaml features** in `bytesrw.ml`:
    - Use `@ local` mode annotations for Slice functions: `is_eod`, `first`, `last`, `length`, `equal`, `compare`, `copy`, `to_bytes`, `to_string`, `to_bigbytes`, `add_to_buffer`, `output_to_out_channel`
    - Add `make_local` function with `exclave_` for stack-allocated Slice creation
    - Use `[@zero_alloc]` attributes on Slice accessors and comparisons
    - Replace `ref` with `mutable` local bindings for iteration variables
    - Use mutable record fields for Reader/Writer state (`pos`, `read`, `write`)
    - Convert recursive loops to `while` loops with `mutable` iteration variables
  
  - **OxCaml features** in `bytesrw_utf.ml`:
    - Add `uchar_utf_8_byte_decode_length_unboxed` using unboxed `char#` type for zero-alloc pattern matching
  
  - **OxCaml features** in `bytesrw_fmt.ml` and `bytesrw_hex.ml`:
    - Convert `for` loops to `while` loops with `mutable` variables
    - Add `[@inline][@zero_alloc]` attributes to hot path functions
    - Optimize `of_binary_string` with lookup table and direct byte writes
  
  - **Compatibility**: Add `Base` library dependency for `Base.Bytes` with local-aware operations (`Local_bytes.length`, `Local_bytes.copy`, `Local_bytes.sub`, `Local_bytes.To_string.sub`)
  </details>
- **dune (3.21.0)**: Fix partial application issues for OxCaml mode compliance and remove boot install file.
  <details><summary>Details</summary>

  - **OxCaml mode fixes for partial application**: Several functions are wrapped with explicit eta-expansion to avoid partial application issues with OxCaml's stricter mode checking:
    - `otherlibs/configurator/src/v1.ml`: `create_process` changed to take explicit `stdin stdout stderr` parameters
    - `otherlibs/configurator/src/v1.ml`: `prerr_endline` and `log` wrapped as `fun s -> prerr_endline s` and `fun s -> log s`
    - `otherlibs/stdune/src/fpath.ml`: `Unix.unlink` wrapped as `fun s -> Unix.unlink s`
    - `src/csexp_rpc/csexp_rpc.ml`: `write` function parameters made explicit (`fd bytes pos len` instead of `t b`)
    - `src/dune_trace/dune_trace.ml`: `Stdlib.output_string out` wrapped as `fun s -> Stdlib.output_string out s`
  - **Removed file**: `boot/dune.install` removed (build system cleanup)
  - **Patch file added**: `oxcaml-dune.patch` contains the same fixes for documentation/reproducibility
  </details>
- **gen_js_api (1.1.2)**: Update PPX to handle OxCaml extended AST features (labeled tuples, modalities) and fix globalThis/multidomain compatibility issues.
  <details><summary>Details</summary>

  - **OxCaml AST compatibility**: Update PPX to handle extended Parsetree types:
    - `Ptyp_arrow` now takes 5 arguments instead of 3 (for modes/modalities)
    - `Ptyp_tuple` now contains labeled types; use `Ppxlib_jane.as_unlabeled_tuple` to extract
    - `Ptyp_var` now takes a tuple `(label, jkind)` instead of just `label`
    - `Ptyp_any` now takes a jkind argument
    - `Pmty_functor` now takes an extra modalities argument
    - `Psig_include` now returns a tuple with modalities
    - Add `ppxlib_jane` library dependency for shim functions
  - **globalThis fix**: Replace deprecated `joo_global_object` with standard `globalThis` in `ojs.ml`
  - **Multidomain alert suppression**: Add `[@ocaml.alert "-unsafe_multidomain"]` to `Printexc.register_printer` call in `ojs_exn.ml`
  - **PPX driver fix**: Disable ppxlib location check to allow `[@@deriving sexp]` with `[@@js]` attributes
  - **Signature parsing refactor**: Extract `parse_sig_items` to handle the new `psg_items` record field structure
  - **AST builder migration**: Replace `Exp.fun_` with `Ast_builder.Default.pexp_fun` for OxCaml compatibility
  - **Parse interface fix**: Handle new return type from `Pparse.parse_interface` (returns record with `.ast` field)
  </details>
- **js_of_ocaml**: Extensive patches for OxCaml compatibility including Float32, Null constant, unboxed types, mixed blocks, atomics, and OCaml 5.2 compiler API changes.
  <details><summary>Details</summary>

  ## OxCaml/Jane Street Compatibility
  - **Float32 support**: Full Float32 type implementation in the IR (code.ml/mli), evaluation (eval.ml), JS and WASM code generation, with new float32.js/float32.wat runtime files
  - **Null constant support**: Added `Const_null` handling in the compiler (code.ml, eval.ml, gc_target.ml)
  - **Mixed blocks**: Support for mixed block bytecode operations
  - **Unboxed types**: Support for unboxed indexing operations and int_u array primitives
  - **Atomics**: Extensive atomic field operations (load, store, cas, fetch_add, exchange, etc.) for both JS and WASM runtimes
  
  ## OCaml 5.2 Compiler API Compatibility
  - **compilation_unit changes**: Updated from `Cmo_format.compilation_unit` to `Cmo_format.compilation_unit_descr`
  - **Import_info**: Handle new `Import_info.t array` format for CRCs
  - **Compilation_unit.Name**: Use `name_as_string` for module name access
  - **Ident.is_global**: Compatibility for identifier API changes
  
  ## Runtime Additions
  - **caml_hash_exn**: Renamed hash function export for exception handling
  - **caml_with_async_exns**: New primitive for async exception handling via `Sys.with_async_exns`
  - **caml_bigstring_strncmp**: New bigstring comparison primitive
  - **caml_array_append**: Array append operation
  - **iarray primitives**: Immutable array support
  - **TLS runtime stubs**: Thread-local storage support
  - **caml_ml_set_channel_refill**: Stub for channel refill
  
  ## Configuration Changes
  - **use-js-string default**: Changed from `true` to `false` (important monorepo compatibility)
  
  ## Build System Fixes
  - **dune ppx flags**: Added `-no-check` flag to sedlex.ppx preprocessing
  - **build_fs.ml fix**: Added `globalThis.caml_create_file` fallback in filesystem initialization
  
  ## Bug Fixes
  - **global_deadcode fix**: Guard against variables minted after info table construction
  - **flow.ml**: Added `info_defs_length` check to prevent index out of bounds
  - **optcomp_light fixes**: PPX preprocessor compatibility fixes
  </details>
- **notty-community (0.2.4)**: Add OxCaml portable annotations, data-race-free parallelism support, and new terminal features including cursor styles, hyperlinks, and line-based diffing
  <details><summary>Details</summary>

  ## OxCaml Portability Changes
  - Add `@@ portable` annotation to `c_winsize` external function
  - Add `@ portable` mode annotations to `Winch.add` and `set_winch_handler` functions
  - Use `Sys.Safe.signal` instead of `Sys.signal` for data-race-free signal handling
  - Replace `Hashtbl` with `Portable_lockfree_htbl` for thread-safe SIGWINCH handler storage
  - Use `[%atomic.loc]` and `Portable.Atomic.Loc.update` for atomic updates to window resize state
  - Add `ppx_jane` preprocessing and `base`, `portable`, `portable_lockfree_htbl` dependencies
  - Use `Base.Obj.magic_portable` for Lwt integration compatibility
  
  ## New Features
  - **Cursor styles**: Add support for different cursor kinds (`Bar`, `Block`, `Underline`, blinking variants, `Default`)
  - **Hyperlinks**: Add `A.href` for clickable hyperlink support via OSC 8 escape sequences
  - **Default color**: Add `A.default` color constant
  - **Title management**: Add `set_title`, `save_title`, `restore_title` functions
  - **Exposed API**: Add `Tty_width_hint` module exposing notty's optimized unicode width function
  - **Ctrl+Backspace**: Distinguish Ctrl+Backspace from regular Backspace in input handling
  
  ## Performance Improvements
  - Line-based diffing: Only redraw changed lines during refresh by comparing previous and current operations
  - Track `previous_lines` state in `Tmachine.t` for incremental updates
  - Avoid sending `cleareol` on fully-filled lines to fix rendering in some terminals
  
  ## Terminal Compatibility Fixes
  - Use `\x1b[E` (cursor_nextline) instead of `\x1bE` for newline escape sequence
  - Use `\n` instead of escape codes for emacs terminal compatibility
  - Reset cursor to default style on terminal release
  - Fix rendering when last column is fully filled (vim's `:term` compatibility)
  </details>
- **ppxlib**: Extensive OxCaml AST extensions for unboxed types, modes, modalities, jkinds, comprehensions, and AST version 999 migration support.
  <details><summary>Details</summary>

  - **OxCaml AST extensions in ast.ml**: Adds support for modes (`mode`, `modes`), modalities (`modality`, `modalities`), jkind annotations (`jkind_annotation`), and `include_kind`
  - **Unboxed types support**: New AST nodes for `Pconst_unboxed_integer`, `Pconst_unboxed_float`, `Pconst_untagged_char`, `Ptyp_unboxed_tuple`, `Ppat_unboxed_tuple`, `Pexp_unboxed_tuple`, `Pexp_record_unboxed_product`, `Ppat_record_unboxed_product`, `Pexp_unboxed_field`
  - **Index kinds for unboxed arrays**: `Index_int`, `Index_unboxed_int64`, `Index_unboxed_int32`, `Index_unboxed_int16`, `Index_unboxed_int8`, `Index_unboxed_nativeint`
  - **Mode annotations on types**: `Ptyp_arrow` extended with mode annotations, `Ptyp_any` and `Ptyp_var` extended with jkind annotations
  - **Expression extensions**: `Pexp_let` extended with mutable flag, `Pexp_function` restructured with `function_param`, `function_constraint`, `function_body`, `Pexp_stack` for stack allocation, `Pexp_comprehension` for list/array comprehensions, `Pexp_idx` for indexed access with `block_access` and `unboxed_access`
  - **Runtime metaprogramming**: `Ptyp_quote`, `Ptyp_splice`, `Pexp_quote`, `Pexp_splice`, `Pexp_hole`
  - **Module system extensions**: `Pmty_functor` extended with modes, `Pmty_strengthen`, `Pmod_instance` for module instances, `signature` type restructured with `psg_modalities`
  - **Labeled tuples**: `Ptyp_tuple`, `Ppat_tuple`, `Pexp_tuple` extended with optional string labels
  - **New AST version (ast_999.ml)**: Complete OxCaml-extended Parsetree definition
  - **Migration modules**: `migrate_500_999.ml` and `migrate_999_500.ml` for AST version conversion
  - **Build system changes**: Split `ppxlib_ast` as separate package, added `ppxlib_jane` dependency, `-open Ocaml_shadow` flag
  - **Pattern extensions**: `Ppat_constraint` extended with optional type and modes, `Ppat_construct` extended with jkind annotations on type variables
  - **Type declaration extensions**: `ptype_jkind_annotation`, `Ptype_record_unboxed_product`, `pld_modalities` on label declarations, `pcd_vars` with jkind annotations, `constructor_argument` type with modalities
  - **Value binding extensions**: `pvb_modes` for mode annotations on let bindings
  - **Kind abbreviations**: `Psig_kind_abbrev`, `Pstr_kind_abbrev` for jkind abbreviation declarations
  </details>
- **re**: OxCaml portability annotations and immutable data layouts for thread safety, plus bugfix for exec_partial consistency with execp
  <details><summary>Details</summary>

  - **OxCaml portability and data race freedom**:
    - Added `@@ portable` module annotations throughout library interfaces (ast.mli, automata.mli, compile.mli, core.mli, cset.mli, etc.)
    - Added kind annotations for immutable data types (`type t : immutable_data`) for thread-safe sharing
    - Added `[@@unsafe_allow_any_mode_crossing]` annotations for types with mutable fields used in double-checked locking patterns
    - Changed mutable `int array` to `int iarray` in `mark_infos.ml` for thread safety
    - Added new `iarray.ml` module providing portable immutable array operations
    - Added runtime5-conditional Mutex implementation that's a no-op on runtime4 (for JSOO compatibility)
    - Changed `Hashtbl.Make` to `Hashtbl.MakePortable`, `Map.Make` to `Map.MakePortable`, `Set.Make` to `Set.MakePortable`
    - Added `Stdlib.Obj.magic_uncontended` calls for safe initialization of shared state
    - Changed `Positions.empty` from a value to `Positions.get_empty ()` function to avoid shared mutable state
    - Replaced integer comparison operators with `Int.compare` for portability
  
  - **Bugfix for exec_partial**:
    - Fixed `exec_partial` to return `Full` when pattern matches at end of input (previously returned `Partial` for patterns like `str "hello"` on input `"hello"`)
    - Added boundary check for partial matching without groups when at end of input
    - Added comprehensive tests for exec_partial consistency with execp
  
  - **Compatibility changes**:
    - Removed TSAN suppressions and related CI configuration (no longer needed with proper OxCaml annotations)
    - Simplified concurrency tests
    - Downgraded dune-project lang from 3.15 to 3.12
    - Added `seq` dependency
    - Removed `js_of_ocaml` as test dependency
    - Inlined top-level character class definitions in perl.ml to avoid portability issues
    - Adjusted GitHub Actions versions (downgraded checkout@v5 to v4, etc.)
  </details>
- **uutf (1.0.3)**: Add dune build support and OxCaml portability annotations with local_ folder types
  <details><summary>Details</summary>

  - **Dune port**: Added `dune-project`, `src/dune`, `test/dune`, and `doc/dune` files to replace ocamlbuild/topkg build system
  - **Removed ocamlbuild files**: Deleted `_tags`, `src/uutf.mllib`, `pkg/pkg.ml`, `pkg/META`, `B0.ml`, `BRZO`, and root `opam` file
  - **OxCaml portability**: Added `@@ portable` annotation to `uutf.mli`
  - **OxCaml local_ types**: Changed `'a folder` type to use `local_` for the curried function arguments, enabling stack allocation of intermediate closures
  - **OxCaml contended arrays**: Added `magic_uncontended` helper to handle `utf_8_len` array access in a data-race-free manner (array is immutable)
  - **Bytes.empty replacement**: Changed `Bytes.empty` to `Bytes.create 0` throughout (4 occurrences) for OxCaml compatibility
  - **Test renamed**: `test/test.ml` renamed to `test/test_uutf.ml` in dune configuration
  </details>
- **zarith (1.14+ox)**: Add dune build support and OxCaml portability annotations for arbitrary-precision integer library
  <details><summary>Details</summary>

  - **Dune build support**: Added `dune`, `dune-project`, and `tests/dune` files with build rules that run the upstream configure script and extract compiler flags
  - **OxCaml portability annotations**: Added `@@ portable` annotations to all external C functions in `z.ml` and `z.mli`
  - **Immutable data jkind**: Changed `type t` to `type t : immutable_data` in both `Z` and `Q` modules
  - **Local mode support**: Added `local_` annotations to input parameters (`of_int32`, `of_int64`, `of_float`, `of_string`, etc.) and added `compare__local`/`equal__local` variants in `Q` module
  - **Global field annotations**: Added `global_` annotations to `Q.t` record fields (`num`, `den`)
  - **Safe callback registration**: Changed `Callback.register_exception` to `Callback.Safe.register_exception`
  - **Configure script improvements**: Added Windows (MSVC/mingw) platform detection for proper object/library/dll suffixes; now generates `Makefile.config` instead of `Makefile`; bumped minimum OCaml version to 4.07
  - **Opam improvements**: Added homebrew arm64 configure support; added `conf-pkg-config` dependency; version bumped to 1.14+ox
  - **Q.abs optimization**: Avoids allocation when the number is already non-negative
  - **Discover script**: Added `discover.ml` for platform-specific GMP library detection (OpenBSD, FreeBSD, macOS/Homebrew)
  </details>

## Dune Ports

Packages with dune build support added

- **astring (0.8.5)**: Add dune build support replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` file with lang dune 3.20
  - Added `src/dune` defining the main `astring` library and `astring.top` sublibrary
  - Added `test/dune` for test executables and runtest rules
  - Added `doc/dune` for documentation
  - Renamed `opam` to `astring.opam` (standard dune convention)
  - Removed ocamlbuild files: `_tags`, `pkg/pkg.ml`, `pkg/META`, `src/*.mllib`
  </details>
- **bos (0.2.1)**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Removed `B0.ml`, `_tags`, `pkg/pkg.ml`, `pkg/META`, and `.mllib` files (ocamlbuild/topkg build artifacts)
  - Renamed `opam` to `bos.opam` (dune convention)
  - Added `dune-project` file with dune 3.20 and project name
  - Added `src/dune` with three libraries: `bos` (main), `bos.setup`, and `bos.top`
  - Added `test/dune` with test executables and runtest rule
  - Added `doc/dune` for documentation
  - Watch test marked as `optional` since it depends on mtime which may not always be available
  </details>
- **brr (0.0.7)**: Port from ocamlbuild/topkg/B0 build system to dune
  <details><summary>Details</summary>

  - Remove ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`, `pkg/META`, `pkg/pkg.ml`, `src/brr.mllib`
  - Add `dune-project` file with lang dune 3.20
  - Add dune files for all libraries:
    - `src/dune`: Main `brr` library with explicit module list
    - `src/ocaml_poke/dune`: `brr.ocaml_poke` sub-library
    - `src/ocaml_poke_ui/dune`: `brr.ocaml_poke_ui` sub-library
    - `src/poke/dune`: `brr.poke` sub-library
    - `src/poked/dune`: `brr.poked` sub-library
  - Add `src/console/dune` for installing console share files
  - Add `doc/dune` for documentation and odoc assets installation
  - Move opam file from `opam` to `brr.opam` (standard dune location)
  </details>
- **cmarkit (0.3.0)**: Add dune build support replacing B0/ocamlbuild build system
  <details><summary>Details</summary>

  - Add `dune-project` file with lang dune 3.20
  - Add `src/dune` with library definition (wrapped false, public_name cmarkit)
  - Add `src/tool/dune` for the cmarkit executable (optional, depends on cmdliner)
  - Add `doc/dune` for documentation
  - Move `opam` file to `cmarkit.opam` (standard location for dune)
  - Remove B0 build files: `B0.ml`, `BRZO`
  - Remove ocamlbuild files: `_tags`, `src/cmarkit.mllib`
  - Remove topkg files: `pkg/pkg.ml`, `pkg/META`
  </details>
- **fmt (0.10.0)**: Add dune build support, replacing ocamlbuild/topkg/B0 build system
  <details><summary>Details</summary>

  - Added `dune-project` file with dune language 3.20
  - Added `dune` files for main library (`src/dune`), tty library (`src/tty/dune`), cli library (`src/cli/dune`), top library (`src/top/dune`), documentation (`doc/dune`), and tests (`test/dune`)
  - Removed B0 build system file (`B0.ml`)
  - Removed ocamlbuild files (`_tags`, `pkg/pkg.ml`, `pkg/META`, `src/fmt.mllib`)
  - Moved `opam` file to `fmt.opam` (standard dune convention)
  </details>
- **fpath (0.7.3)**: Add dune build support replacing ocamlbuild/topkg
  <details><summary>Details</summary>

  - Remove ocamlbuild files: `_tags`, `.mllib` files, `pkg/pkg.ml`, `pkg/META`
  - Add `dune-project` with lang dune 3.20
  - Add `src/dune` for main library with astring dependency
  - Add `src/top/dune` for toplevel support library (fpath.top)
  - Add `doc/dune` for documentation
  - Add `test/dune` for test executable using b0.testing
  - Rename `opam` to `fpath.opam`
  </details>
- **hmap (0.8.1)**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Remove ocamlbuild files: `_tags`, `pkg/pkg.ml`, `pkg/META`, `src/hmap.mllib`
  - Remove ocamldoc files: `doc/api.odocl`, `doc/dev.odocl`
  - Add `dune-project` with lang dune 3.20
  - Add `src/dune` for library with `(wrapped false)`
  - Add `test/dune` for test executable with runtest alias
  - Convert `opam` file from opam 1.2 to opam 2.0 format with dune build commands
  - Update OCaml version requirement from 4.02.0 to 4.08.0
  </details>
- **htmlit (0.2.0)**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Add `dune-project` file with lang dune 3.21
  - Add root `dune` file with compiler flags matching ocamlbuild settings
  - Add `src/dune` defining the library with `(wrapped false)`
  - Add `test/dune` for the example executable
  - Replace `opam` file with `htmlit.opam` using dune build commands
  - Remove ocamlfind, ocamlbuild, and topkg build dependencies
  </details>
- **jsonm (1.0.2)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` with dune 3.20 and package name
  - Added `src/dune` library definition with uutf dependency
  - Added `test/dune` with executables (jsontrip, test, examples, jtree) and runtest alias
  - Added `doc/dune` for documentation
  - Added `jsonm.opam` file (moved from `opam`)
  - Removed ocamlbuild files: `_tags`, `src/jsonm.mllib`, `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build system files: `B0.ml`, `BRZO`
  </details>
- **jsont (0.2.0)**: Add dune build support, replacing B0/topkg build system
  <details><summary>Details</summary>

  - Remove B0.ml build file
  - Remove ocamlbuild _tags file
  - Remove pkg/META and pkg/pkg.ml (topkg packaging)
  - Remove .mllib files (src/jsont.mllib, src/brr/jsont_brr.mllib, src/bytesrw/jsont_bytesrw.mllib)
  - Add dune-project with dune 3.20
  - Add jsont.opam with dune build instructions
  - Add src/dune for main jsont library
  - Add src/brr/dune for optional jsont.brr library
  - Add src/bytesrw/dune for optional jsont.bytesrw library
  - Add doc/dune for documentation
  </details>
- **logs (0.9.0)**: Add dune build support replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Add `dune-project` file with lang dune 3.20
  - Add `dune` files for all sub-libraries: core `logs`, `logs.fmt`, `logs.browser`, `logs.cli`, `logs.lwt`, `logs.threaded`, `logs.top`
  - Add `doc/dune` for documentation
  - Move `opam` file to `logs.opam` (standard dune convention)
  - Remove ocamlbuild/topkg files: `B0.ml`, `BRZO`, `_tags`, `pkg/pkg.ml`, `pkg/META`, `src/logs.mllib`
  - Mark optional libraries (`logs.browser`, `logs.lwt`, `logs.threaded`) with `(optional)` in dune
  - Use `(wrapped false)` to maintain backward compatibility with existing module paths
  - Add install stanzas for top-level init files (`logs_top_init.ml`, `logs_fmt_top_init.ml`)
  </details>
- **mtime (2.1.0)**: Port from ocamlbuild/topkg to dune build system with reorganized source structure.
  <details><summary>Details</summary>

  - Removed ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`, `pkg/pkg.ml`, `pkg/META`, `.mllib` and `.clib` files
  - Added `dune-project` and `mtime.opam` files for dune-based builds
  - Added `dune` files in `src/`, `src/top/`, `src-clock/`, `test/`, and `doc/` directories
  - Reorganized clock implementation: moved from `src/clock/` to new `src-clock/` directory
  - The `mtime.clock.os` library now lives in `src-clock/` with proper dune configuration including C stubs and js_of_ocaml runtime
  - Added virtual `mtime.clock` library in `src/` (empty modules placeholder)
  - Added `Mtime_clock` interface file (`mtime_clock.mli`) in main `src/` directory for the virtual clock
  - Tests are defined but disabled (marked optional, runtest rules commented out) as they require the `b0` library
  </details>
- **ocamlfind (1.9.8.git)**: Add dune build support with META files for standard libraries and fix toplevel logging for OxCaml compatibility.
  <details><summary>Details</summary>

  - **Dune build system**: Added `dune-project`, `dune` files for building findlib and ocamlfind executables
  - **META files for standard libraries**: Added META files for bigarray, bytes, compiler-libs, dynlink, raw_spacetime, stdlib, str, threads, unix
  - **Configurator-based discovery**: New `tools/discover.ml` using dune-configurator to generate `findlib_config.ml`, `topfind`, and `findlib.conf`
  - **Extract args tool**: Added `tools/extract_args/` with lexer to extract OCaml compiler arguments
  - **Toplevel scripts**: Added `topfind_rd0.p` and `topfind_rd1.p` templates supporting both bytecode and native toplevel
  - **OxCaml compatibility fix**: Modified `topfind.ml.in` to use explicit lambda `fun s -> prerr_endline s` instead of partial application with `ignore`, avoiding type inference issues with OxCaml's stricter typing
  - **Opam packages**: Split into `findlib.opam` (library) and `ocamlfind.opam` (tool) packages
  - **Runtime events support**: Added META template for OCaml 5.x runtime_events library
  </details>
- **ocurl (0.10.0)**: Port from autoconf to dune build system and add new libcurl features including PREREQFUNCTION, AWS_SIGV4, TCP keepalive options, NOPROXY, bigstring write callbacks, and Multi.closesocket_function.
  <details><summary>Details</summary>

  - **Build system migration**: Complete removal of autoconf/configure build system in favor of dune with dune-configurator for feature detection
  - **Package rename**: Transition from `ocurl` to `curl` package name with `curl_lwt` split into separate package
  - **New CURL options**: Added `CURLOPT_PREREQFUNCTION`, `CURLOPT_AWS_SIGV4`, `CURLOPT_PROXY_SSL_OPTIONS`, `CURLOPT_NOPROXY`, `CURLOPT_TCP_KEEPALIVE`, `CURLOPT_TCP_KEEPIDLE`, `CURLOPT_TCP_KEEPINTVL`
  - **New API functions**: Added `get_headers` for curl_easy_nextheader, `set_writefunction_buf` for zero-copy bigstring callbacks, `Multi.set_closesocket_function`
  - **Multi.wait/poll improvements**: Extended with `extra_fds` parameter for curl_multi_wait/poll extra file descriptors
  - **Runtime lock fix**: Fixed handling of runtime lock when calling curl multi callback cleanups
  - **C code modernization**: Updated to use `caml_release_runtime_system`/`caml_acquire_runtime_system` instead of deprecated enter/leave blocking section functions
  - **OCaml 5 compatibility**: Added support for OCaml 4.12+ option helpers and custom block operations
  </details>
- **ptime (1.2.0)**: Add dune build support, replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20
  - Add `src/dune` for main ptime library with install rule for `ptime_top_init.ml`
  - Add `src/clock/dune` for ptime.clock library with C stubs, js_of_ocaml runtime, and deprecated ptime.clock.os alias
  - Add `src/top/dune` for ptime.top library with toplevel support
  - Add `doc/dune` for documentation
  - Rename `opam` to `ptime.opam` (standard dune convention)
  - Remove ocamlbuild files: `B0.ml`, `BRZO`, `_tags`, `myocamlbuild.ml`
  - Remove topkg files: `pkg/pkg.ml`, `pkg/META`
  - Remove mllib files: `src/ptime.mllib`
  </details>
- **react (1.2.2)**: Add dune build support, replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Add `dune-project` with dune 3.20 and package name
  - Add `src/dune` defining `react` library and `react.top` toplevel support library
  - Add `test/dune` with test executable and runtest alias, plus clock and breakout example executables
  - Add `doc/dune` for documentation
  - Rename `opam` to `react.opam` (dune convention)
  - Remove ocamlbuild files: `_tags`, `src/react.mllib`, `src/react_top.mllib`
  - Remove topkg files: `pkg/pkg.ml`, `pkg/META`
  - Remove B0 build files: `B0.ml`, `BRZO`
  </details>
- **rresult (0.7.0)**: Add dune build support, replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Added `dune-project` file with lang dune 3.20
  - Added `src/dune` with two libraries: `rresult` (main library) and `rresult.top` (toplevel support)
  - Added install stanza for `rresult_top_init.ml`
  - Added `test/dune` with executable and runtest rule
  - Added `doc/dune` for documentation
  - Moved `opam` to `rresult.opam` (dune naming convention)
  - Removed ocamlbuild files: `_tags`, `src/rresult.mllib`, `src/rresult_top.mllib`
  - Removed topkg files: `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build configuration: `B0.ml`
  </details>
- **uucp (16.0.0)**: Add dune build support replacing ocamlbuild/topkg build system
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 and package name
  - Added `src/dune` defining the `uucp` library with `wrapped false`
  - Added `test/dune` with executables for `ucharinfo` (optional, with uunf/cmdliner), `link_test`, and `perf`
  - Added `doc/dune` for documentation
  - Moved opam file from `opam` to `uucp.opam` (standard dune convention)
  - Removed ocamlbuild files: `_tags`, `src/uucp.mllib`, `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build system files: `B0.ml`, `BRZO`
  </details>
- **uunf**: Port from ocamlbuild/topkg to dune build system
  <details><summary>Details</summary>

  - Add `dune-project` file with lang dune 3.20
  - Add `src/dune` library definition with `(wrapped false)` and `-w -32` flag to suppress warnings
  - Replace `opam` file with `uunf.opam` using dune build command
  - Add development files: `.gitignore`, `.merlin`, `.ocp-indent`
  - Revert hardcoded version strings to `%%VERSION%%` placeholders in README.md, doc/index.mld, pkg/META, and test/unftrip.ml
  - Update homepage to dune-universe fork in opam file
  - Remove ocamlbuild/topkg build dependencies, add dune dependency
  </details>
- **uuseg (16.0.0)**: Add dune build support replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Added `dune-project` file with dune 3.20 and package name
  - Added `src/dune` with library definition (unwrapped, depends on uucp)
  - Added `test/dune` with executables for usegtrip and test_uuseg (marked optional)
  - Added `doc/dune` for documentation
  - Moved opam file from `opam` to `uuseg.opam` (standard dune location)
  - Removed ocamlbuild files: `_tags`, `src/uuseg.mllib`, `pkg/pkg.ml`, `pkg/META`
  - Removed B0 build files: `B0.ml`, `BRZO`
  - Tests are disabled in dune (commented out runtest alias) due to b0 library dependency
  </details>
- **xmlm (1.4.0)**: Add dune build support, replacing ocamlbuild/topkg build system.
  <details><summary>Details</summary>

  - Add `dune-project` file with dune 3.20 language version
  - Add `src/dune` to build the xmlm library
  - Add `test/dune` with executables for xmltrip, test, examples, and test_tree, plus runtest rules
  - Add `doc/dune` for documentation
  - Add `xmlm.opam` file (renamed from `opam`)
  - Remove ocamlbuild files: `_tags`, `src/xmlm.mllib`, `pkg/pkg.ml`, `pkg/META`
  - Remove B0 build file `B0.ml`
  - Remove old `opam` file (content preserved in `xmlm.opam`)
  </details>

## New Features

Packages with new features not in upstream

- **ocaml-lsp-server (1.19.0+ox)**: Major fork integrating Jane Street Async runtime, removing Dune RPC integration, and adding internal extensions for monorepo compatibility.
  <details><summary>Details</summary>

  - **Jane Street Async integration**: Replaces lev/lev-fiber event loop with new `fiber-async` and `lev-fiber-async` modules that bridge Fiber and Async runtimes
  - **Dependency changes**: Adds core, core_unix, async, async_log, async_kernel, async_unix, re2, ppx_jane preprocessors; removes lev, lev_fiber
  - **Removed Dune RPC integration**: Removes `dune.ml`/`dune.mli` and all Dune-related diagnostics, progress reporting, and promotion commands
  - **Version downgrade**: Uses version 1.19.0+ox (based on older upstream) vs upstream 1.25.0
  - **Merlin version pinned**: Uses merlin-lib 5.2.1-502+ox instead of 5.6
  - **OCamlformat downgrade**: Uses 0.27.0 instead of 0.28.1
  - **New modules**: Adds `lsp_timing_logger.ml`, `diagnostic_util.ml`, `call_hierarchy.ml`, refactored code actions into separate files
  - **Configuration changes**: Renames `report_dune_diagnostics` to `display_merlin_diagnostics` with inverted default; changes `shorten_merlin_diagnostics` default to true
  - **Feature changes**: Adds call hierarchy provider and color provider capabilities; removes some custom requests (construct, type_search, merlin_jump, phrase, type_expression, locate)
  - **Alert suppression**: Adds `-alert -unstable` and `-alert -unsafe_multidomain` flags for OCaml 5 compatibility
  - **JSON type expansion**: Expands Json.t type definition inline to include `Intlit`, `Tuple`, `Variant` variants
  - **Submodule lev disabled**: The lev submodule dune files are removed (libraries not built)
  </details>

## Bug Fixes

Packages with bug fixes not yet in upstream

- **afl-persistent**: Fix config.sh to return to original directory instead of root
  <details><summary>Details</summary>

  - Modified `config.sh` to save current directory before changing to temp directory
  - Changed `cd /` to `cd "$curdir"` after cleanup, returning to original directory instead of root
  - Included patch file documenting the change for upstream submission
  </details>

## Compatibility

Packages with monorepo compatibility changes

- **alcotest (1.9.0)**: Expand eta-reduced function to avoid locality inference issues
  <details><summary>Details</summary>

  - Modify `length_utf8` function in `alcotest_stdlib_ext.ml` to explicitly take parameter `t` instead of being point-free
  - This change works around locality/mode inference issues where the eta-reduced form causes type errors in OxCaml's stricter mode system
  - Includes a `local-restriction.patch` file documenting the change
  </details>
- **conduit (8.0.0)**: Remove SSL test configuration file for monorepo compatibility
  <details><summary>Details</summary>

  - Removed `tests/conduit-lwt-unix/server.conf` SSL certificate generation configuration file used for testing
  </details>
- **jingoo**: Remove .gitignore file for monorepo compatibility
  <details><summary>Details</summary>

  - Removed `.gitignore` file that excluded build artifacts, generated files, and editor config files
  </details>
- **lwt (6.0.0)**: Compatibility updates for OxCaml AST changes and type inference fixes in ppx and Unix stubs.
  <details><summary>Details</summary>

  - **ppx_lwt.ml AST compatibility**: Update pattern matching for `Ppat_constraint` to handle 3-tuple form `(p,_,_)` instead of 2-tuple
  - **ppx_lwt.ml AST compatibility**: Update `Pexp_let` pattern to include extra leading wildcard parameter `(_, Nonrecursive, vbl, e)`
  - **ppx_lwt.ml AST compatibility**: Replace `pexp_function_cases` with `pexp_function` (5 occurrences) for updated ppxlib API
  - **lwt_unix.cppo.ml type inference fix**: Expand conditional function bindings (`do_recv`, `do_send`, `do_recvfrom`, `do_sendto`) from point-free style to explicit lambda form to help type inference with external stub functions
  </details>
- **menhir (20250912)**: Remove generated documentation files and add opam metadata for monorepo integration
  <details><summary>Details</summary>

  - Remove `doc/manual.html` (generated HTML documentation)
  - Remove `doc/manual.pdf` (generated PDF documentation)
  - Remove `src/.merlin` (IDE configuration file)
  - Add `opam` package metadata file
  </details>
- **ocamlbuild**: Makefile and configuration fixes for `for-pack` tag propagation to bytecode compilation
  <details><summary>Details</summary>

  - **Makefile changes**: Split generic `%.cmo` and `%.cmi` pattern rules into directory-specific rules (`src/`, `bin/`, `plugin-lib/`) to ensure `src/` modules are compiled with `-for-pack Ocamlbuild_pack` flag for bytecode
  - **Configuration fix** (`src/configuration.ml`): Added logic to propagate `for-pack(...)` tags from `.cmx` files to corresponding `.ml` and `.mli` files, ensuring pack configuration applies consistently to both native and bytecode compilation
  - **Removed `.depend` file**: Generated dependency file removed from version control
  - **Included `flambda2.patch`**: Contains the same Makefile and configuration changes as a patch file
  </details>
- **psq (0.2.1)**: Remove seq library dependency as it is now part of stdlib
  <details><summary>Details</summary>

  - Remove `(libraries seq)` from dune file since `Seq` is now part of the OCaml standard library (since OCaml 4.14)
  </details>
- **sedlex (3.6)**: Compatibility fixes for OxCaml's extended AST with labeled tuples and ppxlib_jane integration
  <details><summary>Details</summary>

  - Remove unused `Ast_helper` import
  - Replace `Exp.fun_` with `Ppxlib_jane.Ast_builder.Default.add_fun_param` for function parameter construction
  - Update `Ppat_tuple` pattern matching to handle labeled tuple AST format: `([None, p0; None, p1], Closed)` instead of `[p0; p1]`
  - Use `Ppxlib_jane.as_unlabeled_tuple` helper function to extract unlabeled tuples with proper error handling
  - Add `loc_ghoster` object to mark generated code locations as ghost locations
  - Apply `loc_ghoster` transformation to all PPX-generated expressions and structures to avoid spurious location warnings
  - Include patch file documenting all modifications
  </details>
- **topkg**: Remove redundant string function declarations that shadow stdlib String module
  <details><summary>Details</summary>

  - Remove `val trim : string -> string` declaration from `topkg_string.mli` (already available via `include module type of String`)
  - Remove `val uppercase_ascii : string -> string` declaration (already available via `include module type of String`)
  - These declarations were redundant since the file includes `module type of String`, and removing them avoids shadowing warnings or conflicts with newer OCaml stdlib versions
  </details>
- **tyxml (4.6.0)**: Remove seq library dependency as it is part of stdlib in OCaml 5.x
  <details><summary>Details</summary>

  - Remove `seq` from libraries in `lib/dune` - the Seq module is part of the standard library in OCaml 4.14+ and no longer requires a separate dependency
  </details>

## Build Fixes

Packages with build system fixes

- **gen (1.1)**: Remove seq library dependency and clean up unused build files
  <details><summary>Details</summary>

  - Remove `seq` library dependency from `src/dune` (seq is now part of stdlib)
  - Delete unused `bench/.merlin` file
  - Delete unused `qtest/Makefile` and `qtest/dune` test infrastructure files
  </details>

## JaneStreet

Packages from github.com/janestreet or github.com/oxcaml (unmodified)

abstract_algebra, accessor, accessor_async, accessor_base, accessor_core, am_running_how_js, async, async_durable, async_extra, async_find, async_inotify, async_interactive, async_iterator, async_js, async_kernel, async_log, async_rpc_kernel, async_rpc_websocket, async_sendfile, async_shell, async_smtp, async_ssl, async_udp, async_unix, async_websocket, await, babel, base, base_bigstring, base_quickcheck, base_trie, basement, bidirectional_map, big_percent, bigdecimal, bignum, bin_prot, binaryen (119), bitset, bonsai, bonsai_bench, bonsai_concrete, bonsai_examples, bonsai_term, bonsai_term_components, bonsai_term_examples, bonsai_term_test, bonsai_test, bonsai_web, bonsai_web_components, bonsai_web_test, capitalization, capsule, capsule0, codicons, cohttp_async_websocket, cohttp_static_handler, command_nodejs, command_rpc, concurrent, content_security_policy, core, core_bench, core_extended, core_kernel, core_profiler, core_unix, csvfields, curl_async, current_exe, dedent, delimited_parsing, ecaml, email_message, env_config, expect_test_helpers_async, expect_test_helpers_core, expectable, expectree, fieldslib, file_path, filesystem, flexible_sexp, float_array, font_awesome_icons, fuzzy_match, fzf, handled_effect, hardcaml, hardcaml_axi, hardcaml_c, hardcaml_circuits, hardcaml_event_driven_sim, hardcaml_fixed_point, hardcaml_handshake, hardcaml_hobby_boards_demos, hardcaml_hobby_boards_kernel, hardcaml_of_verilog, hardcaml_step_testbench, hardcaml_template_project, hardcaml_test_harness, hardcaml_verify, hardcaml_verilator, hardcaml_waveterm, hardcaml_xilinx, hardcaml_xilinx_components, hardcaml_xilinx_reports, heterogeneous_list, hex_encode, hg_lib, higher_kinded, incr_dom, incr_dom_interactive, incr_dom_partial_render, incr_dom_sexp_form, incr_map, incr_select, incremental, indentation_buffer, insertion_ordered_map, int_repr, jane-street-headers, jane_rope, janestreet_cpuid, janestreet_csv, janestreet_lru_cache, janestreet_shims, js_of_ocaml_patches, jsonaf, jst-config, legacy_diffable, line-up-words, lsp_rpc, man_in_the_middle_debugger, maybe_pushback, mdx, memtrace, memtrace_viewer, merlin (5.2.1-502+ox), mlt_parser, n_ary, netsnmp, nonempty_interval_lib, notty_async, numeric_string, ocaml-compiler-libs (v0.17.0), ocaml-embed-file, ocaml-probes, ocaml_intrinsics, ocaml_intrinsics_kernel, ocaml_openapi_generator, ocaml_simd, ocamlformat, of_json, oklab, ordinal_abbreviation, oxcaml_intrinsics, pam, parallel, parsexp, parsexp_io, parsexp_prefix, parsexp_symbolic_automaton, patdiff, patience_diff, pending_or_error, pipe_with_writer_error, polling_state_rpc, portable, portable_lockfree_htbl, portable_ws_deque, posixat, postgres_async, ppx_accessor, ppx_anonymous_record, ppx_array, ppx_array_base, ppx_assert, ppx_base, ppx_bench, ppx_bin_prot, ppx_box, ppx_cold, ppx_compare, ppx_conv_func, ppx_css, ppx_csv_conv, ppx_custom_printf, ppx_debug_assert, ppx_demo, ppx_derive_at_runtime, ppx_diff, ppx_disable_unused_warnings, ppx_embed_file, ppx_enumerate, ppx_expect, ppx_fields_conv, ppx_fixed_literal, ppx_for_loop, ppx_fuelproof, ppx_globalize, ppx_hardcaml, ppx_hash, ppx_helpers, ppx_here, ppx_html, ppx_ignore_instrumentation, ppx_inline_test, ppx_int63_literal, ppx_jane, ppx_js_style, ppx_jsonaf_conv, ppx_let, ppx_log, ppx_module_timer, ppx_optcomp, ppx_optional, ppx_pattern_bind, ppx_pipebang, ppx_portable, ppx_quick_test, ppx_rope, ppx_sexp_conv, ppx_sexp_message, ppx_sexp_value, ppx_shorthand, ppx_simple_xml_conv, ppx_stable, ppx_stable_witness, ppx_string, ppx_string_conv, ppx_template, ppx_tydi, ppx_typed_fields, ppx_typerep_conv, ppx_var_name, ppx_variants_conv, ppx_with, ppx_xml_conv, ppx_yojson_conv, ppx_yojson_conv_lib, ppxlib_jane, profunctor, protocol_version_header, re2, re_parser, record_builder, redis-async, regex_parser_intf, resource_cache, result (1.5), rpc_parallel, semantic_version, sequencer_table, sexp, sexp_diff, sexp_grammar, sexp_macro, sexp_pretty, sexp_select, sexp_string_quickcheck, sexp_type, sexplib, sexplib0, shell, shexp, simple_xml, spawn, splay_tree, splittable_random, stdio, stored_reversed, streamable, string_dict, testable_timeout, textutils, textutils_kernel, tilde_f, time_ago, time_now, timezone, toplayer, toplevel_backend, toplevel_expect_test, topological_sort, torch, tracing, typerep, unboxed, unboxed_datatypes, unique, univ_map, unsafe_clear, uopt, uri_parsing, username_kernel, utop, variantslib, vcaml, vec, versioned_polling_state_rpc, virtual_dom, xpath, zarith_stubs_js, zstandard

## Unchanged

Packages with no modifications from upstream

angstrom (0.16.1), asn1-combinators (0.3.2), base64 (3.5.2), bigarray-compat (1.1.0), bigstringaf (0.10.0), ca-certs (1.0.1), camlp-streams, checkseum (0.5.2), cmdliner (1.3.0), cohttp (6.2.1), cppo (1.8.0), crowbar, crunch (4.0.0), cryptokit, csexp (1.5.2), cstruct (6.2.0), ctypes, decompress (1.5.3), digestif (1.3.0), domain-local-await (1.0.1), domain-name (0.5.0), dune-compiledb (0.6.0), either (1.0.0), eqaf (0.10), ezjsonm (1.3.0), faraday (0.8.2), fiber (3.7.0), fix, gmap (0.3.0), hex (1.5.0), httpaf (0.7.1), inotify (2.6), integers, iomux (0.4), ipaddr (5.6.1), jsonfeed (1.1.0), kdf (1.0.0), lambda-term (3.2.0), lambdasoup, lwt-dllist (1.1.0), magic-mime (1.3.1), markup, mew, mew_vi, mirage-crypto (2.0.2), num (1.7~dev), ocaml-syntax-shims (1.0.0), ocaml-version, ocamlgraph (2.0.0), ocp-indent (1.9.0), ocplib-endian, ohex, opam, opam-file-format (2.2.0), optint (0.3.0), owee (0.8), patch (3.1.0), pcre (8.0.5), pp (2.0.0), ppx_blob (0.9.0), ppx_derivers, progress (0.5.0), sha (1.15.4), sitemap (1.0), stdlib-shims (0.3.0), stringext (1.6.0), swhid_core, syndic (1.7.0), thread-table (1.0.0), tls (2.0.3), trie, uchar, uri (4.4.0), uring (2.7.0), x509 (1.0.6), yojson (2.2.2), zed

