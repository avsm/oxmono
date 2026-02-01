# OxCaml Changes: 5.2.0minus-23 to 5.2.0minus-25

This document summarizes the key changes between OxCaml versions 5.2.0minus-23
and 5.2.0minus-25.

## Major New Features

### The Dissector: Linking Large Executables (#5146)

A new compiler pass called the "dissector" enables linking very large executables
with the small code model by:

- Analyzing all object files to compute total ELF section sizes
- Partitioning object files to prevent relocation overflow
- Partially linking each partition
- Creating an intermediate PLT/GOT for cross-partition calls

Enable with `-dissector`. Additional flags:
- `-dissector-partition-size <gb>` - Set partition threshold (default varies)
- `-ddissector` - Verbose logging
- `-ddissector-sizes` - Dump section sizes per file
- `-ddissector-partitions` - Keep partition files for debugging

### Untagged Int Arrays (#4643)

Full support for packed arrays of small integers:

```ocaml
(* New array types - tightly packed *)
let bytes : int8# array = [| #0s; #1s; #255s |]      (* 1 byte/element *)
let shorts : int16# array = [| #0S; #1S; #32767S |]  (* 2 bytes/element *)
let ints : int# array = [| #0; #1; #42 |]            (* native word/element *)
let chars : char# array = [| #'a'; #'b'; #'c' |]     (* 1 byte/element *)
```

Pattern matching now works with char# ranges:
```ocaml
match c with
| #'a'..#'z' -> `lowercase
| #'A'..#'Z' -> `uppercase
| _ -> `other
```

### Unboxed Elements at Module Top-Level (#4020, #5064)

Unboxed types can now appear at the top-level of modules:

```ocaml
module M = struct
  let pi : float# = #3.14159
  let answer : int32# = #42l
end
```

Previously this was prohibited.

## SIMD Enhancements

### AVX2 Gather Intrinsics (#5040)

Gather operations for loading values from non-contiguous memory addresses:

```ocaml
(* Gather using index vector - loads arr[indices[0]], arr[indices[1]], etc. *)
gather_int32x4 ~base ~indices ~scale ~mask
gather_float64x2 ~base ~indices ~scale ~mask
```

### BMI/BMI2 Intrinsics (#5065)

Complete set of bit manipulation instructions:

- **BMI**: `andn`, `bextr`, `blsi`, `blsmsk`, `blsr`, `tzcnt`
- **BMI2**: `bzhi`, `mulx`, `pdep`, `pext`, `rorx`, `sarx`, `shrx`, `shlx`
- **POPCNT**: `popcnt_int32`, `popcnt_int64`
- **LZCNT**: `lzcnt_int32`, `lzcnt_int64`

### SIMD Load/Store Intrinsics (#4994)

Direct memory operations with explicit alignment handling:

```ocaml
(* Aligned/unaligned loads and stores *)
vec128_load_aligned, vec128_store_aligned
vec128_load_unaligned, vec128_store_unaligned
vec256_load_aligned, vec256_store_aligned

(* Non-temporal (streaming) stores *)
vec128_store_aligned_uncached

(* Partial loads/stores *)
vec128_load_low64, vec128_load_low32
vec128_store_low64, vec128_store_low32
```

### 128-bit Integer Arithmetic (#5025)

Support for wide integer arithmetic using register pairs.

### Float64/Int64 Cast Builtins (#5114)

Bitwise reinterpretation between float64 and int64.

## Mode System Changes

### New `shareable` Portability Mode

The portability axis now has three values instead of two:

```
nonportable → shareable → portable
```

- `nonportable`: Functions capturing uncontended mutable state
- `shareable`: Functions capturing shared state (may execute in parallel)
- `portable`: Functions capturing all values at contended (may execute concurrently)

### `@@ global` Implies `@@ aliased`

For modalities, `@@ global` now always implies `@@ aliased`. Using
`@@ global unique` together is forbidden to ensure soundness of borrowing:

```ocaml
(* OK *)
type t = { field : 'a @@ global aliased }

(* ERROR - forbidden *)
type t = { field : 'a @@ global unique }
```

### Improved Modal Inclusion Errors (#5112)

Better error messages when mode constraints are violated.

## CFG Backend Improvements

### CFG Reducibility Checking (#4920, #4921)

The compiler now checks for and handles irreducible control flow graphs,
with safeguards to prevent optimizations that could create them.

### CFG Value Propagation (#4879, #4807)

Extended value propagation to float values and improved terminator simplification.
New flags:
- `-cfg-value-propagation` / `-no-cfg-value-propagation`
- `-cfg-value-propagation-float` / `-no-cfg-value-propagation-float`

### Register Allocation Affinity (#5059)

Basic support for register affinity hints in the allocator.

### CFG Peephole: Neutral Element Removal (#4932)

Removes operations whose operand is a neutral element (e.g., adding 0).

## Flambda2 / Optimizer Improvements

### Reaper Enhancements

- **Auto mode for direct call preservation** (#5081): New `-reaper-preserve-direct-calls auto`
  option that preserves direct calls only when the reaper cannot identify
  called functions.
- **Type rewriting** (#5043): The reaper can now rewrite types.
- **Local field handling**: New `-reaper-local-fields` flag.

### Improved Inlining Metrics (#5116)

Added profiling counters for inlining decisions that don't decrease code size.

### to_cmm Safety Improvements (#4941)

Prevents illegal re-orderings when converting to Cmm representation.

## Runtime Metaprogramming (Experimental)

### Slambda and Quotes (#4776, #5023, #5077)

Initial support for compile-time metaprogramming:
- `runtime_metaprogramming` language extension
- Slambda splices in Lambda
- Quote printing and AST mapper fixes

## Probes Support

### Unoptimized Implementation (#5007)

Basic probe support for runtime instrumentation. Closure middle-end support
was removed (#4990) - probes now require Flambda 2.

## Type System

### With-Bounds for GADTs Re-enabled (#5046)

Support for with-bounds constraints on GADTs has been restored.

### Looser Checking for Staticity (#5075)

More permissive checking for static expressions.

## Compiler Flags

### New Flags

- `-dissector` and related debug flags
- `-cfg-value-propagation[-float]`
- `-reaper-preserve-direct-calls auto`
- `-reaper-local-fields`
- `-reaper-unbox`
- `-reaper-change-calling-conventions`
- `-flambda2-expert-cmm-safe-subst`
- `-ddwarf-metrics-output-file`
- `-no-locs` (for test output)

## Build System Changes

- Removed `boot` directory (#5067)
- Added `make clean` and `make distclean` targets (#5035)
- Manifest files support (#4986)

## Bug Fixes

- Fixed computation of code size for `Boolean_not` switches (#5121)
- Fixed code size computation when converting switch to lookup table (#5120)
- Fixed over-estimation of removed primitives due to canonicalization (#5118)
- Race condition fix for `-dump-dir` with nonexistent path (#5010)
- Fixed fiber cache leak (#5017)
- Don't access globals in tight marking loop (#4997)
- Avoid touching global variables in sweep loop (#5022)
- Application type error now preempts mode error (#5073)
- Fixed simplify terminator pass for irreducible graphs (#5174)
- Small int SIMD casts no longer sign-extend incorrectly (#4987)

## Array Tag Changes (#5126)

Rearranged array tags to restore backwards compatibility with existing code.

## Documentation Updates

- Parallelism tutorials now use `#(...)` syntax for unboxed tuples
- Clarified portability mode documentation
- Updated small numbers documentation with array support
- Fixed contended/uncontended field projection documentation
