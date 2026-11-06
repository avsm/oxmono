## Vendored base (2026-10-15)

Based on `v1.4.0`, commit
`adcc963fcf936ceb400082fbf552973a5afc73e6`. See [../upstreams.json](../upstreams.json)
for the repository and import scope.

The refresh adds BLAKE3, exposes keyed BLAKE2 functors and optimizes the
OCaml hash cores. The C backend remains the workspace default.

Local patches cover the shared sources and both backends:

- `src/digestif.mli` retains the portable digest API and raw digest/HMAC helpers.
- `src-c/digestif_native.ml` marks the C entry points portable, including
  BLAKE3.
- `src-c/native/*.{c,h}` take hash input lengths as `size_t`. Upstream
  declares every `digestif_*_update` except BLAKE3's with a `uint32_t`
  length, and `stubs.c` narrows the OCaml length to `uint32_t` or `int`
  before each call. A single feed of 4 GiB or more was therefore hashed
  modulo 2^32 bytes. The patch widens the 11 prototypes, the SHA3 loop
  index, the update glue and the BLAKE3 derive-key context length, and
  includes `<stddef.h>` in the seven headers that lacked `size_t`. The
  algorithms and all other C lines are unchanged upstream sources.
- `src-c/native/stubs.c` also passes the BLAKE3 XOF output length as a
  `size_t`. Upstream narrows it with `Int_val`, so `BLAKE3.get_xof` and
  `get_xof_into_bytes` wrote out of bounds for 2 GiB or more and left all
  but the low 32 bits of a 4 GiB or longer request unwritten.
- `src-c/digestif.ml` preserves portable functor boundaries and implements
  the raw helpers. BLAKE3 uses a 32-byte digest and 64-byte HMAC block.
- `src/digestif_by.ml` and `src/digestif_bi.ml` mark their externals
  portable. `Digestif_by.blit` is a global wrapper around OxCaml's
  local-taking `Bytes.blit`, and string readers serve the lookup tables.
- `src-ocaml/digestif.ml` retains the corresponding helper interface and
  requires portable hash cores. Unkeyed BLAKE2 contexts start from a fresh
  empty key rather than the shared `Bytes.empty`.
- `src-ocaml/baijiu_*.ml` declare their cores portable. Read-only lookup
  tables are strings or immutable arrays, local operator externals are
  portable and partially applied `feed` functions are eta-expanded. The
  algorithms are otherwise unchanged.
- `test/test_cve.ml` expects `ec66be1e...` for the 2^32 + 1 byte SHA3-224
  case. Upstream expects `bdd51672...`, the digest of one zero byte, which
  only the truncating C backend produced.

The compiler checks both backends' portable declarations. On OxCaml
5.4.0+ox9 each backend passes its 695 upstream cases, 694 of them quick, and
both slow SHA3 CVE-2022-37454 cases. The corrected 2^32 + 1 byte digest
matches Python's `hashlib`.

The length patch was checked with single feeds of 2^32 + 17 and
3 * 2^30 + 7 patterned bytes through the bigstring, bytes and string entry
points of all 16 C hashes. Each digest equals the chunked feed and an
independent implementation: `hashlib`, RHash for Whirlpool and BLAKE3, and
OpenSSL for Keccak-256. The OCaml backend agrees on the larger input. BLAKE3
derive-key contexts and XOF outputs of 2^31 + 9 and 2^32 + 5 bytes match the
OCaml backend.

`Int_val` remains for the BLAKE2 key length and the XOR helper length. Both
are bounded by their OCaml callers.

`wip/d10/test/compat/digestif_probe.ml` checks the added portable BLAKE3
helpers against the upstream module API.

On refresh, merge against the recorded upstream commit, retain these patches,
run `@@vendor/digestif/test/c/runtest` and
`@@vendor/digestif/test/ocaml/runtest` explicitly and run the D10, Siftbot and
HTTPz consumer tests. Recursive vendor aliases skip the upstream tests. Those
aliases pass `--quick-tests`, so also run both `test_cve.exe` binaries
without it. Each run needs 4 GiB of memory and takes about one minute on the
C backend and three on the OCaml backend. If upstream widens the lengths,
drop the C patch and keep the expectation that agrees with `hashlib`.

Digestif - Hash algorithms in C and OCaml
=========================================

Digestif is a toolbox which implements hashes:

 * MD5
 * SHA1
 * SHA2
 * SHA3
 * WHIRLPOOL
 * BLAKE2B
 * BLAKE2S
 * BLAKE3
 * RIPEMD160

Digestif uses a trick about linking and let the end-user to choose which
implementation he wants to use. We provide 2 implementations:

 * C implementation with `digestif.c`
 * OCaml implementation with `digestif.ocaml`
 
Both are well-tested. However, OCaml implementation is slower than the C
implementation.

**Note**: The linking trick requires `digestif.c` or `digestif.ocaml` to be the
first of your dependencies.

Documentation: https://mirage.github.io/digestif/

Contact: Romain Calascibetta `<romain.calascibet ta@gmail.com>`

## Install & Usage

The library is available on [OPAM](https://opam.ocaml.org/packages/digestif/). You can install it via:
```sh
$ opam install digestif
```

This is a simple program which implements `sha1sum`:
```sh
$ cat >sha1sum.ml <<EOF
let sum ic =
  let tmp = Bytes.create 0x1000 in
  let rec go ctx = match input ic tmp 0 0x1000 with
    | 0 -> Digestif.SHA1.get ctx
    | len ->
      let ctx = Digestif.SHA1.feed_bytes ctx ~off:0 ~len tmp in
      go ctx
    | exception End_of_file -> Digestif.SHA1.get ctx in
  go Digestif.SHA1.empty

let () = match Sys.argv with
  | [| _; filename; |] when Sys.file_exists filename ->
    let ic = open_in filename in
    let hash = sum ic in
    close_in ic ; print_endline (Digestif.SHA1.to_hex hash)
  | [| _ |] ->
    let hash = sum stdin in
    print_endline (Digestif.SHA1.to_hex hash)
  | _ -> Format.eprintf "%s [<filename>]\n%!" Sys.argv.(0)
EOF
$ cat >dune <<EOF
(executable
 (name sha1sum)
 (libraries digestif))
EOF
$ dune exec ./sha1sum.exe -- sha1sum.ml
fe6e6639a817c23857b507e2d833ec776f23f327
```

## API

For each hash, we implement the same API which is referentially transparent.
Then, on the top of these, we reflect functions (like `digesti` or `hmaci`) with
GADT - however, conversion from GADT to hash type is not possible (but you can
_destruct_ GADT with `to_raw_string`).

## Equal/Compare function

We deciced to protect users to timing-attack. In this case, `Digestif.equal` (by
[eqaf](https://github.com/mirage/eqaf.git) package) compares hashes in
constant-time.

However, we provide `unsafe_compare` function too which is __not__ a constant
time function. In some contexts, like `ocaml-git`, we don't care about timing
attack and we use `unsafe_compare` - then, we need to make a wrap where we
rename `unsafe_compare` to `compare` to be able to use it in some functors like
`Map.Make` or `Set.Make`.

It's little annoying to do that but it forces the user to get the right question
about security issues. So, please, don't ask to rename this function.

## MirageOS

Of course, this package is available to be used on MirageOS (both
implementations). User is able to compile `digestif.ocaml` with `js_of_ocaml`
and this package is platform agnostic.

## Build Requirements

 * OCaml >= 4.03.0 (may be less but need test)
 * `base-bytes` meta-package
 * `base-bigarray` meta-package
 * `dune` to build the project
 
If you want to compile the test program, you need:

 * `alcotest`

## Credits

This work is from the [nocrypto](https://github.com/mirleft/nocrypto) library
and the Vincent hanquez's work in
[ocaml-sha](https://github.com/vincenthz/ocaml-sha).

All credits appear in the begin of files and this library is motivated by two
reasons:
  * delete the dependancy with `nocrypto` if you don't use the encryption (and
    common) part
  * aggregate all hashes functions in one library
