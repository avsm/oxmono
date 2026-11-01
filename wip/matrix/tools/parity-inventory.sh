#!/usr/bin/env bash
# Produce the bounded, mechanical side-by-side API inventory used by TODO P2.2.
#
# The Rust checkout need not have the requested commit checked out: every Rust
# file is read with `git show COMMIT:path`. The OCaml side is read from the
# requested commit as well, so a dirty worktree cannot accidentally enter an
# inventory. This is intentionally a textual inventory, not a semantic API
# or behavioural-parity check.
# shellcheck disable=SC2016 # Markdown backticks in printf format strings.
set -euo pipefail

usage() {
  local status=${1:-2}
  cat >&2 <<'EOF'
usage: tools/parity-inventory.sh --rust-commit COMMIT [options]

Generate Markdown on stdout. Options:
  --rust-repo PATH       Rust repository (default: ../matrix-rust-sdk)
  --rust-commit OBJECT   Rust matrix-sdk commit (required; must name a commit)
  --ocaml-repo PATH      OCaml repository (default: this checkout)
  --ocaml-commit OBJECT  OCaml commit (default: HEAD)
  --check FILE            regenerate and compare stdout with FILE
  --help
EOF
  exit "$status"
}

script_dir=$(cd -- "$(dirname -- "$0")" && pwd -P)
default_ocaml_repo=$(cd -- "$script_dir/.." && pwd -P)
rust_repo="$default_ocaml_repo/../matrix-rust-sdk"
ocaml_repo="$default_ocaml_repo"
rust_commit=""
ocaml_commit="HEAD"
ocaml_commit_supplied=0
check_file=""

while (($#)); do
  case "$1" in
    --rust-repo)
      (($# >= 2)) || usage
      rust_repo=$2
      shift 2
      ;;
    --rust-commit)
      (($# >= 2)) || usage
      rust_commit=$2
      shift 2
      ;;
    --ocaml-repo)
      (($# >= 2)) || usage
      ocaml_repo=$2
      shift 2
      ;;
    --ocaml-commit)
      (($# >= 2)) || usage
      ocaml_commit=$2
      ocaml_commit_supplied=1
      shift 2
      ;;
    --check)
      (($# >= 2)) || usage
      check_file=$2
      shift 2
      ;;
    --help|-h)
      usage 0
      ;;
    *)
      printf 'parity-inventory: unknown option: %s\n' "$1" >&2
      usage
      ;;
  esac
done

[[ -n "$rust_commit" ]] || {
  printf 'parity-inventory: --rust-commit is required; do not inventory a moving ref\n' >&2
  usage
}

git_in() {
  local repo=$1
  shift
  git -C "$repo" "$@"
}

git_in "$rust_repo" rev-parse --git-dir >/dev/null
git_in "$ocaml_repo" rev-parse --git-dir >/dev/null

# Resolve abbreviated IDs to immutable object IDs before reading any files.
rust_sha=$(git_in "$rust_repo" rev-parse --verify "$rust_commit^{commit}")
ocaml_sha=$(git_in "$ocaml_repo" rev-parse --verify "$ocaml_commit^{commit}")

git_in "$rust_repo" cat-file -e "$rust_sha:crates/matrix-sdk/Cargo.toml"

# Keep the command self-contained and copy/pasteable from the OCaml checkout.
# Paths are shell-quoted, while the resolved SHA makes the Rust input fixed.
shell_quote() {
  local value=$1
  value=${value//\'/\'\\\'\'}
  printf "'%s'" "$value"
}
regen_command="tools/parity-inventory.sh --ocaml-repo $(shell_quote "$ocaml_repo") --rust-repo $(shell_quote "$rust_repo") --rust-commit $(shell_quote "$rust_sha")"
if ((ocaml_commit_supplied)); then
  regen_command+=" --ocaml-commit $(shell_quote "$ocaml_sha")"
fi

tmp_output=""
if [[ -n "$check_file" ]]; then
  tmp_output=$(mktemp "${TMPDIR:-/tmp}/parity-inventory.XXXXXX")
  trap 'rm -f "$tmp_output"' EXIT
  # Re-run without --check so the comparison covers exactly stdout generation.
  check_args=(--ocaml-repo "$ocaml_repo" --rust-repo "$rust_repo" --rust-commit "$rust_sha")
  if ((ocaml_commit_supplied)); then
    check_args+=(--ocaml-commit "$ocaml_commit")
  fi
  "$0" "${check_args[@]}" >"$tmp_output"
  if ! cmp -s "$tmp_output" "$check_file"; then
    printf 'parity-inventory: %s is stale (regenerate with:\n  %s\n' \
      "$check_file" "$regen_command" >&2
    exit 1
  fi
  printf 'parity-inventory: %s is up to date\n' "$check_file" >&2
  exit 0
fi

rust_show() {
  git_in "$rust_repo" show "$rust_sha:$1"
}

ocaml_show() {
  git_in "$ocaml_repo" show "$ocaml_sha:$1"
}

rust_feature_names() {
  rust_show crates/matrix-sdk/Cargo.toml | awk '
    BEGIN { section = 0 }
    /^\[features\][[:space:]]*$/ { section = 1; next }
    /^\[/ { section = 0 }
    section && /^[A-Za-z0-9_.-]+[[:space:]]*=/ {
      line = $0
      sub(/[[:space:]]*=.*/, "", line)
      print line
    }
  '
}

rust_default_definition() {
  rust_show crates/matrix-sdk/Cargo.toml | awk '
    BEGIN { section = 0; found = 0 }
    /^\[features\][[:space:]]*$/ { section = 1; next }
    /^\[/ && section { exit }
    section && /^default[[:space:]]*=/ { found = 1 }
    found {
      line = $0
      gsub(/[[:space:]]+/, " ", line)
      sub(/^ /, "", line)
      sub(/ $/, "", line)
      printf "%s ", line
      if (index(line, "]") != 0) exit
    }
  ' | sed -E 's/[[:space:]]+/ /g; s/^ //; s/ $//'
}

rust_public_items() {
  rust_show "$1" | awk '
    # Deliberately only match `pub `, not pub(crate), pub(super), or pub(in).
    /^[[:space:]]*pub[[:space:]]/ {
      line = $0
      sub(/^[[:space:]]*pub[[:space:]]+/, "", line)
      gsub(/[[:space:]]+/, " ", line)
      sub(/^async[[:space:]]+/, "", line)
      kind = line
      sub(/[[:space:]].*/, "", kind)
      rest = line
      sub(/^[^[:space:]]+[[:space:]]*/, "", rest)
      if (kind == "use") {
        sub(/[[:space:];].*/, "", rest)
        print "use:" rest
      } else if (kind ~ /^(fn|struct|enum|trait|type|const|static|mod|union|macro_rules!|macro)$/) {
        sub(/[<{(=;:].*/, "", rest)
        sub(/[[:space:]].*/, "", rest)
        print kind ":" rest
      }
    }
  '
}

ocaml_public_items() {
  ocaml_show "$1" | awk '
    /^[[:space:]]*(val|type|exception|module|class|include)([[:space:]:%]|$)/ {
      line = $0
      sub(/^[[:space:]]+/, "", line)
      kind = line
      sub(/[[:space:]].*/, "", kind)
      rest = line
      sub(/^[^[:space:]]+[[:space:]]*/, "", rest)
      if (kind == "module" && rest ~ /^type[[:space:]]/) {
        sub(/^type[[:space:]]+/, "", rest)
        sub(/[[:space:]:=].*/, "", rest)
        print "module-type:" rest
      } else if (kind == "class" && rest ~ /^type[[:space:]]/) {
        sub(/^type[[:space:]]+/, "", rest)
        sub(/[[:space:]:=].*/, "", rest)
        print "class-type:" rest
      } else {
        if (kind == "type" && rest ~ /^nonrec[[:space:]]/) sub(/^nonrec[[:space:]]+/, "", rest)
        sub(/[[:space:]:;(=].*/, "", rest)
        print kind ":" rest
      }
    }
  '
}

emit_entry() {
  local path=$1
  shift
  local item
  local line_len=$((4 + ${#path}))
  printf -- '- `%s`' "$path"
  if (($# == 0)); then
    printf ': (no direct public declarations)\n'
    return
  fi
  item=$1
  shift
  if ((line_len + 2 + ${#item} <= 120)); then
    printf ': %s' "$item"
    line_len=$((line_len + 2 + ${#item}))
  else
    printf ':\n  %s' "$item"
    line_len=$((2 + ${#item}))
  fi
  for item in "$@"; do
    if ((line_len + 2 + ${#item} > 120)); then
      printf ',\n  %s' "$item"
      line_len=$((2 + ${#item}))
    else
      printf ', %s' "$item"
      line_len=$((line_len + 2 + ${#item}))
    fi
  done
  printf '\n'
}

mapfile -t rust_sources < <(git_in "$rust_repo" ls-tree -r --name-only "$rust_sha" -- crates/matrix-sdk/src | awk '/\.rs$/ { print }')
mapfile -t ocaml_interfaces < <(git_in "$ocaml_repo" ls-tree -r --name-only "$ocaml_sha" -- lib | awk '/^lib\/.*\.mli$/ { print }')
mapfile -t rust_features < <(rust_feature_names)

default_features=$(rust_default_definition)
mapfile -t rust_default_features < <(
  printf '%s\n' "$default_features" | grep -oE '"[^"]+"' | tr -d '"'
)
declare -A rust_default_feature_set=()
for feature in "${rust_default_features[@]}"; do
  rust_default_feature_set["$feature"]=1
done
if ((${#rust_sources[@]} == 0)); then
  printf 'parity-inventory: matrix-sdk has no Rust sources at %s\n' "$rust_sha" >&2
  exit 1
fi

{
  printf '# Mechanical parity inventory\n\n'
  printf '> This is a bounded, textual inventory for parity-audit bookkeeping. '
  printf 'It is not a semantic API comparison or a claim of behavioural parity.\n\n'
  printf -- '- OCaml repository commit: `%s`\n' "$ocaml_sha"
  printf -- '- Rust repository commit: `%s`\n' "$rust_sha"
  printf -- '- Rust crate: `crates/matrix-sdk`\n'
  printf -- '- Regeneration command: `%s > PARITY_INVENTORY.md`\n\n' "$regen_command"

  printf '## Rust Cargo features\n\n'
  printf 'The `default` declaration is recorded separately from opt-in features.\n\n'
  printf -- '- Declared default: `%s`\n' "${default_features:-<none>}"
  printf -- '- Enabled by default:\n'
  for feature in "${rust_default_features[@]}"; do
    printf '  - `%s`\n' "$feature"
  done
  printf -- '- Opt-in-only features:\n'
  for feature in "${rust_features[@]}"; do
    if [[ "$feature" != default && -z "${rust_default_feature_set[$feature]:-}" ]]; then
      printf '  - `%s`\n' "$feature"
    fi
  done
  printf '\n'

  printf '## Rust source modules and direct public items\n\n'
  printf 'Each source path is listed once; item names are extracted from lines beginning with `pub `.\n'
  printf 'This does not expand macros or conditional compilation and does not resolve re-exports.\n\n'
  printf -- '- Source files: `%d`\n\n' "${#rust_sources[@]}"
  for source in "${rust_sources[@]}"; do
    mapfile -t items < <(rust_public_items "$source")
    emit_entry "$source" "${items[@]}"
  done
  printf '\n## OCaml public interfaces\n\n'
  printf 'Only tracked `lib/**/*.mli` files are included; declaration names are extracted mechanically.\n'
  printf 'This does not expand functors/includes or interpret generated interfaces.\n\n'
  printf -- '- Interface files: `%d`\n\n' "${#ocaml_interfaces[@]}"
  for interface in "${ocaml_interfaces[@]}"; do
    mapfile -t items < <(ocaml_public_items "$interface")
    emit_entry "$interface" "${items[@]}"
  done
}
