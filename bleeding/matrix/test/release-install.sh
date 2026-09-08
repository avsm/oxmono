#!/usr/bin/env bash
# Validate an extracted release archive in a fresh, isolated opam root/switch.
# Usage: MATRIX_HTTPZ_SOURCE=https://host/repo.git#<40-hex-sha> \
#          test/release-install.sh /absolute/path/to/extracted-release
set -euo pipefail
SOURCE="${1:?pass the extracted release source directory}"
SOURCE="$(cd "$SOURCE" && pwd)"
[ -f "$SOURCE/matrix-chat.opam" ] || { echo 'matrix-chat.opam is absent' >&2; exit 1; }
: "${MATRIX_HTTPZ_SOURCE:?set a published immutable HTTPz Git URL with all five packages}"
case "$MATRIX_HTTPZ_SOURCE" in
  https://*'#'*) ;;
  *) echo 'MATRIX_HTTPZ_SOURCE must be an HTTPS Git URL ending in #<commit>' >&2; exit 1 ;;
esac
REVISION="${MATRIX_HTTPZ_SOURCE##*#}"
[[ "$REVISION" =~ ^[a-fA-F0-9]{40}$ ]] || { echo 'HTTPz commit must be a full 40-digit SHA' >&2; exit 1; }
RUN_DIR="$(mktemp -d "${TMPDIR:-/tmp}/matrix-chat-release-install.XXXXXXXX")"
exec > >(tee "$RUN_DIR/install.log") 2>&1
printf 'Installation evidence: %s\n' "$RUN_DIR"
git clone --quiet "${MATRIX_HTTPZ_SOURCE%#*}" "$RUN_DIR/httpz"
git -C "$RUN_DIR/httpz" checkout --quiet "$REVISION"
for package in httpz fetch fetch-httpz proffer proffer-httpz; do
  [ -f "$RUN_DIR/httpz/$package.opam" ] || { echo "Published revision lacks $package.opam"; exit 1; }
done
export OPAMROOT="$RUN_DIR/opam"
unset OPAMSWITCH
opam init --bare --no-setup --disable-sandboxing -y default https://opam.ocaml.org
opam switch create release ocaml-base-compiler.5.5.0 -y
export OPAMSWITCH=release
for package in httpz fetch fetch-httpz proffer proffer-httpz; do
  opam pin add --no-action -y "$package" "$MATRIX_HTTPZ_SOURCE"
done
opam install "$SOURCE" --with-test -y
for library in matrix-chat.proto matrix-chat.client matrix-chat.eio matrix-chat.cli matrix-chat.ui matrix-chat.ui.sqlite matrix-chat.bot; do
  opam exec -- ocamlfind query "$library"
done
mkdir "$RUN_DIR/consumer"
cat > "$RUN_DIR/consumer/dune-project" <<'DUNE'
(lang dune 3.21)
DUNE
cat > "$RUN_DIR/consumer/dune" <<'DUNE'
(executable
 (name smoke)
 (libraries matrix-chat.proto matrix-chat.client matrix-chat.eio matrix-chat.cli matrix-chat.ui matrix-chat.ui.sqlite matrix-chat.bot))
DUNE
cat > "$RUN_DIR/consumer/smoke.ml" <<'OCAML'
let () =
  ignore (Matrix_proto.Id.User_id.of_string_exn "@release:example.org");
  ignore Matrix_client.Encryption.empty_state;
  ignore Matrix_eio.Client.is_logged_in;
  print_endline "installed Matrix modules linked"
OCAML
opam exec -- dune exec --root "$RUN_DIR/consumer" ./smoke.exe
opam exec -- omatrix --help=plain
opam exec -- matrix-bot --help=plain
opam list --installed > "$RUN_DIR/installed.txt"
printf 'Fresh-switch installation passed. Logs: %s\n' "$RUN_DIR"
