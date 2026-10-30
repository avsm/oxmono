# Runs the host-built spindle. OCaml executables link the host's glibc, so
# the base image must be at least as new as the build host's.
FROM archlinux:base@sha256:e82623936fa1a69700b48e98b058df6eaab18b737c27473a7234a40fdecb9413
RUN pacman -Sy --noconfirm --needed git ca-certificates gmp openssl zstd brotli \
    && pacman -Scc --noconfirm
COPY .state/spindle /usr/local/bin/spindle
USER 1000:1000
ENTRYPOINT ["spindle"]
