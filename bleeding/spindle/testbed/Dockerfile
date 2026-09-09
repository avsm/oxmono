FROM ubuntu:26.04@sha256:2260313b31c8c011cd2eebe728008efac1b3982be73eb71348ea2648d2c0e09b
RUN apt-get update && apt-get install -y --no-install-recommends \
    git ca-certificates libssl3t64 libgmp10 libzstd1 \
    && rm -rf /var/lib/apt/lists/*
COPY .state/spindle /usr/local/bin/spindle
USER 1000:1000
ENTRYPOINT ["spindle"]
