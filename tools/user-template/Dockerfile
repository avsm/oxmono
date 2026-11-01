# syntax=docker/dockerfile:1
FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3 AS build
RUN apt-get update && apt-get install -y --no-install-recommends \
    opam git ca-certificates curl build-essential autoconf m4 pkg-config \
    python3 unzip rsync bubblewrap libgmp-dev zlib1g-dev libzstd-dev \
    libproj-dev proj-data libblosc-dev libopenblas-dev liblapacke-dev \
    libcurl4-openssl-dev libffi-dev libssl-dev clang llvm-dev \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
COPY . .
RUN tools/bootstrap-ox.sh
# The selected context has no Git identity; this is a development image.
RUN git init -q && git add . && \
    opam exec --root=/tmp/oxmono-bootstrap/opam --switch=oxmono -- \
    _build/default/wip/ox/bin/main.exe dist bundle --prepared --user @OWNER@ \
      --output /opt/oxmono && \
    python3 tools/bundle-runtime.py /opt/oxmono

FROM debian:bookworm-slim@sha256:99c0b25c46d0214d5a2091092e7908907dcaecd6a962cde0ac700acee222b6f3
RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates tzdata git curl python3 openssh-client proj-data ncurses-term \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --create-home --uid 10001 oxmono \
    && mkdir -p /data/config /data/cache /data/state /data/share \
    && chown -R oxmono:oxmono /data
COPY --from=build /opt/oxmono /opt/oxmono
COPY tools/container-entrypoint.sh /usr/local/bin/oxmono-entrypoint
ENV PATH=/opt/oxmono/bin:$PATH \
    LD_LIBRARY_PATH=/opt/oxmono/lib/native \
    XDG_CONFIG_HOME=/data/config XDG_CACHE_HOME=/data/cache \
    XDG_STATE_HOME=/data/state XDG_DATA_HOME=/data/share
USER oxmono
WORKDIR /data
ENTRYPOINT ["oxmono-entrypoint"]
