# OwnTracks

OwnTracks message codecs, an mqttz adapter, GeoJSON export and Recorder HTTP
queries for OxCaml. The `owntracks` command provides `listen`, `geojson`,
`recorder`, `devices` and `init`.

```sh
opam exec --switch=5.2.0+ox -- dune build --profile release-check @bleeding/owntracks/all
_build/default/bleeding/owntracks/bin/main.exe init
_build/default/bleeding/owntracks/bin/main.exe listen --host mqtt.example.com --tls
_build/default/bleeding/owntracks/bin/main.exe geojson --device phone
_build/default/bleeding/owntracks/bin/main.exe geojson --track --duration 60
_build/default/bleeding/owntracks/bin/main.exe geojson --from 2026-01-01 --to 2026-01-02 \
  --user alice --device phone --recorder-url https://recorder.example.com
```

## Libraries

| Library | Interface |
|---|---|
| `owntracks` | `Owntracks`: messages, MQTT envelopes, Recorder codecs and GeoJSON |
| `owntracks.eio` | `Owntracks_eio`: receive and publish on a `Mqttz_eio.t` |
| `owntracks.config` | `Owntracks_config`: explicit TOML configuration |
| `owntracks.recorder` | `Owntracks_recorder_client`: bounded streaming queries using Fetch |

`Owntracks.Message.decode` accepts a borrowed `Mqttz.Slice.t`, including a
local slice. It reads the bytes directly through Jsont without rewriting or
copying the whole JSON payload. Decoded records and strings are owned by the
result. The caller must keep the input unchanged until decoding returns.
`Message.encode` writes owned bytes for publishing. Serialization buffers and
decoded values still allocate. No OwnTracks or mqttz source uses Cstruct.

```ocaml
let receive_location client =
  match Owntracks_eio.receive client with
  | Ok envelope ->
      (match Owntracks.Mqtt.message envelope with
       | Owntracks.Message.Location loc -> Some loc
       | _ -> None)
  | Error message -> failwith message
```

Use mqttz's plaintext or verified TLS connector to create the client. Its
bounded receive queue and operation deadlines apply to OwnTracks. The CLI
uses Fetch's httpz backend for Recorder HTTP and HTTPS requests.

## Configuration

The default file is `$XDG_CONFIG_HOME/owntracks/owntracks.toml`, falling back to
`$HOME/.config/owntracks/owntracks.toml`. `--config FILE` selects an explicit
file. An absent default file uses defaults. Invalid configuration and a missing
explicit file are errors. CLI options override the file. `init` creates the
file with mode 0600 and requires `--force` to overwrite it.

```toml
[owntracks]
topic = "owntracks/#"
default_device = "phone"

[[owntracks.devices]]
id = "phone"
name = "My Phone"

[owntracks.recorder]
url = "https://recorder.example.com"
# user = "api-user"
# password = "secret"

[mqtt]
host = "mqtt.example.com"
tls = true
version = "5.0"
# username = "alice"
# password = "secret"
```

`[mqtt]` uses `Mqttz_config.codec`, with a fresh client identifier when none
is configured. Both MQTT 3.1.1 and 5.0 are supported. HTTP Basic credentials
are separate from the OwnTracks `--user` query parameter.

Live tracks group locations by MQTT topic, keeping different users separate
when they use the same device ID. Each group becomes a timestamp-sorted
LineString, or a Point if only one location arrived. Multiple groups produce
a FeatureCollection. Collection defaults to 30 seconds and 100,000 locations.
`--duration` and `--max-points` adjust these limits. Historical queries use the
same timeout and point limit, plus a 16 MiB response limit.

## Checks

```sh
opam exec --switch=5.2.0+ox -- dune runtest --profile release-check --force \
  bleeding/mqttz bleeding/owntracks
opam exec --switch=5.2.0+ox -- dune build --profile release-check \
  @bleeding/mqttz/fmt @bleeding/owntracks/fmt
bleeding/owntracks/test/docker/run.sh
```

The Docker harness reuses mqttz's digest-pinned Mosquitto broker and runs its
full suite first. OwnTracks checks cover QoS 0/1/2 on both protocol versions,
malformed-message recovery, retained CLI messages, distinct user/device tracks,
configuration and collection bounds. A local HTTP fixture tests Recorder
queries, Basic authentication, streaming JSON and error responses. It does not
run a Recorder deployment. Containers, networks and temporary files are
removed on exit.

See [OXMONO.md](OXMONO.md) for provenance and interface changes, and
[REVIEW.md](REVIEW.md) for the review and supported scope.
