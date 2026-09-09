# Local ATProto development

This stack runs a real PDS, PLC directory, PostgreSQL, Jetstream and a TLS
gateway. Accounts, DIDs, repository records and event delivery use local
services. It needs no live account, public domain or public relay. Image
downloads happen during preparation. Images are pinned by digest.

From the oxmono root:

```sh
python3 bleeding/atp/testbed/run.py pull
python3 bleeding/atp/testbed/run.py test
```

`test` starts the services, creates Alice and Bob if absent, verifies PLC and
HTTPS handle discovery, writes a Tangled record and waits for that exact
record on Jetstream. It leaves the services and accounts running for
development. Subsequent runs reuse the accounts and update the smoke record.
The account password is `local-password`. Both handles end in
`.pds.tangled.test`. The owner's DID is in `.state/owner-did`.

| Service | From the host | Inside Compose |
| --- | --- | --- |
| PDS | `http://127.0.0.1:2583` | `https://pds.tangled.test` |
| PLC | `http://127.0.0.1:2582` | `https://plc.tangled.test` |
| Jetstream | `ws://127.0.0.1:6008/subscribe` | `wss://jetstream.tangled.test/subscribe` |
| TLS gateway | `127.0.0.1:8443` with the appropriate hostname | port 443 |

For example:

```sh
curl http://127.0.0.1:2583/xrpc/_health
curl --cacert bleeding/atp/testbed/.state/certs/ca.crt \
  --resolve pds.tangled.test:8443:127.0.0.1 \
  https://pds.tangled.test:8443/xrpc/_health
```

The host does not need changes to `/etc/hosts` or its trust store. Docker aliases
provide the service and account hostnames inside the stack. The local CA is
trusted explicitly by clients that use HTTPS. DID documents correctly point
at the PDS's HTTPS name. A host-side client doing its own DID discovery needs
corresponding hostname routing, or can run on the Compose network.

`up` starts services without the smoke test. `down` stops the project and
retains its volumes. `reset` removes this project's data volumes, including
accounts and records. Generated keys and certificates stay in `.state` and
are excluded from Git. Back up that directory with the data volumes if the
development identities need to survive moving this stack.

```sh
python3 bleeding/atp/testbed/run.py down
python3 bleeding/atp/testbed/run.py up
docker compose -f bleeding/atp/testbed/compose.yml logs --tail=100
```

The Compose network is an ordinary private bridge and its host ports bind
loopback. Local endpoint configuration makes it self-contained. The optional
`check_isolation.py` experiment uses
stricter isolated bridges and local canaries, and is not part of normal
startup. Image pulls use an empty temporary Docker client configuration for
anonymous access to the public registries. Existing Docker credentials are
left alone.

The pinned PDS image uses `@atproto/pds` 0.5.27. Its actual configuration was
checked: PLC is explicitly local, crawler lists are empty, public Bluesky
appview and moderation/report proxy services are disabled, and no backup DNS
servers are specified. Jetstream subscribes directly to this PDS's
`com.atproto.sync.subscribeRepos`. PDS development mode permits the local
addresses and removes the need for invitation codes. It does not turn this
into a substitute production deployment.

The [spindle audit](../TANGLED-SPINDLE.md) describes the next layer: knot, an
OCaml job runner and Proffer CI endpoints, then Tangled appview. Those services
are not implemented by this Compose file. The smoke repository record names
future local knot and spindle hosts. It exercises ATP event delivery, not a
Git push or CI execution.
