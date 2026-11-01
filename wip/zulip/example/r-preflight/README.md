# Check a hosted account

This recipe authenticates, prints the server version and account identity, and
lists subscribed channels. It makes only GET requests to Zulip. The shared CLI
also loads local profile/state files; `--zuliprc` imports local credentials.

```sh
dune exec example/r-preflight/preflight.exe -- --profile eeg --zuliprc /path/to/zuliprc
```

Check that the URL, account and channel list are what you intended before
starting a bot. See the [hosted bot guide](../../HOSTED_BOTS.md) for deployment
and a deliberate test-channel smoke check.
