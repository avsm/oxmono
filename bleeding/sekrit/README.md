# Sekrit

Sekrit resolves secret references and stores named values using authenticated
ChaCha20-Poly1305 encryption. Each write uses a fresh random 96-bit nonce. The
version and logical context are authenticated. The random 256-bit master key
is stored separately from the encrypted vault.

```
sekrit init
sekrit put mail/password --file private-password.txt
sekrit list
```

Applications can use `sekrit:mail/password` in supported credential-reference
fields. `put --replace` rotates a value. `export NAME --file NEW_FILE` is the
explicit plaintext export operation. Neither listing nor errors prints values.

The vault defaults to `$XDG_DATA_HOME/sekrit/vault.sekrit`. The key defaults to
`$XDG_CONFIG_HOME/sekrit/master.key`. Standard XDG home-directory defaults apply.
`SEKRIT_STORE_DIR`, `SEKRIT_KEY_FILE`, `--store`, and `--key-file` override these
paths. Keep the key outside encrypted storage. Back up the key separately.
Losing it loses access to the secrets. An existing vault never causes a missing
key to be regenerated.

Encryption protects a copied vault without its key. It does not protect against
an attacker who can read both files or inspect the running process. Files must
be owned and private. OCaml strings cannot be reliably erased from memory.

`sekrit.eio` is the application interface. It accepts filesystem capabilities,
provides switch-scoped store handles, performs bounded Eio secret-file reads,
and offloads crypto and directory fsync to worker threads. Store data is read
and written through pinned directory capabilities. File and named references
resolve afresh. `Sekrit_eio.Source.snapshot` explicitly retains a resolved value
for a connection. Applications choose when to rebuild that connection after
rotation.

```ocaml
Eio.Switch.run (fun sw ->
    let store = Sekrit_eio.Store.open_ ~sw ~root ~key_file in
    let source =
      Sekrit_eio.Source.v ~fs ~backend:(Sekrit_eio.Store.backend store)
        (Sekrit.Reference.named "mail/password")
    in
    Sekrit.Secret.with_value (Sekrit_eio.Source.get source) connect)
```

Here `root`, `key_file` and `fs` are directory-relative Eio paths supplied by
the application. Initialize storage separately with `sekrit init`. Store handles
expire with the switch. Concurrent writes on one handle serialize. Contending
handles return a busy error. Atomic publication and initial key creation are
protected from cancellation once started.

`Sekrit.Backend.v` accepts a resolver without granting write or enumeration
access. A future macOS Keychain backend can implement that interface. Give tools
only the authenticated service capabilities they need, not the secret store.

The synchronous `sekrit` library supports native Unix consumers. Its
`Sealed_file` module also encrypts application-owned structured configuration,
using a stable authenticated context and the central master key. Applications
own their schema and migration from plaintext. Reads never silently migrate or
rewrite credentials.

To migrate existing service configurations on a host, use
the [migration script](scripts/migrate-services.sh), described in the
[migration guide](scripts/README.md). It previews by default,
imports and verifies credentials, retains private backups, and can restart
explicitly named user services after a successful migration.
