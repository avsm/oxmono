# Changes

Treat embedded vCard photos as authoritative and materialize them in the XDG
cache for file-based callers.

Keep store identity in local sync metadata. Cards use their UID and no longer
carry or require X-SORTAL-STORE.

Store contacts as native vCards under the XDG Sortal root. Retain unknown
metadata during typed edits and remove the legacy YAML contact backend.
