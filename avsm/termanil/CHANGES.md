# Changes

- Pull before task edits and immediately commit and push with the Git backend.
  Share backend policy, private-file writes and draft locking with the libraries.

- Synchronize tasks through the selected Dooit backend, including Git and
  explicit local-only storage.

- Show done and cancelled task rows in muted grey while retaining selection
  highlighting.

- Keep done and cancelled tasks below unfinished tasks after completion and
  synchronization, preserving selection when the list reorders.

- Expose the terminal input queue for other Bonsai frontends to reuse paste
  and focus-boundary handling.

Add a Bonsai terminal client for JMAP mail, native Sortal contacts and Dooit
with linked tasks, reply drafts, identity signatures and a persistent demo.
