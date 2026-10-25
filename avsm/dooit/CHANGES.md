# Changes

- Share backend-aware writes and synchronization through `Dooit.Client`, with
  explicit local, pushed and pending results and scoped store transactions.

- Reject oversized Git objects before reading their bodies and bound subprocess
  output while terminating abandoned descendant processes.

- Show done and cancelled tasks in muted grey in coloured CLI output.

- List unfinished tasks before done and cancelled tasks, including search
  results, and display due dates in CLI task summaries.

- Simplify the CLI around init, status and sync, with task and email subcommands.
  Select Git, WebDAV or local storage explicitly and show human output by default.

- Pull before agent task writes, then commit and push immediately. Preserve
  pending writes for safe retries when synchronization fails.

- Add explicit Git setup, clone, pull and push, defaulting to
  `fork.recoil.org:git/dooit`, with validated notes and private local journals.
- Add reusable Agentkit task tools with revision checks, bounded reads and
  retry-safe writes. Serialize writers within and across processes.
- Preserve relative root choices during config setup and require strictly
  formatted due dates.

- Allow email capture to set a structured due date while retaining source
  deduplication and existing-task metadata.

Add Markdown tasks with tags, extensible links and JMAP email capture. Sync
through configured WebDAV storage with app passwords, dry runs and conflicts.
