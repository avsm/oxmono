In plain mode (no terminal), the display writes one line when a task starts and
one when it ends, each line of output at once after its task's name, and the
header last, as a summary. A task that fails repeats the last lines of its
window under its line. This is the text a log collector ingests.

  $ dump_progress.exe
  • foo.ml
  ✓ foo.ml  0.1s
  • bar.ml
  ✓ bar.ml  0.1s
  • baz.ml
  ✓ baz.ml  0.1s
  • qux.ml
    › qux.ml: line 1
    › qux.ml: line 2
    › qux.ml: line 3
  ✗ qux.ml  0.1s  failed
  ✗ Processing 0.5s (4/4)
