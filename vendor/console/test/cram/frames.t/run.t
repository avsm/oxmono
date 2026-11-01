A TTY progress session replayed through a terminal emulator: each frame is the
reconstructed screen (real control characters applied), so a duplicated or
stranded row would show up directly. Geometry is pinned for a stable diff.

On a roomy terminal the whole tree, its per-row log windows, the rows that
ended and the summary below them all fit, and every row appears exactly once.
A line of a task's output lives in the window beneath the row that made it,
and only there: one that ages out of the window ("cc a.o" below) is gone, and
a row that succeeds takes its lines with it. A row that ends stays under its
parent, marked, until its top task ends; then the whole tree goes above the
region, one line per row, and the summary is printed last:

  $ COLUMNS=70 LINES=20 frames.exe
  --- tree with one node + a log ---
  ⠋ app   0.0s
    › planning
  • Build 0.0s (0/1)
  --- two children, each streaming logs ---
  ⠋ app             0.0s
    › planning
    ├── ⠋ resolve   0.0s
    │   › fetch index
    └── ⠋ compile   0.0s
        › cc a.o
        › cc b.o
  • Build 0.0s (0/3)
  --- resolve finished, under its parent, siblings still live ---
  ⠋ app             0.0s
    › planning
    ├── ✓ resolve   0.0s
    └── ⠋ compile   0.0s
        › cc a.o
        › cc b.o
  • Build 0.0s (1/3)
  --- more logs after the row closed ---
  ⠋ app             0.0s
    › planning
    › still working
    ├── ✓ resolve   0.0s
    └── ⠋ compile   0.0s
        › cc b.o
        › cc c.o
        › link
  • Build 0.0s (1/3)
  --- all finished ---
  ✓ app             0.0s
    ├── ✓ resolve   0.0s
    └── ✓ compile   0.0s
  ✓ Build 0.0s (3/3)

On a short terminal the live region would not fit. Rather than let a redraw
scroll a stale copy into the scrollback (which reads as a duplicated row), the
rows that ended under a task fold into one line that counts them, then the
oldest log lines give way, and the summary stays last:

  $ COLUMNS=70 LINES=8 frames.exe
  --- tree with one node + a log ---
  ⠋ app   0.0s
    › planning
  • Build 0.0s (0/1)
  --- two children, each streaming logs ---
  ⠋ app             0.0s
    ├── ⠋ resolve   0.0s
    │   › fetch index
    └── ⠋ compile   0.0s
        › cc a.o
        › cc b.o
  • Build 0.0s (0/3)
  --- resolve finished, under its parent, siblings still live ---
  ⠋ app             0.0s
    › planning
    ├── ✓ resolve   0.0s
    └── ⠋ compile   0.0s
        › cc a.o
        › cc b.o
  • Build 0.0s (1/3)
  --- more logs after the row closed ---
  ⠋ app             0.0s
    › still working
    ├── ✓ 1 done
    └── ⠋ compile   0.0s
        › cc c.o
        › link
  • Build 0.0s (1/3)
  --- all finished ---
  ✓ app             0.0s
    ├── ✓ resolve   0.0s
    └── ✓ compile   0.0s
  ✓ Build 0.0s (3/3)
