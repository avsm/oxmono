CONSOLE_CAST=FILE records what a run writes on the terminal to FILE, in
asciicast v2, rather than on the output: a header, then one
[time, "o", data] line per write, at its time on the virtual clock. The terminal is $COLUMNS by $LINES, with no pty
behind it, and the program's sleeps take no real time.

  $ export CONSOLE_DETERMINISTIC_CLOCK=1 TERM=xterm-256color LANG=en_US.UTF-8
  $ export COLUMNS=80 LINES=24
  $ CONSOLE_CAST=columns.cast ../../../examples/columns/main.exe
  $ head -1 columns.cast
  {"version":2,"width":80,"height":24,"env":{"TERM":"xterm-256color"}}

The two builds of examples/columns take the time of the longer, 26 sleeps
of 0.11 seconds, when the last write comes:

  $ tail -1 columns.cast | cut -c 1-12
  [2.86,"o","\

Two recordings are the same bytes:

  $ CONSOLE_CAST=again.cast ../../../examples/columns/main.exe
  $ cmp columns.cast again.cast

CONSOLE_CAST implies CONSOLE_DETERMINISTIC_CLOCK:

  $ CONSOLE_DETERMINISTIC_CLOCK= CONSOLE_CAST=alone.cast \
  >   ../../../examples/columns/main.exe
  $ cmp columns.cast alone.cast

The recording leaves the screen that the program leaves on a terminal:

  $ ../tty.exe ../../../examples/columns/main.exe > terminal
  $ ../tty.exe --cast columns.cast > recorded
  $ cmp terminal recorded
  $ grep Building recorded
  [+] Building 2.9s (9/9)

A program that prints without a run is recorded from Console_tty.setup
on, each write at time zero, and the recording leaves its screen too:

  $ CONSOLE_CAST=charts.cast ../../../examples/charts/main.exe
  $ tail -n +2 charts.cast | cut -c 1-6 | sort -u
  [0,"o"
  $ ../tty.exe ../../../examples/charts/main.exe > terminal
  $ ../tty.exe --cast charts.cast > recorded
  $ cmp terminal recorded
  $ grep -c Machine recorded
  1

A file that cannot be opened ends the program with an error, before it
runs:

  $ CONSOLE_CAST=missing/run.cast ../../../examples/columns/main.exe
  main.exe: CONSOLE_CAST: cannot record: missing/run.cast: No such file or directory
  [1]
