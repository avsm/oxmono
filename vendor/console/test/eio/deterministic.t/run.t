CONSOLE_DETERMINISTIC_CLOCK, set to any value but the empty string, makes
the time of every Console_eio.run depend on the program alone, so a
transcript of a program's progress holds nothing of how fast the machine
ran it. The examples/progress program sleeps on Console_eio.clock, fifteen
times 0.2 seconds one after the other, so its time reads 3.0s. The example
takes three seconds.

  $ export CONSOLE_DETERMINISTIC_CLOCK=1

On a pipe:

  $ ../../../examples/progress/main.exe 2>&1 | cat
  [-] RUN make
  [-] RUN make > sha256:abcd...
    # RUN make:   CC src/file0.c
    # RUN make:   CC src/file1.c
    # RUN make:   CC src/file2.c
    # RUN make:   CC src/file3.c
    # RUN make:   CC src/file4.c
    # RUN make:   CC src/file5.c
  [+] RUN make > sha256:abcd...  2.00 GiB / 2.00 GiB  3.0s
  [+] RUN make  5/5  3.0s
  [+] Building 3.0s (2/2)

On a terminal, the screen it leaves:

  $ ../tty.exe ../../../examples/progress/main.exe
  [+] RUN make             ######################                       5/5   3.0s
    +-- [+] sha256:abcd... ######################       2.00 GiB / 2.00 GiB   3.0s
  [+] Building 3.0s (2/2)

The examples/columns program sleeps on Console_eio.clock: two of its runs
print the same bytes, and its two builds, which sleep at once, take the
time of the longer, 26 sleeps of 0.11 seconds. With keep_ended, the steps
stay for its last frame, which ends with the builds side by side.

  $ ../../../examples/columns/main.exe > first 2>&1
  $ ../../../examples/columns/main.exe > second 2>&1
  $ cmp first second
  $ grep -e '] macos  ' -e Building first
  [+] macos  2.9s
  [+] linux                                    1.9s  [+] macos                                    2.9s
  [+] Building 2.9s (9/9)

A sleep wakes at the time it started plus its duration, and sleeps at once
overlap, whatever the program does between them: two fibers that sleep 0.1
seconds four times and 0.13 seconds three times end at 0.4 seconds, whether
one of them works before each sleep or not.

  $ ../piped.exe sleeps 0 2> idle
  $ ../piped.exe sleeps 200000000 2> busy
  $ cmp idle busy
  $ cat idle
  {"time":0,"event":"start","id":0,"task":["build"]}
  {"time":0.4,"event":"end","id":0,"task":["build"],"status":"succeeded","elapsed":0.4}
  {"time":0.4,"event":"end","status":"succeeded","elapsed":0.4}

The empty string leaves the clock running:

  $ CONSOLE_DETERMINISTIC_CLOCK= ../../../examples/progress/main.exe 2>&1 | grep -c ' 0\.0s'
  0
  [1]
