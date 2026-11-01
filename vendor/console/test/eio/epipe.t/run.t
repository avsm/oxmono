A reader that stops early closes the pipe under the program. A closed pipe on
the display's output ends the display's lines and nothing else: the program
goes on and exits as it returns.

  $ { ../piped.exe 50000 2>&1 >/dev/null; echo "exit $?" > status; } | head -1
  [-] step 1
  $ cat status
  exit 0

Standard output is the program's: once its reader is gone, the program's own
writes meet the closed pipe as they do without console, here as an uncaught
exception.

  $ { ../piped.exe bare 50000 2>/dev/null; echo "exit $?" > status; } | head -1
  result 1
  $ cat status
  exit 2
  $ { ../piped.exe 50000 2>/dev/null; echo "exit $?" > status; } | head -1
  result 1
  $ cat status
  exit 2
  $ { ../piped.exe 50000 2>&1; echo "exit $?" > status; } | head -1
  result 1
  $ cat status
  exit 2
