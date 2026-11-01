Quiet, a line the program leaves unended on the display's output does not
take the display's next line along: the line of a failed task starts a line
of its own.

A line too long to hold goes out unended:

  $ ../piped.exe quiet-long 2>&1 | sed 's/xxx*/x.../'
  x...
  [x] work  0.0s  failed

So does a question written on the display's output:

  $ echo me | ../piped.exe quiet-prompt 2>&1
  name? 
  [x] work  0.0s  failed
