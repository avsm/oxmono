Without a terminal, the display flushes standard output before each line it
writes, so that the two outputs merged with 2>&1 keep the order in which the
program wrote them:

  $ ordering.exe 2>&1 | cat
  result 1
  • fetch
  result 2
    › fetch: fetched
  ✓ fetch  0.0s
  result 3
