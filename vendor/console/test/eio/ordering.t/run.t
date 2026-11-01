Without a terminal, a program under Console_eio.run keeps the order in which it
wrote its outputs merged with 2>&1: its results, its task lines, its Logs
messages in a task or not, and what it prints on standard error itself (in
ASCII, the test having no UTF-8 locale):

  $ ../piped.exe notes 2 2>&1 | cat
  result 1
  [-] step 1
    step 1: working on 1
  [+] step 1  0.0s
  ! after 1
  note 1
  result 2
  [-] step 2
    step 2: working on 2
  [+] step 2  0.0s
  ! after 2
  note 2
