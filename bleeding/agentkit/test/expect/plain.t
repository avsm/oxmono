A workspace with no dune-project gets the plain tools and says so on the
status line. No dune is started, so this one runs in the cram sandbox.

The last two calls ask to write outside the capability, upwards and by
absolute path. Each is refused in its own words, and neither file appears.

  $ humpty-cpu expect --dir . <<'EOF'
  > write {"cap":"","path":"b.txt","content":"hi\n"}
  > read {"cap":"","path":"b.txt"}
  > write {"cap":"","path":"../escape.txt","content":"no\n"}
  > write {"cap":"","path":"/escape.txt","content":"no\n"}
  > EOF
  okit: off (no dune-project)
  > write {"cap":"","path":"b.txt","content":"hi\n"}
  wrote 3 bytes to b.txt
  > read {"cap":"","path":"b.txt"}
  hi
  > write {"cap":"","path":"../escape.txt","content":"no\n"}
  Eio.Io Fs Permission_denied Unix_error (Capabilities insufficient, "openat", "../escape.txt"),
    opening <.:../escape.txt>
  > write {"cap":"","path":"/escape.txt","content":"no\n"}
  "/escape.txt" is outside this capability. Use open_dir to ask for access to it, then pass the name it returns as cap.

  $ cat b.txt
  hi
  $ test -e ../escape.txt && echo escaped || echo no escape
  no escape
  $ test -e /escape.txt && echo escaped || echo no escape
  no escape
