A dune spawned while another dune holds the workspace does not fail. It hands
the request to the running instance and exits without ever opening the RPC
socket okit waits for. okit reads the child's first line of output instead of
waiting out its window, and the workspace keeps the plain tools.

The socket the running server opened is removed first, so that okit finds a
workspace that looks free and spawns.

  $ WS=$(mktemp -d /tmp/humpty-expect.XXXXXX)
  $ cd $WS
  $ cat > dune-project <<'EOF'
  > (lang dune 3.21)
  > EOF
  $ mkdir lib
  $ cat > lib/dune <<'EOF'
  > (library (name fix))
  > EOF
  $ cat > lib/fix.ml <<'EOF'
  > let x = 1
  > EOF

The server is started with dune's own variables dropped, as okit drops them,
so that it serves this workspace and not the one running the cram test. Its
pid goes in a file: the shell that starts it is not the shell that kills it.

  $ env -u INSIDE_DUNE -u DUNE_BUILD_DIR -u DUNE_RPC \
  >   dune build --passive-watch-mode > server.out 2>&1 &
  $ echo $! > server.pid
  $ n=0; while [ ! -e _build/.rpc/dune ] && [ $n -lt 300 ]; do sleep 0.1; n=$((n+1)); done
  $ test -e _build/.rpc/dune && echo socket up
  socket up
  $ rm _build/.rpc/dune

The run is quiet, because the warning the interface shows about a workspace
without dune tools goes to standard error unscrubbed, and cram reads both
streams. The status line below says the same thing.

  $ humpty-cpu expect -q --dir "$WS" <<'EOF'
  > write {"cap":"","path":"a.txt","content":"hi\n"}
  > EOF
  okit: no dune tools. the dune this session started in $WS exited without opening its RPC socket at $WS/_build/.rpc/dune: Error: Another Dune instance is currently running. Aborting...
  > write {"cap":"","path":"a.txt","content":"hi\n"}
  wrote 3 bytes to a.txt

The status a killed dune reports is dune's own business and has changed
between versions, so the server is waited for rather than pinned.

  $ kill $(cat server.pid)
  $ wait $(cat server.pid) 2>/dev/null; true
  $ cd /
  $ rm -rf $WS
