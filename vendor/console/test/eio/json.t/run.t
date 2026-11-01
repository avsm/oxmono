With --progress=json, what the program prints on standard output while a
task is current becomes an output event of that task when standard output
writes where the display does, as in 2>&1, so that every line of the stream
is a JSON event (README, Known limitations):

  $ ../piped.exe json-stdout 2>&1
  {"time":0,"event":"start","id":0,"task":["build"]}
  {"time":0,"event":"output","id":0,"task":["build"],"text":"from stdout"}
  {"time":0,"event":"output","id":0,"task":["build"],"text":"from stderr"}
  {"time":0,"event":"end","id":0,"task":["build"],"status":"succeeded","elapsed":0}
  {"time":0,"event":"end","status":"succeeded","elapsed":0}

Console.Task.Event.jsont reads each of these events back, and writes it
as it was:

  $ ../piped.exe json-stdout 2>&1 | ../events.exe
  5 events read back
