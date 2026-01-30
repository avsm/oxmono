(** Example: JSON Feed Echo

    Reads a JSON Feed from stdin, parses it, and outputs it to stdout. Useful
    for testing round-trip parsing and identifying any changes during
    serialization/deserialization.

    Usage: feed_echo < feed.json cat feed.json | feed_echo > output.json diff
    <(cat feed.json | feed_echo) feed.json

    Exit codes: 0 - Success 1 - Parsing or encoding failed *)

let echo_feed () =
  (* Create a bytesrw reader from stdin *)
  let stdin = Bytesrw.Bytes.Reader.of_in_channel In_channel.stdin in

  (* Parse the JSON feed *)
  match Jsonfeed.decode ~locs:true stdin with
  | Error err ->
      Format.eprintf "Parsing failed:\n  %s\n%!" (Jsont.Error.to_string err);
      exit 1
  | Ok feed -> (
      (* Encode the feed back to stdout *)
      match Jsonfeed.to_string ~minify:false feed with
      | Error err ->
          Format.eprintf "Encoding failed:\n  %s\n%!"
            (Jsont.Error.to_string err);
          exit 1
      | Ok json ->
          print_string json;
          print_newline ();
          exit 0)

let () = echo_feed ()
