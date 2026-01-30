(** Example: JSON Feed Validator

    Reads a JSON Feed from stdin and validates it.

    Usage: feed_validator < feed.json cat feed.json | feed_validator

    Exit codes: 0 - Feed is valid 1 - Feed parsing failed 2 - Feed validation
    failed *)

let validate_stdin () =
  let stdin = Bytesrw.Bytes.Reader.of_in_channel In_channel.stdin in
  match Jsonfeed.decode ~locs:true stdin with
  | Error err ->
      Format.eprintf "Parsing failed:\n  %s\n%!" (Jsont.Error.to_string err);
      exit 1
  | Ok feed -> (
      match Jsonfeed.validate feed with
      | Ok () ->
          Format.printf "Feed is valid\n%!";
          Format.printf "\nFeed details:\n";
          Format.printf "  Title: %s\n" (Jsonfeed.title feed);
          Format.printf "  Version: %s\n" (Jsonfeed.version feed);
          (match Jsonfeed.home_page_url feed with
          | Some url -> Format.printf "  Home page: %s\n" url
          | None -> ());
          Format.printf "  Items: %d\n" (List.length (Jsonfeed.items feed));
          exit 0
      | Error errors ->
          Format.eprintf "Validation failed:\n%!";
          List.iter (fun err -> Format.eprintf "  - %s\n%!" err) errors;
          exit 2)

let () = validate_stdin ()
