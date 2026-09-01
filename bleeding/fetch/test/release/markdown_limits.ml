let repeat n s = String.concat "" (List.init n (fun _ -> s))
let inputs =
  [ "plain nesting", String.make 2000 '[' ^ "x" ^ String.make 2000 ']';
    "hidden nesting", repeat 40 "[`]`" ^ "x" ^ String.make 40 ']';
    "escaped brackets", repeat 2000 "\\[\\]";
    "malformed links", repeat 2000 "[link](broken";
    "large shallow input", repeat 50000 "one [link](https://example.com)\n\n" ]

let check adapter codec =
  List.iter (fun (name, source) ->
    match Httpz.Media.decode codec source with
    | Ok _ -> ()
    | Error error -> failwith (adapter ^ " " ^ name ^ ": " ^ Httpz.Media.error_to_string error)) inputs

let () =
  (* The external timeout bounds synchronous parser CPU work. Use a permissive
     local bracket limit so these cases actually reach the corrected parser. *)
  check "Fetch" (Fetch.Markdown.markdown ~max_bracket_depth:max_int ());
  check "Proffer" (Proffer.Markdown.markdown ~max_bracket_depth:max_int ());
  print_endline "Markdown adversarial parser regressions passed"
