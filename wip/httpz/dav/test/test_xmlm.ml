let () =
  let source = "<?xml version=\"1.0\" encoding=\"ISO-8859-15\"?><a>\164\166\168\180\184\188\189\190</a>" in
  let input = Xmlm.make_input (`String (0, source)) in
  assert (Xmlm.input input = `Dtd None);
  assert (Xmlm.input input = `El_start (("", "a"), []));
  assert (Xmlm.input input = `Data "€ŠšŽžŒœŸ");
  assert (Xmlm.input input = `El_end);
  assert (Xmlm.eoi input);
  print_endline "Xmlm ISO-8859-15 decoding passed";
  List.iter (fun strip ->
    let input = Xmlm.make_input ~strip (`String (0,
      "<a v=\"  one  two\t\r\n &#x9;&#xA;&#xD;  \"/>")) in
    assert (Xmlm.input input = `Dtd None);
    assert (Xmlm.input input =
      `El_start (("", "a"), [("", "v"), "  one  two   \t\n\r  "]))
  ) [false; true];
  let b = Buffer.create 64 in
  let output = Xmlm.make_output (`Buffer b) in
  let signals = [`Dtd None;
    `El_start (("", "a"), [("", "v"), " \t\n\r  "]);
    `Data "\r\n\t"; `El_end] in
  List.iter (Xmlm.output output) signals;
  let input = Xmlm.make_input (`String (0, Buffer.contents b)) in
  List.iter (fun signal -> assert (Xmlm.input input = signal)) signals;
  assert (Xmlm.eoi input);
  print_endline "Xmlm attribute normalization and whitespace round trips passed";
  List.iter (fun source ->
    let input = Xmlm.make_input (`String (0, source)) in
    assert (try
      ignore (Xmlm.input input); ignore (Xmlm.input input); false
      with Xmlm.Error _ -> true))
    ["<x xmlns:xml='wrong'/>"; "<x xmlns:xmlns='DAV:'/>";
     "<x xmlns='http://www.w3.org/XML/1998/namespace'/>";
     "<x xmlns:p='http://www.w3.org/2000/xmlns/'/>";
     "<x xmlns:p='http://www.w3.org/XML/1998/namespace'/>"];
  print_endline "Xmlm reserved namespace checks passed"
