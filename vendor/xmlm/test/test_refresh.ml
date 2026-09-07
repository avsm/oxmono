let () =
  let source = "<?xml version=\"1.0\" encoding=\"ISO-8859-15\"?><a>\164\166\168\180\184\188\189\190</a>" in
  let input = Xmlm.make_input (`String (0, source)) in
  assert (Xmlm.input input = `Dtd None);
  assert (Xmlm.input input = `El_start (("", "a"), []));
  assert (Xmlm.input input = `Data "€ŠšŽžŒœŸ");
  assert (Xmlm.input input = `El_end);
  assert (Xmlm.eoi input);
  print_endline "Xmlm ISO-8859-15 decoding passed"
