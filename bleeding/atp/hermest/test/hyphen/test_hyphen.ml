module Item = Hyphen_lexicons.Com.Bad_example.Pocket.Defs

let () =
  let item : Item.item = { name = "x" } in
  let use : Hyphen_lexicons.Com.Example.Use.main = { item } in
  assert (use.item.name = "x");
  print_endline "PASS: hyphenated NSID authorities generate valid modules"
