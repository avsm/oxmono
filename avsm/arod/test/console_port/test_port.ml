let check_color (r, g, b) =
  let expected =
    Array.to_list Reference_color.named
    |> List.assoc (Reference_color.nearest_named (r, g, b))
  in
  let actual = Console.Color.(to_rgb (downsample `Ansi_16 (rgb r g b))) in
  if actual <> expected then failwith (Printf.sprintf "Colour mismatch %d %d %d" r g b)

let check_clusters text =
  let expected = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun xs x -> x :: xs) [] text |> List.rev in
  let actual = ref [] in
  Glyph.String.iter_graphemes
    (fun ~offset ~len -> actual := String.sub text offset len :: !actual) text;
  assert (List.rev !actual = expected)

let () =
  for i = 0 to 255 do check_color (Console.Color.to_rgb (Console.Color.ansi_256 i)) done;
  for r = 0 to 15 do for g = 0 to 15 do for b = 0 to 15 do
    check_color (r * 17, g * 17, b * 17)
  done done done;
  List.iter check_clusters [""; "abc"; "a\r\nb"; "é🇬🇧👩🏽‍💻"; "क्‍ष"; String.make 4092 'a' ^ "👩🏽‍💻" ];
  let random = Random.State.make [| 7; 39 |] in
  for _ = 1 to 1000 do
    let b = Buffer.create 128 in
    for _ = 1 to 30 do
      let n = Random.State.int random 0x110000 in
      if Uchar.is_valid n then Buffer.add_utf_8_uchar b (Uchar.of_int n)
    done;
    check_clusters (Buffer.contents b)
  done;
  for n = 0 to 255 do
    let c = Char.chr n in
    assert (Console_compat.Char.Ascii.is_hex_digit c = K7_compat.Char.Ascii.is_hex_digit c)
  done;
  assert (K7_compat.Sys.signal_to_int Sys.sigint = 2);
  assert (K7_compat.Sys.signal_to_int Sys.sigterm = 15);
  assert (K7_compat.Sys.signal_to_int 9 = 9);
  print_endline "Console colour and grapheme differential checks and K7 signal conversion passed"
