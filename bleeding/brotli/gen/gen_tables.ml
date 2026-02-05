(* Generate dictionary.ml from dictionary.bin *)

let dictionary_path = "data/dictionary.bin"
let output_path = "src/dictionary.ml"

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let data = really_input_string ic n in
  close_in ic;
  data

let escape_string s =
  let buf = Buffer.create (String.length s * 4) in
  String.iter (fun c ->
    let code = Char.code c in
    if code >= 32 && code < 127 && c <> '"' && c <> '\\' then
      Buffer.add_char buf c
    else
      Printf.bprintf buf "\\%03d" code
  ) s;
  Buffer.contents buf

let () =
  let dict_data = read_file dictionary_path in
  let oc = open_out output_path in

  Printf.fprintf oc "(* Brotli static dictionary - auto-generated from dictionary.bin *)\n\n";

  Printf.fprintf oc "(* Dictionary size: %d bytes *)\n" (String.length dict_data);
  Printf.fprintf oc "let data = \"%s\"\n\n" (escape_string dict_data);

  Printf.fprintf oc "(* Word offsets by length (indices 0-24, only 4-24 are valid) *)\n";
  Printf.fprintf oc "let offset_by_length = [|\n";
  Printf.fprintf oc "  0;     0;     0;     0;     0;  4096;  9216; 21504; 35840; 44032;\n";
  Printf.fprintf oc "  53248; 63488; 74752; 87040; 93696; 100864; 104704; 106752; 108928; 113536;\n";
  Printf.fprintf oc "  115968; 118528; 119872; 121280; 122016\n";
  Printf.fprintf oc "|]\n\n";

  Printf.fprintf oc "(* Log2 of word count per length *)\n";
  Printf.fprintf oc "let size_bits_by_length = [|\n";
  Printf.fprintf oc "  0;  0;  0;  0; 10; 10; 11; 11; 10; 10;\n";
  Printf.fprintf oc "  10; 10; 10;  9;  9;  8;  7;  7;  8;  7;\n";
  Printf.fprintf oc "  7;  6;  6;  5;  5\n";
  Printf.fprintf oc "|]\n\n";

  Printf.fprintf oc "let min_word_length = 4\n";
  Printf.fprintf oc "let max_word_length = 24\n\n";

  Printf.fprintf oc "(* Get a word from the dictionary *)\n";
  Printf.fprintf oc "let get_word ~length ~index =\n";
  Printf.fprintf oc "  if length < min_word_length || length > max_word_length then\n";
  Printf.fprintf oc "    invalid_arg \"Dictionary word length out of range\";\n";
  Printf.fprintf oc "  let offset = offset_by_length.(length) + index * length in\n";
  Printf.fprintf oc "  String.sub data offset length\n";

  close_out oc;
  Printf.printf "Generated %s from %s (%d bytes)\n" output_path dictionary_path (String.length dict_data)
