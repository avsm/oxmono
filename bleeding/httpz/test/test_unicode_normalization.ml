(* These closures must retain access to the module-level lookup tables. *)
let normalize form text : string =
  (Uunf_string.normalize_utf_8 : _ @ portable) form text

let text scalars =
  let buffer = Buffer.create 16 in
  List.iter (fun scalar ->
    Buffer.add_utf_8_uchar buffer (Uchar.of_int scalar)) scalars;
  Buffer.contents buffer

let check form source expected =
  Alcotest.(check string) "normal form" (text expected)
    (normalize form (text source))

let test_normal_forms () =
  check `NFC [0x65; 0x301] [0xe9];
  check `NFD [0xe9] [0x65; 0x301];
  check `NFKC [0xfb03] [0x66; 0x66; 0x69];
  check `NFKD [0x212b] [0x41; 0x30a];
  check `NFC [0x1100; 0x1161; 0x11a8] [0xac01];
  check `NFD [0xac01] [0x1100; 0x1161; 0x11a8];
  check `NFD [0x61; 0x315; 0x300] [0x61; 0x300; 0x315]

let test_decomposition_ownership () =
  let scalar = Uchar.of_int 0xe9 in
  let expected = Uunf.decomp scalar in
  let copy = Uunf.decomp scalar in
  copy.(0) <- 0;
  Alcotest.(check (array int)) "table survives caller mutation"
    expected (Uunf.decomp scalar);
  check `NFD [0xe9] [0x65; 0x301]

let test_portable_idna () =
  let to_ascii = (Punycode_idna.to_ascii : _ @ portable) in
  Alcotest.(check string) "NFC host"
    (to_ascii (text [0xe9] ^ ".example"))
    (to_ascii (text [0x65; 0x301] ^ ".example"))

let shared_input = text [0x65; 0x301; 0x1100; 0x1161; 0x11a8]

let test_domains () =
  let work () =
    List.init 1000 (fun _ -> normalize `NFC shared_input)
  in
  let first = Domain.Safe.spawn work in
  let second = Domain.Safe.spawn work in
  let first = Domain.join first in
  let second = Domain.join second in
  Alcotest.(check (list string)) "independent accumulators" first second;
  Alcotest.(check string) "shared table result"
    (text [0xe9; 0xac01]) (List.hd first)

let () =
  Alcotest.run "portable Unicode normalization" [
    "normalization", [
      "four forms and ordering", `Quick, test_normal_forms;
      "decomposition ownership", `Quick, test_decomposition_ownership;
      "portable IDNA", `Quick, test_portable_idna;
      "two domains", `Quick, test_domains;
    ];
  ]
