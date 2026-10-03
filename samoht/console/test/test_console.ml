module type Surface = sig end

let exposed name _module = Alcotest.(check string) name name name

let test_public_surface () =
  exposed "Color" (module Console.Color : Surface);
  exposed "Style" (module Console.Style : Surface);
  exposed "Width" (module Console.Width : Surface);
  exposed "Guide" (module Console.Guide : Surface);
  exposed "Rule" (module Console.Rule : Surface);
  exposed "Table" (module Console.Table : Surface);
  exposed "Display" (module Console.Display : Surface)

(* The library's terminal-geometry contract says that malformed UTF-8 renders as
   a replacement glyph and that terminal controls in content cannot move the
   cursor. Every widget applies that to what it lays out. A caller printing a
   string of its own -- a subprocess's stderr, a peer's name off the network --
   needs the same rule over a plain string, and the only way to reach it was to
   wrap the string in a Span and unwrap the result, so callers write private
   ANSI filters instead and each one draws its own line between a control
   character and a printable one. *)
let test_sanitize () =
  Alcotest.(check string)
    "a tab, a newline and an escape all become spaces" "a b c d"
    (Console.sanitize ~keep_newlines:false "a\tb\nc\027d");
  Alcotest.(check string)
    "a newline is a row boundary, so it is kept by default" "a b\nc d"
    (Console.sanitize "a\tb\nc\027d");
  Alcotest.(check string)
    "a malformed sequence becomes the replacement glyph" "a\u{FFFD}b"
    (Console.sanitize "a\255b");
  Alcotest.(check string)
    "a C1 control, which UTF-8 hides behind two bytes, becomes a space" "a b"
    (Console.sanitize "a\u{009B}b");
  Alcotest.(check string)
    "DEL becomes a space" "a b"
    (Console.sanitize "a\127b");
  Alcotest.(check string)
    "well-formed text, wide glyphs included, is returned unchanged"
    "\u{4E16}\u{754C} ok"
    (Console.sanitize "\u{4E16}\u{754C} ok")

(* A caller that pages a subprocess's output wants the colours it already
   carries and none of the rest of ANSI. ECMA-48 8.3.117 SELECT GRAPHIC
   RENDITION is the only control sequence that cannot move the cursor, erase a
   region or name a resource, so it is the only one a sanitizer may forward and
   still promise what {!Console.sanitize} promises. Recognising it means parsing
   it: CSI, parameter bytes, final byte 'm', and this library accepts a strict
   subset of the parameter bytes ECMA-48 5.4 allows -- the digits and ';' -- so
   the private forms reserved there and the ':' sub-parameter of ITU-T T.416
   are refused rather than forwarded on a guess. *)
let styles s = Console.sanitize_styles s

(* Everything that is not a strictly-parsed SGR is what {!Console.sanitize}
   makes of it, and one function decides that for both. *)
let neutralised what s =
  Alcotest.(check string) what (Console.sanitize s) (styles s)

let test_sanitize_styles_passes_sgr () =
  Alcotest.(check string)
    "a colour and its reset survive byte for byte" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "so does a 24-bit colour, which is eleven numeric parameters"
    "\027[38;2;255;0;0mred\027[0m"
    (styles "\027[38;2;255;0;0mred\027[0m");
  Alcotest.(check string)
    "an empty parameter list is SGR 0 and survives" "\027[mplain"
    (styles "\027[mplain");
  Alcotest.(check string)
    "a control inside styled text still becomes a space" "\027[31ma b\027[0m"
    (styles "\027[31ma\tb\027[0m");
  Alcotest.(check string)
    "and a newline still goes when it is not wanted" "\027[31ma b\027[0m"
    (Console.sanitize_styles ~keep_newlines:false "\027[31ma\nb\027[0m")

(* The parameter run is bounded, because an unbounded scan over untrusted bytes
   is a scan whoever wrote the bytes sizes. *)
let test_sanitize_styles_bounds_the_parameters () =
  let params n = String.make n '1' in
  Alcotest.(check string)
    "sixty-four parameter bytes are within the bound"
    ("\027[" ^ params 64 ^ "mx\027[0m")
    (styles ("\027[" ^ params 64 ^ "mx\027[0m"));
  neutralised "sixty-five are not, and neutralise" ("\027[" ^ params 65 ^ "mx");
  neutralised "nor does a sequence the string ends in the middle of"
    "tail\027[31"

let test_sanitize_styles_neutralises_the_rest () =
  neutralised "erase in display moves nothing through" "\027[2Jgone";
  neutralised "nor does cursor position" "\027[10;20Hgone";
  neutralised "nor a private parameter byte, whatever its final byte"
    "\027[?25lgone";
  neutralised "nor a colon sub-parameter, which is not the accepted grammar"
    "\027[38:2:255:0:0mred";
  neutralised "an OSC 8 hyperlink is a spoofing surface, not styling"
    "\027]8;;https://example.invalid/\027\\click\027]8;;\027\\";
  neutralised "a device control string carries whatever it likes"
    "\027Pq#0;2;0;0;0\027\\";
  neutralised "an escape that is not a control sequence at all" "\027(Bgone";
  (* C1 CSI is one code point, not ESC '[', and the ruling neutralises C1. *)
  neutralised "the C1 introducer does not open an SGR" "\u{009B}31mred"

(* The sanitized text cannot change the styling of what a caller prints after
   it, which is the half of {!Console.sanitize}'s promise a styled variant can
   still keep. SGR parameters apply left to right and 0 turns every attribute
   off, so a last parameter of 0 -- or an empty one, which SGR reads as 0 --
   already leaves the terminal in its default state and nothing is appended. *)
let test_sanitize_styles_closes_what_it_opens () =
  Alcotest.(check string)
    "an unterminated colour is closed" "\027[31mred\027[0m"
    (styles "\027[31mred");
  Alcotest.(check string)
    "so is one whose last parameter sets an attribute" "\027[0;31mred\027[0m"
    (styles "\027[0;31mred");
  Alcotest.(check string)
    "a trailing reset is not doubled" "\027[31mred\027[0m"
    (styles "\027[31mred\027[0m");
  Alcotest.(check string)
    "nor is a reset that arrives as the last parameter" "\027[31;0mred"
    (styles "\027[31;0mred");
  let s = "\027[31mred\027[2Jgone\tand\u{009B}31m" in
  Alcotest.(check string)
    "and sanitizing twice changes nothing the first pass did not" (styles s)
    (styles (styles s))

(* no-color.org: a non-empty NO_COLOR turns colour off; an explicit --color
   wins; TERM unset, empty or dumb is no colour; else a terminal is styled. *)
let test_style_renderer () =
  let decide ?renderer ~is_tty pairs =
    match
      Console.style_renderer ?renderer
        ~getenv:(fun name -> List.assoc_opt name pairs)
        ~is_tty ()
    with
    | `Ansi_tty -> "ansi"
    | `None -> "none"
  in
  let term = ("TERM", "xterm-256color") in
  let check label expected got = Alcotest.(check string) label expected got in
  check "terminal" "ansi" (decide ~is_tty:true [ term ]);
  check "pipe" "none" (decide ~is_tty:false [ term ]);
  check "NO_COLOR" "none" (decide ~is_tty:true [ term; ("NO_COLOR", "1") ]);
  check "empty NO_COLOR" "ansi" (decide ~is_tty:true [ term; ("NO_COLOR", "") ]);
  check "TERM dumb" "none" (decide ~is_tty:true [ ("TERM", "dumb") ]);
  check "TERM empty" "none" (decide ~is_tty:true [ ("TERM", "") ]);
  check "TERM unset" "none" (decide ~is_tty:true []);
  check "--color=always" "ansi"
    (decide ~renderer:`Ansi_tty ~is_tty:false [ ("NO_COLOR", "1") ]);
  check "--color=never" "none" (decide ~renderer:`None ~is_tty:true [ term ])

let suite =
  ( "console",
    [
      Alcotest.test_case "style renderer" `Quick test_style_renderer;
      Alcotest.test_case "public surface" `Quick test_public_surface;
      Alcotest.test_case "sanitize" `Quick test_sanitize;
      Alcotest.test_case "sanitize_styles passes SGR" `Quick
        test_sanitize_styles_passes_sgr;
      Alcotest.test_case "sanitize_styles bounds the parameters" `Quick
        test_sanitize_styles_bounds_the_parameters;
      Alcotest.test_case "sanitize_styles neutralises the rest" `Quick
        test_sanitize_styles_neutralises_the_rest;
      Alcotest.test_case "sanitize_styles closes what it opens" `Quick
        test_sanitize_styles_closes_what_it_opens;
    ] )
