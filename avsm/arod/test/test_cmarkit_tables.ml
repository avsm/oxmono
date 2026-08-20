(* [cmarkit] is vendored under vendor/ and patched so that its parsing tables
   can be read from a portable function. The four tables that used to be a
   Set or a Map are now sorted immutable arrays that a binary search reads.
   Dune skips aliases under a vendored directory, so the vendored copy has no
   test of its own that runs. This one does.

   Everything here is checked against the generated arrays the tables are
   built from, so nothing needs updating when a re-vendor brings new Unicode
   data. A binary search that accepts a key it should not, misses one it
   should find, cannot reach the first or the last entry, or gives up on a
   one-element window, fails here. So does a tag dropped from or added to the
   two HTML block start conditions.

   avsm/arod/test/test_md_golden.ml is the other half of the guarantee. It
   pins rendered bytes, but its four documents reach only a handful of these
   entries. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* Code points, minus the surrogate range that [Uchar.of_int] rejects. *)
let fold_uchars f acc =
  let acc = ref acc in
  for u = 0 to 0x10FFFF do
    if u < 0xD800 || u > 0xDFFF then acc := f !acc (Uchar.unsafe_of_int u)
  done;
  !acc

let distinct a = List.length (List.sort_uniq Int.compare (Array.to_list a))

(* [last_wins a] maps each key of [a] to the value of its last occurrence,
   which is what folding [Map.add] over [a] produced. *)
let last_wins a =
  let h = Hashtbl.create (Array.length a * 2) in
  Array.iter (fun (k, v) -> Hashtbl.replace h k v) a;
  h

let () =
  (* Whitespace. Every listed code point answers true, and no other one
     does. *)
  Array.iter
    (fun u ->
       check "a listed whitespace code point answers true"
         (Cmarkit_data.is_unicode_whitespace (Uchar.unsafe_of_int u)))
    Cmarkit_data_uchar.whitespace;
  check "no unlisted code point answers whitespace"
    (fold_uchars
       (fun n u -> if Cmarkit_data.is_unicode_whitespace u then n + 1 else n) 0
     = distinct Cmarkit_data_uchar.whitespace);

  (* Punctuation, the same two ways. *)
  Array.iter
    (fun u ->
       check "a listed punctuation code point answers true"
         (Cmarkit_data.is_unicode_punctuation (Uchar.unsafe_of_int u)))
    Cmarkit_data_uchar.punctuation;
  check "no unlisted code point answers punctuation"
    (fold_uchars
       (fun n u -> if Cmarkit_data.is_unicode_punctuation u then n + 1 else n) 0
     = distinct Cmarkit_data_uchar.punctuation);

  (* Case folding. Every listed code point folds to its listed replacement,
     and no other one folds at all. *)
  let folds = last_wins Cmarkit_data_uchar.case_fold in
  Hashtbl.iter
    (fun u f ->
       check "a listed code point folds to its replacement"
         (Cmarkit_data.unicode_case_fold (Uchar.unsafe_of_int u) = Some f))
    folds;
  check "no unlisted code point folds"
    (fold_uchars
       (fun n u ->
          match Cmarkit_data.unicode_case_fold u with
          | None -> n
          | Some _ -> n + 1)
       0
     = Hashtbl.length folds);

  (* HTML entities. Every name resolves to its replacement, and a name with a
     character appended resolves only if it is itself a name. A binary search
     that compared a prefix rather than the whole string would pass the first
     of these and fail the second. *)
  let entities = last_wins Cmarkit_data_html.entities in
  Hashtbl.iter
    (fun name rep ->
       check "a listed entity resolves to its replacement"
         (Cmarkit_data.html_entity name = Some rep))
    entities;
  Hashtbl.iter
    (fun name _ ->
       let longer = name ^ "x" in
       check "an entity name with a character appended resolves only if named"
         (Cmarkit_data.html_entity longer
          = Hashtbl.find_opt entities longer))
    entities;
  check "an entity name truncated by one character resolves only if named"
    (Hashtbl.fold
       (fun name _ ok ->
          let short = String.sub name 0 (String.length name - 1) in
          ok
          && Cmarkit_data.html_entity short = Hashtbl.find_opt entities short)
       entities true);
  check "an unknown entity name resolves to nothing"
    (Cmarkit_data.html_entity "" = None
     && Cmarkit_data.html_entity "zzzz" = None
     && Cmarkit_data.html_entity "Amp" = None);
  check "entity lookup is case sensitive"
    (Cmarkit_data.html_entity "amp" = Some "&"
     && Cmarkit_data.html_entity "AMP" = Some "&"
     && Cmarkit_data.html_entity "nbsp" = Some "\194\160"
     && Cmarkit_data.html_entity "NBSP" = None);

  (* The two HTML block start conditions, which used to be string sets and
     are now matches. A tag in either one interrupts a paragraph. A tag in
     neither is parsed as inline raw HTML instead. *)
  let html tag =
    let md = Printf.sprintf "before\n<%s>x</%s>\nafter\n" tag tag in
    Cmarkit_html.of_doc ~safe:false (Cmarkit.Doc.of_string ~strict:false md)
  in
  let interrupts tag =
    String.equal (html tag)
      (Printf.sprintf "<p>before</p>\n<%s>x</%s>\nafter\n" tag tag)
  in
  let does_not_interrupt tag =
    String.equal (html tag)
      (Printf.sprintf "<p>before\n<%s>x</%s>\nafter</p>\n" tag tag)
  in
  let cond_1_tags = [ "pre"; "script"; "style"; "textarea" ] in
  let cond_6_tags =
    [ "address"; "article"; "aside"; "base"; "basefont"; "blockquote"; "body";
      "caption"; "center"; "col"; "colgroup"; "dd"; "details"; "dialog"; "dir";
      "div"; "dl"; "dt"; "fieldset"; "figcaption"; "figure"; "footer"; "form";
      "frame"; "frameset"; "head"; "header"; "hr"; "html"; "iframe"; "legend";
      "li"; "link"; "main"; "menu"; "menuitem"; "nav"; "noframes"; "ol";
      "optgroup"; "option"; "p"; "param"; "section"; "source"; "summary";
      "table"; "tbody"; "td"; "tfoot"; "th"; "thead"; "title"; "tr"; "track";
      "ul" ]
  in
  (* [h1] to [h6] are in condition 6 upstream and cannot be reached, because
     the tag name is scanned with [Ascii.is_letter] and stops at the digit.
     The arms are kept for fidelity with the set they replaced. This pins
     that they are unreachable, so that a future reader who notices them does
     not take their behaviour to have changed. *)
  let heading_tags = [ "h1"; "h2"; "h3"; "h4"; "h5"; "h6" ] in
  let other_tags =
    [ "notatag"; "pr"; "prex"; "u"; "uli"; "h7"; "h0"; "span"; "em"; "b"; "a";
      "img"; "video"; "tabl"; "tables"; "l"; "z" ]
  in
  check "every condition 6 tag interrupts a paragraph and runs to the blank"
    (List.for_all interrupts cond_6_tags);
  check "every condition 1 tag interrupts a paragraph and ends at its close"
    (List.for_all
       (fun tag ->
          String.equal (html tag)
            (Printf.sprintf "<p>before</p>\n<%s>x</%s>\n<p>after</p>\n" tag tag))
       cond_1_tags);
  check "a numbered heading tag reaches neither condition"
    (List.for_all does_not_interrupt heading_tags);
  check "no tag outside the two conditions interrupts a paragraph"
    (List.for_all does_not_interrupt other_tags);

  (* The tables as the parser reaches them. *)
  check "entities are decoded in rendered output"
    (String.equal
       (Cmarkit_html.of_doc ~safe:false
          (Cmarkit.Doc.of_string "&copy; &nbsp; &amp; &#35; &#X22; &nope;"))
       "<p>\194\169 \194\160 &amp; # &quot; &amp;nope;</p>\n");
  check "reference labels match after case folding"
    (String.equal
       (Cmarkit_html.of_doc ~safe:false
          (Cmarkit.Doc.of_string "[\xE1\xBA\x9E]\n\n[SS]: /x"))
       "<p><a href=\"/x\">\xE1\xBA\x9E</a></p>\n");
  check "a Unicode punctuation flank opens emphasis"
    (String.equal
       (Cmarkit_html.of_doc ~safe:false
          (Cmarkit.Doc.of_string "\xC2\xBF*a*\xC2\xA1"))
       "<p>\xC2\xBF<em>a</em>\xC2\xA1</p>\n");

  Printf.printf "test_cmarkit_tables: %d checks ok\n" !checks
