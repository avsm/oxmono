(* [xmlm] is vendored under vendor/ and patched so that its interface is
   callable from the portable context arod and sortal parse and render feeds
   in. Dune skips aliases under a vendored directory, so the vendored copy's
   own tests never run. This one does, and it fails if a re-vendor drops the
   patch. It also pins the two tables the patch turned into code, the UTF-8
   byte lengths and the five predefined entities. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* [el] and [data] are called from the portable closures below, so they must
   not close over anything nonportable. *)
let el ((_, l), _) kids = "[" ^ l ^ ":" ^ String.concat "" kids ^ "]"
let data d = d

(* The ascriptions are the point. None of these compile unless [xmlm.mli]
   still carries its [@@ portable] annotations, because a portable closure
   reaches a module-level value only if that value is portable, and every
   [Xmlm] name below is one. *)

let render : (unit -> string) @ portable =
 fun () ->
  let b = Buffer.create 64 in
  let o = Xmlm.make_output ~decl:false (`Buffer b) in
  Xmlm.output o (`Dtd None);
  Xmlm.output o (`El_start (("", "greeting"), [ (("", "lang"), "en") ]));
  Xmlm.output o (`Data "a & b");
  Xmlm.output o `El_end;
  Buffer.contents b

let render_tree : (unit -> string) @ portable =
 fun () ->
  let b = Buffer.create 64 in
  let o = Xmlm.make_output ~decl:false (`Buffer b) in
  let frag = function
    | `Doc kids -> `El ((("", "doc"), []), kids)
    | `Leaf s -> `Data s
  in
  Xmlm.output_doc_tree frag o (None, `Doc [ `Leaf "x" ]);
  Buffer.contents b

let parse : (string -> string) @ portable =
 fun s -> snd (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input (`String (0, s))))

let parse_error : (string -> string) @ portable =
 fun s ->
  let i = Xmlm.make_input (`String (0, s)) in
  try ignore (Xmlm.input_doc_tree ~el ~data i : Xmlm.dtd * string); "no error"
  with Xmlm.Error ((line, col), e) ->
    Printf.sprintf "%d:%d %s" line col (Xmlm.error_message e)

let parse_entity : (string -> string) @ portable =
 fun s ->
  let entity = function "nbsp" -> Some "\194\160" | _ -> None in
  snd
    (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input ~entity (`String (0, s))))

(* The [`Fun] and [`Channel] arms of [source] and [dest] carry a callback, an
   [in_channel] and an [out_channel]. None of the three stops a portable
   closure, as long as it makes the channel or the callback itself. A
   [Buffer.t] or a channel held at module level is a different matter: it is
   mutable data and does not cross contention, so it cannot be read from a
   portable closure at all. That is stdlib's rule about a shared sink, not
   xmlm's, and passing one in as an argument is unaffected. *)

let via_fun : (string -> string) @ portable =
 fun s ->
  let b = Buffer.create 16 in
  let next =
    let n = ref 0 in
    fun () ->
      if !n >= String.length s then raise End_of_file
      else (
        let c = Char.code (String.get s !n) in
        incr n;
        c)
  in
  let o =
    Xmlm.make_output ~decl:false
      (`Fun (fun c -> Buffer.add_char b (Char.chr c)))
  in
  Xmlm.output_doc_tree
    (fun t -> t)
    o
    (Xmlm.input_doc_tree
       ~el:(fun tag kids -> `El (tag, kids))
       ~data:(fun d -> `Data d)
       (Xmlm.make_input (`Fun next)));
  Buffer.contents b

let via_channel : (string -> string) @ portable =
 fun s ->
  let path = Filename.temp_file "test_xmlm" ".xml" in
  let oc = open_out_bin path in
  let o = Xmlm.make_output ~decl:false (`Channel oc) in
  Xmlm.output_doc_tree
    (fun t -> t)
    o
    (Xmlm.input_doc_tree
       ~el:(fun tag kids -> `El (tag, kids))
       ~data:(fun d -> `Data d)
       (Xmlm.make_input (`String (0, s))));
  close_out oc;
  let ic = open_in_bin path in
  let r = snd (Xmlm.input_doc_tree ~el ~data (Xmlm.make_input (`Channel ic))) in
  close_in ic;
  Sys.remove path;
  r

(* [reserved_ns] does not compile unless the module-level strings [Xmlm] binds
   for the two reserved namespaces cross portability and contention. A portable
   closure reads a module-level value only if its type does. Passing one in as
   an argument would prove nothing. *)
let reserved_ns : (unit -> string) @ portable =
 fun () -> Xmlm.ns_xml ^ " " ^ Xmlm.ns_xmlns

let () =
  check "a portable closure writes signals to a buffer"
    (String.equal (render ()) "<greeting lang=\"en\">a &amp; b</greeting>");
  check "a portable closure writes a document tree"
    (String.equal (render_tree ()) "<doc>x</doc>");
  check "a portable closure reads a document tree"
    (String.equal (parse "<doc><p>hi</p></doc>") "[doc:[p:hi]]");
  check "a portable closure drives a callback source and destination"
    (String.equal (via_fun "<doc><p>hi</p></doc>") "<doc><p>hi</p></doc>");
  check "a portable closure drives a channel source and destination"
    (String.equal (via_channel "<doc><p>hi</p></doc>") "[doc:[p:hi]]");
  check "the reserved namespace names are readable from one"
    (String.equal (reserved_ns ())
       "http://www.w3.org/XML/1998/namespace http://www.w3.org/2000/xmlns/");
  (* The five predefined entities were a module-level hash table before the
     patch and are a comparison chain after it. *)
  check "the five predefined entities expand"
    (String.equal
       (parse "<doc>&lt;&gt;&amp;&apos;&quot;</doc>")
       "[doc:<>&'\"]");
  check "an unknown entity reaches the callback"
    (String.equal (parse_entity "<doc>&nbsp;</doc>") "[doc:\194\160]");
  check "an entity with no callback answer is an error"
    (String.equal
       (parse_error "<doc>&nope;</doc>")
       "1:12 unknown entity reference (nope)");
  (* The UTF-8 first-byte lengths were a module-level array before the patch
     and are a match after it. One case per length the table gives. *)
  check "a two byte sequence decodes"
    (String.equal (parse "<doc>\194\169</doc>") "[doc:\194\169]");
  check "a three byte sequence decodes"
    (String.equal (parse "<doc>\230\151\165</doc>") "[doc:\230\151\165]");
  check "a four byte sequence decodes"
    (String.equal
       (parse "<doc>\240\159\142\137</doc>")
       "[doc:\240\159\142\137]");
  check "a continuation byte in the lead position is malformed"
    (String.equal
       (parse_error "<doc>\128</doc>")
       "1:6 malformed character stream");
  check "an out of range lead byte is malformed"
    (String.equal
       (parse_error "<doc>\245</doc>")
       "1:6 malformed character stream");
  Printf.printf "test_xmlm: %d checks ok\n" !checks
