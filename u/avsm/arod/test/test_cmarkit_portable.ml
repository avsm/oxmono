(* [cmarkit] is vendored under vendor/ and patched so that a portable context
   can parse markdown and render it. Dune skips aliases under a vendored
   directory, so the vendored copy's own tests never run. This one does, and it
   fails if a re-vendor drops the annotations or the kinds that go with them.

   The captures are the point. A parameter-shaped ascription guards nothing: a
   closure that only takes cmarkit values as arguments compiles whatever the
   interface says about modes. Every closure below reads a module-level value
   instead, which is what a portable function is forbidden to do unless the
   value's type crosses portability and contention. *)

let checks = ref 0

let check name b =
  incr checks;
  if not b then (
    prerr_endline ("FAIL: " ^ name);
    exit 1)

(* Two metadata keys held at module level, the shape [bushel_md.ml] holds
   them in. One is written by a parse-time resolver and read back by mappers
   that run afterwards, so the two must agree on one key value and the key
   cannot move inside either. They do not compile unless
   [Cmarkit.Meta.key] still has the [immutable_data] kind. *)
let sluglink : unit Cmarkit.Meta.key = Cmarkit.Meta.key ()
let authorlink : unit Cmarkit.Meta.key = Cmarkit.Meta.key ()

(* A renderer state key held at module level, which is what both cmarkit
   backends do. It does not compile unless
   [Cmarkit_renderer.Context.State.t] still has the [immutable_data] kind. *)
let seen : int ref Cmarkit_renderer.Context.State.t =
  Cmarkit_renderer.Context.State.make ()

(* A resolver that tags a reference label with one of the module-level keys,
   exactly as [Bushel.Md.with_bushel_links] does. This is the value the whole
   campaign was gated on. *)
let resolver : Cmarkit.Label.resolver @ portable = function
  | `Def _ as ctx -> Cmarkit.Label.default_resolver ctx
  | `Ref (_, _, (Some _ as def)) -> def
  | `Ref (_, ref, None) ->
    let key = Cmarkit.Label.key ref in
    if String.length key = 0 then None
    else
      let tag k =
        Some (Cmarkit.Label.with_meta (Cmarkit.Meta.tag k (Cmarkit.Label.meta ref)) ref)
      in
      (match key.[0] with ':' -> tag sluglink | '@' -> tag authorlink | _ -> None)

(* Parse and render, end to end, from a portable closure. [Doc.of_string] is
   given the portable resolver above, and the rendering goes through both
   backends the tree uses. Renderers are built inside the closure: a
   [Cmarkit_renderer.t] is a record of closures and does not cross, so one
   cannot be held at module level and read here. *)
let to_html : (string -> string) @ portable =
 fun md ->
  let doc = Cmarkit.Doc.of_string ~strict:false ~resolver md in
  Cmarkit_html.of_doc ~safe:true doc

let to_commonmark : (string -> string) @ portable =
 fun md ->
  let doc = Cmarkit.Doc.of_string ~strict:false ~resolver md in
  Cmarkit_commonmark.of_doc doc

(* Read the tags back, as the mappers in [bushel_md.ml] do, through a mapper
   built in the portable closure and both module-level keys. *)
let tagged_labels : (string -> string list) @ portable =
 fun md ->
  let found = ref [] in
  let inline m = function
    | Cmarkit.Inline.Link (l, _) ->
      (match Cmarkit.Inline.Link.referenced_label l with
       | Some label ->
         let meta = Cmarkit.Label.meta label in
         (match Cmarkit.Meta.find sluglink meta with
          | Some () -> found := ("slug " ^ Cmarkit.Label.key label) :: !found
          | None ->
            (match Cmarkit.Meta.find authorlink meta with
             | Some () -> found := ("author " ^ Cmarkit.Label.key label) :: !found
             | None -> ()));
         `Default
       | None -> `Default)
    | _ -> ignore m; `Default
  in
  let m = Cmarkit.Mapper.make ~inline () in
  ignore (Cmarkit.Mapper.map_doc m (Cmarkit.Doc.of_string ~strict:false ~resolver md));
  List.rev !found

(* A composed renderer that reads the module-level state key, built and run
   inside the portable closure. This is the guard on
   [Cmarkit_renderer.Context.State]. *)
let count_code_spans : (string -> int) @ portable =
 fun md ->
  let init_context c _ = Cmarkit_renderer.Context.State.set c seen (Some (ref 0)) in
  let inline c = function
    | Cmarkit.Inline.Code_span _ ->
      incr (Cmarkit_renderer.Context.State.get c seen);
      false
    | _ -> false
  in
  let custom = Cmarkit_renderer.make ~init_context ~inline () in
  let r = Cmarkit_renderer.compose (Cmarkit_html.renderer ~safe:true ()) custom in
  let b = Buffer.create 256 in
  let doc = Cmarkit.Doc.of_string ~strict:false md in
  let c = Cmarkit_renderer.Context.make r b in
  Cmarkit_renderer.Context.doc c doc;
  !(Cmarkit_renderer.Context.State.get c seen)

(* The CommonMark renderer's backslash escaping. Upstream holds the five
   character sets it escapes by in a [Set.Make (Char)]. This copy holds five
   predicates, because a module-level container constant does not cross. The
   document below is what reaches four of them, and it is here because nothing
   else in the tree does: [test_md_golden.ml]'s three renderers are all HTML,
   and the only in-tree caller of [Cmarkit_commonmark] is
   [Bushel.Md.to_markdown], which no test runs.

   [<x\<y>] is an angled destination, for [esc_angles]. [p\(q\)] is a bare one,
   for [esc_parens]. The two titles are double and single quoted, for
   [esc_dquote] and [esc_quote]. [esc_link_label], the fifth, is reached by the
   reference links in [doc] below.

   The document round-trips, so the check is that rendering returns it
   unchanged. That bites in both directions: a predicate that answers [true]
   everywhere adds backslashes and one that answers [false] everywhere drops
   them, and each of the four is caught on its own. *)
let escaping_md = "[a](<x\\<y> \"q\\\"r\")\n\n[b](p\\(q\\) 'i\\'t')\n"

let render_escapes : (string -> string) @ portable =
 fun md -> Cmarkit_commonmark.of_doc (Cmarkit.Doc.of_string md)

(* A fold over the document, also from a portable closure. The results are
   written as [`Default] and [`Fold] rather than through [Folder.default] and
   [Folder.ret]: those are module-level values of a polymorphic variant type,
   which crosses nothing, so a portable function cannot read them. The same
   goes for [Mapper.default] and [Mapper.delete] above.

   This one is deliberately parameter-shaped: it captures no module-level
   value, so what it guards is only that [Folder.make] and [Folder.fold_block]
   are themselves portable values, which is the floating [@@ portable] on
   [cmarkit.mli]. The five closures above are capture-shaped and guard the
   kinds as well. *)
let count_paragraphs : (string -> int) @ portable =
 fun md ->
  let block _ acc b =
    match b with
    | Cmarkit.Block.Paragraph _ -> `Fold (acc + 1)
    | _ -> `Default
  in
  let f = Cmarkit.Folder.make ~block () in
  Cmarkit.Folder.fold_block f 0 (Cmarkit.Doc.block (Cmarkit.Doc.of_string md))

let doc = "# Title\n\nSome *emph* and `code` and [a slug][:hello] and [me][@ada].\n"

let () =
  (* The two undefined-label comments are the resolver's work showing through:
     it answers a label for [:hello] and [@ada] that no definition backs, and
     the HTML renderer says so by name. A resolver that never ran would leave
     the two references as literal text instead. *)
  check "a portable closure parses and renders HTML"
    (String.equal (to_html doc)
       "<h1>Title</h1>\n\
        <p>Some <em>emph</em> and <code>code</code> and a slug\
        <!-- Undefined label :hello --> and me\
        <!-- Undefined label @ada -->.</p>\n");
  check "a portable closure renders CommonMark"
    (String.equal (to_commonmark doc)
       "# Title\n\nSome *emph* and `code` and [a slug][:hello] and [me][@ada].\n");
  check "the tags a portable resolver wrote are read back through the keys"
    (List.equal String.equal (tagged_labels doc) [ "slug :hello"; "author @ada" ]);
  check "a portable closure renders through a composed renderer and its state"
    (count_code_spans doc = 1);
  check "a portable closure folds the document" (count_paragraphs doc = 1);
  check "the CommonMark escaper's four link character sets answer"
    (String.equal (render_escapes escaping_md) escaping_md);
  check "an empty document renders empty" (String.equal (to_html "") "");
  Printf.printf "test_cmarkit_portable: %d checks ok\n" !checks
