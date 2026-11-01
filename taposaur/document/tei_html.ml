open Xml

type node = Text of string | Element of element
and element = {
  serial : int; ns : string; name : string;
  attrs : ((string * string) * string) list; children : node list;
}
let tei_ns = "http://www.tei-c.org/ns/1.0"
let math_ns = "http://www.w3.org/1998/Math/MathML"
let attr e name = List.assoc_opt ("",name) e.attrs
let xml_id e = List.assoc_opt
  ("http://www.w3.org/XML/1998/namespace","id") e.attrs
let is name = function
  | Element e -> (e.ns = "" || e.ns = tei_ns) && e.name = name
  | Text _ -> false
let rec plain = function
  | Text s -> s
  | Element e -> String.concat "" (List.map plain e.children)
let content e = normalize (String.concat "" (List.map plain e.children))
let elements nodes = List.filter_map (function Element e -> Some e | _ -> None) nodes
let rec descendants name nodes = List.concat_map (function
  | Text _ -> []
  | Element e as n -> if is name n then [e] else descendants name e.children) nodes
let first name nodes = List.find_opt (is name) nodes
let text_of name nodes = match descendants name nodes with
  | [] -> None | e::_ -> let s = content e in if s = "" then None else Some s
let escape s = Htmlit.El.to_string ~doctype:false (Htmlit.El.txt s)
let quote s = "\"" ^ escape s ^ "\""
let anchor s =
  let b = Buffer.create (min 4096 (String.length s * 2 + 4)) in
  Buffer.add_string b "tei-";
  String.iter (fun c -> Buffer.add_string b (Printf.sprintf "%02x" (Char.code c))) s;
  Buffer.contents b
let words s = String.split_on_char ' ' (normalize s) |> List.filter ((<>) "")
let safe_url s =
  let lower = String.lowercase_ascii s in
  if String.exists (fun c -> Char.code c <= 32 || c = '\\' || c = '"' ||
    c = '<' || c = '>') s then None
  else if String.starts_with ~prefix:"https://" lower ||
          String.starts_with ~prefix:"http://" lower then Some s
  else None
let doi_url s = "https://doi.org/" ^ String.concat ""
  (List.init (String.length s) (fun i -> match s.[i] with
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '/' | '-' | '_' | '.' | '~' ->
        String.make 1 s.[i]
    | c -> Printf.sprintf "%%%02X" (Char.code c)))
let doi s = match Taposaur.Doi.of_string (String.trim s) with
  | Ok d -> Some (Taposaur.Doi.to_string d) | Error _ -> None
let positive s = match int_of_string_opt s with
  | Some n when n > 0 && n <= 1000 -> Some (string_of_int n) | _ -> None

let parse input =
  let serial = ref 0 in
  let rec node depth =
    if depth > 256 then raise Limit;
    incr serial;
    if !serial > 500000 then raise Limit;
    match Xmlm.input input with
    | `Data s -> Text s
    | `El_start ((ns,name),attrs) ->
        let number = !serial in
        let rec children acc = match Xmlm.peek input with
          | `El_end -> ignore (Xmlm.input input); List.rev acc
          | _ -> children (node (depth+1)::acc) in
        Element {serial=number;ns;name;attrs;children=children []}
    | _ -> failwith "Unexpected XML structure" in
  (match Xmlm.peek input with `Dtd _ -> ignore (Xmlm.input input) | _ -> ());
  let root = node 0 in
  if not (is "TEI" root) then failwith "Expected a TEI document";
  if not (Xmlm.eoi input) then failwith "Trailing XML document";
  root

let render ?(max_bytes=16 * 1024 * 1024)
    ?(max_output=32 * 1024 * 1024)
    ?(resolve_doi : string -> Taposaur.Bibliography.t option = fun _ -> None)
    ?(journal_url=fun _ -> None) ?(source_url=fun _ -> None) flow =
  if max_output <= 0 then invalid_arg "max_output must be positive";
  let input = Xml.input ~max_bytes flow in
  let out = Buffer.create 4096 in
  let add s = if String.length s > max_output - Buffer.length out then raise Limit;
    Buffer.add_string out s in
  let txt s = add (escape s) in
  let start tag attrs = add ("<" ^ tag);
    List.iter (fun (k,v) -> add (" " ^ k ^ "=" ^ quote v)) attrs; add ">" in
  let stop tag = add ("</" ^ tag ^ ">") in
  let wrap tag attrs f = start tag attrs; f (); stop tag in
  try
    let root = parse input in
    let root_children = match root with Element e -> e.children | _ -> [] in
    let texts = List.filter (is "text") root_children in
    let header = match first "teiHeader" root_children with
      | Some (Element e) -> e.children | _ -> [] in
    let abstracts = if descendants "abstract" texts = [] then
      List.map (fun e -> Element e) (descendants "abstract" header) else [] in
    let title = match descendants "titleStmt" header with
      | e::_ -> text_of "title" e.children | [] -> None in
    let visible = abstracts @ texts in
    let ids = Hashtbl.create 128 and targets = Hashtbl.create 128 in
    let heading_numbers = Hashtbl.create 32 in
    let seen = Hashtbl.create 128 in
    let rec index = function
      | Text _ -> ()
      | Element e ->
          let id = match xml_id e with
            | Some source when not (Hashtbl.mem seen source) ->
                Hashtbl.add seen source (); let id = anchor source in
                Hashtbl.add targets source id; id
            | _ -> "tei-node-" ^ string_of_int e.serial in
          Hashtbl.add ids e.serial id;
          if List.mem e.name ["div";"div1";"div2";"div3";"div4"] then
            Option.iter (fun number -> List.iter (function
              | Element h when h.name = "head" ->
                  Hashtbl.replace heading_numbers h.serial number
              | _ -> ()) e.children) (attr e "n");
          List.iter index e.children in
    List.iter index visible;
    let id e = Hashtbl.find ids e.serial in
    let links e = match attr e "target" with None -> [] | Some s ->
      List.filter_map (fun s ->
        if String.starts_with ~prefix:"#" s then
          Option.map (fun id -> "#" ^ id)
            (Hashtbl.find_opt targets (String.sub s 1 (String.length s-1)))
        else safe_url s) (words s) in
    let backs = Hashtbl.create 128 in
    let rec citations = function
      | Text _ -> ()
      | Element e ->
          if e.name = "ref" || e.name = "ptr" then
            List.iter (fun target -> if String.starts_with ~prefix:"#" target then
              Hashtbl.replace backs target
                (id e :: Option.value ~default:[] (Hashtbl.find_opt backs target)))
              (links e);
          List.iter citations e.children in
    List.iter citations visible;
    let external_attrs href = if String.starts_with ~prefix:"#" href then [] else
      ["target","_blank";"rel","noopener noreferrer"] in
    let link href label = wrap "a" (external_attrs href @ ["href",href])
      (fun () -> txt label) in
    let backlinks e = match Hashtbl.find_opt backs ("#" ^ id e) with
      | None -> ()
      | Some sources -> wrap "span" ["class","tei-backlinks"] (fun () ->
          List.rev sources |> List.iteri (fun i source ->
            txt " "; wrap "a" ["href","#" ^ source;
              "aria-label","Return to citation " ^ string_of_int (i+1)]
              (fun () -> txt ("↩" ^ string_of_int (i+1))))) in
    let bibl_doi e = descendants "idno" e.children |> List.find_map (fun n ->
      match Option.map String.lowercase_ascii (attr n "type") with
      | Some "doi" -> doi (content n) | _ -> None) in
    let rec render depth parent = function
      | Text s -> txt s
      | Element e ->
        let children () = List.iter (render depth e.name) e.children in
        let base = ["id",id e] in
        let tagged tag attrs f = wrap tag (base @ attrs) f in
        let normal tag = tagged tag [] children in
        if e.ns = math_ns then math e
        else if e.ns <> "" && e.ns <> tei_ns then children ()
        else match e.name with
        | "text" | "front" | "body" | "back" | "group" -> normal "div"
        | "div" | "div1" | "div2" | "div3" | "div4" ->
            tagged "section" [] (fun () -> List.iter
              (render (min 5 (depth+1)) e.name) e.children)
        | "head" ->
            let tag = if parent = "figure" then "figcaption"
              else if parent = "figure-with-description" then "div"
              else if parent = "table" then "caption"
              else "h" ^ string_of_int (max 2 (min 6 (depth+1))) in
            tagged tag [] (fun () ->
              Option.iter (fun n -> txt (n ^ " "))
                (match attr e "n" with Some _ as n -> n
                 | None -> Hashtbl.find_opt heading_numbers e.serial);
              children ())
        | "abstract" -> tagged "section" ["class","tei-abstract"] (fun () ->
            if first "head" e.children = None then
              wrap "h2" [] (fun () -> txt "Abstract"); children ())
        | "p" | "ab" -> normal (if parent = "inline-note" then "span" else "p")
        | "s" | "seg" | "name" | "rs" | "term" | "foreign" -> normal "span"
        | "list" ->
            let glossary = attr e "type" = Some "gloss" in
            let ordered = List.exists (fun value ->
              List.mem value ["ordered";"numbered";"number"])
              (List.filter_map (attr e) ["type";"rend"]) in
            List.filter (is "head") e.children |> List.iter (render depth "div");
            tagged (if glossary then "dl" else if ordered then "ol" else "ul") []
              (fun () -> List.filter (fun n -> not (is "head" n)) e.children
                |> List.iter (render depth (if glossary then "gloss" else "list")))
        | "item" -> normal (if parent = "gloss" then "dd" else "li")
        | "label" when parent = "gloss" -> normal "dt"
        | "label" -> tagged "span" ["class","tei-label"] children
        | "quote" | "cit" | "epigraph" -> normal "blockquote"
        | "q" -> normal "q"
        | "hi" ->
            let rend = Option.value ~default:"italic" (attr e "rend")
              |> String.lowercase_ascii |> words in
            let tags = List.filter_map (function
              | "bold" -> Some "strong" | "italic" | "italics" -> Some "em"
              | "sup" | "superscript" -> Some "sup"
              | "sub" | "subscript" -> Some "sub"
              | "underline" -> Some "u" | "strikethrough" -> Some "s"
              | "monospace" | "typewriter" -> Some "code" | _ -> None) rend in
            tagged "span" [] (fun () ->
              List.iter (fun tag -> start tag []) tags; children ();
              List.iter stop (List.rev tags))
        | "emph" -> normal "em"
        | "sup" -> normal "sup" | "sub" -> normal "sub"
        | "del" -> normal "del" | "add" -> normal "ins"
        | "code" | "eg" -> normal "code"
        | "lb" -> start "br" base
        | "pb" -> tagged "span" ["class","tei-page"] (fun () ->
            txt ("[Page " ^ Option.value ~default:"break" (attr e "n") ^ "]"))
        | "ref" | "ptr" ->
            let destinations = links e in
            let label () = if content e = "" then
              txt (Option.value ~default:"Reference" (attr e "target")) else children () in
            (match destinations with
             | [] -> tagged "span" ["class","tei-unresolved"] label
             | href::rest -> tagged "a" (external_attrs href @ ["href",href]) label;
                 List.iteri (fun i href -> txt " ";
                   link href ("[" ^ string_of_int (i+2) ^ "]")) rest)
        | "note" when attr e "type" = Some "raw_reference" -> normal "span"
        | "note" ->
            let inline = List.mem parent ["p";"s";"seg";"hi"] in
            tagged (if inline then "span" else "aside")
              ["class","tei-note";"role","note"] (fun () ->
            Option.iter (fun n -> txt (n ^ ". ")) (attr e "n");
            List.iter (render depth (if inline then "inline-note" else "note"))
              e.children; backlinks e)
        | "listBibl" -> tagged "section" ["class","tei-bibliography"] (fun () ->
            if first "head" e.children = None then
              wrap "h2" [] (fun () -> txt "References"); children ())
        | "biblStruct" | "bibl" -> bibliography depth e
        | "table" ->
            tagged "table" [] (fun () ->
              List.filter (is "head") e.children |> List.iter (render depth "table");
              wrap "tbody" [] (fun () -> List.filter
                (fun n -> not (is "head" n)) e.children
                |> List.iter (render depth "table")))
        | "row" -> tagged "tr" [] (fun () -> List.iter
            (render depth (if attr e "role" = Some "label" then "header-row" else "row"))
            e.children)
        | "cell" ->
            let header = parent = "header-row" || attr e "role" = Some "label" in
            let spans = List.filter_map (fun (source,dest) ->
              Option.bind (attr e source) positive |> Option.map (fun n -> dest,n))
              ["rows","rowspan";"cols","colspan"] in
            tagged (if header then "th" else "td") spans children
        | "figure" -> tagged "figure" [] (fun () ->
            List.iter (render depth (if first "figDesc" e.children = None then
              "figure" else "figure-with-description")) e.children;
            if descendants "graphic" e.children = [] &&
               descendants "table" e.children = [] then
              wrap "p" ["class","tei-unavailable"]
                (fun () -> txt "Figure image is not present in this TEI.");
            backlinks e)
        | "figDesc" -> normal "figcaption"
        | "graphic" ->
            tagged "span" ["class","tei-graphic"] (fun () ->
              match Option.bind (attr e "url") safe_url with
              | Some url ->
                  (* User activation prevents source documents from making
                     unsolicited remote image requests. *)
                  wrap "a" ["href",url;"class","tei-image-link"]
                    (fun () -> txt "Open figure image")
              | None -> txt ("Image unavailable: " ^
                  Option.value ~default:"no image resource" (attr e "url")))
        | "formula" ->
            let inline = parent = "p" || parent = "s" ||
              attr e "rend" = Some "inline" in
            let tex = List.exists (fun s -> List.mem s ["tex";"latex"])
              (List.filter_map (fun key -> Option.map String.lowercase_ascii
                (attr e key)) ["notation";"type"]) in
            tagged "span" ["class",if inline then "tei-formula" else "tei-equation"]
              (fun () ->
                if tex then wrap "span" ["data-tex","";
                  "data-display",string_of_bool (not inline)]
                  (fun () -> txt (content e)) else children ();
                Option.iter (fun n -> wrap "span" ["class","tei-equation-number"]
                  (fun () -> txt ("(" ^ n ^ ")"))) (attr e "n"); backlinks e)
        | "choice" ->
            let selected = List.find_map (fun name -> first name e.children)
              ["corr";"reg";"expan";"orig";"sic";"abbr"] in
            tagged "span" [] (fun () -> match selected with
              | Some n -> render depth parent n | None -> children ())
        | "gap" -> tagged "span" ["class","tei-unavailable"] (fun () ->
            txt ("[" ^ Option.value ~default:"Missing text" (attr e "reason") ^ "]"))
        | "date" when content e = "" -> tagged "span" [] (fun () ->
            Option.iter txt (attr e "when"))
        | "biblScope" when content e = "" -> tagged "span" [] (fun () ->
            Option.iter txt (attr e "from");
            Option.iter (fun n -> txt ("–" ^ n)) (attr e "to"))
        | "title" -> normal "cite"
        | "idno" -> tagged "span" ["class","tei-identifier"] (fun () ->
            Option.iter (fun s -> txt (s ^ ": ")) (attr e "type"); children ())
        | "persName" -> tagged "span" [] (fun () ->
            List.filter (function Text s -> String.trim s <> "" | _ -> true)
              e.children |> List.iteri (fun i n ->
              if i > 0 then txt " "; render depth e.name n))
        | "lg" | "l" | "address" | "addrLine" | "byline" | "docAuthor"
        | "docDate" | "dateline" | "opener" | "closer" -> normal "div"
        | "script" | "style" -> ()
        | _ -> normal "span"
    and math e =
      let allowed = ["math";"mrow";"mi";"mn";"mo";"mtext";"ms";
        "mspace";"mfrac";"msqrt";"mroot";"msub";"msup";"msubsup";
        "munder";"mover";"munderover";"mtable";"mtr";"mtd";
        "mmultiscripts";"mprescripts";"none";"semantics";"mstyle";
        "mpadded";"mphantom";"menclose"] in
      if e.name = "mfenced" then
        wrap "mrow" ["id",id e] (fun () ->
          wrap "mo" [] (fun () -> txt (Option.value ~default:"(" (attr e "open")));
          let children = List.filter (function Text s -> String.trim s <> "" | _ -> true)
            e.children in
          List.iteri (fun i n ->
            if i > 0 then wrap "mo" [] (fun () -> txt
              (Option.value ~default:"," (attr e "separators")));
            match n with Element child when child.ns = math_ns -> math child
              | Text s -> txt s | _ -> ()) children;
          wrap "mo" [] (fun () -> txt (Option.value ~default:")" (attr e "close"))))
      else if List.mem e.name ["annotation";"annotation-xml"] then ()
      else if List.mem e.name allowed then begin
        let attrs = List.filter_map (fun ((ns,k),v) ->
          if ns = "" && List.mem k ["display";"mathvariant";"stretchy";
            "fence";"separator";"accent";"accentunder";"columnalign";
            "rowalign";"columnspan";"rowspan";"linethickness";
            "scriptlevel";"displaystyle";"notation"] && String.length v < 80
          then Some (k,v) else None) e.attrs in
        let attrs = if e.name = "math" then ("xmlns",math_ns)::attrs else attrs in
        wrap e.name (("id",id e)::attrs) (fun () ->
          List.iter (function Text s -> txt s
            | Element child when child.ns = math_ns -> math child
            | _ -> ()) e.children)
      end else txt (content e)
    and bibliography depth e =
      let raw = List.find_opt (fun n -> attr n "type" = Some "raw_reference")
        (descendants "note" e.children) in
      let structured = descendants "analytic" e.children <> [] ||
        descendants "monogr" e.children <> [] in
      let identifier = bibl_doi e in
      let identifier,resolved = match identifier with
        | Some d -> Some d,resolve_doi d
        | None ->
            let source = Option.fold ~none:"" ~some:content raw in
            let rec locate i =
              if i+3 >= String.length source then None,None
              else if String.sub source i 3 <> "10." then locate (i+1)
              else
                let finish = ref i in
                while !finish < String.length source && !finish-i < 160 &&
                  not (List.mem source.[!finish] [' ';'\n';'\t';'"']) do
                  incr finish
                done;
                let rec find n = if n < 8 then None,None else
                  match doi (String.sub source i n) with
                  | Some d -> (match resolve_doi d with
                    | Some b -> Some d,Some b | None -> find (n-1))
                  | None -> find (n-1) in
                find (!finish-i) in
            locate 0 in
      let source = Option.bind identifier source_url |> fun url ->
        Option.bind url safe_url in
      let publication name = match source with
        | Some url -> link url name | None -> journal name in
      wrap "div" ["id",id e;"class","tei-reference"] (fun () ->
        Option.iter (fun n -> wrap "span" ["class","tei-reference-label"]
          (fun () -> txt n)) (attr e "n");
        (match resolved with
         | Some b when b.Taposaur.Bibliography.title <> None && not structured ->
             wrap "div" ["class","tei-reference-authors"] (fun () ->
               List.iteri (fun i (a : Taposaur.Author.t) ->
                 if i > 0 then txt ", ";
                 Option.iter (fun g -> txt (g ^ " ")) a.given; txt a.family) b.authors);
             wrap "div" ["class","tei-reference-title"] (fun () ->
               Option.iter (fun title -> match identifier with
                 | Some d -> link (doi_url d) title | None -> txt title) b.title);
             wrap "div" ["class","tei-reference-publication"] (fun () ->
               Option.iter publication b.journal;
               List.iter (Option.iter (fun s -> txt (" · " ^ s)))
                 [(match b.published with Some _ as date -> date
                   | None -> Option.map string_of_int b.year);
                  b.volume; b.issue; b.pages]);
             wrap "small" ["class","tei-reference-source"]
               (fun () -> txt "Cached catalogue metadata");
             Option.iter (fun raw -> wrap "details" [] (fun () ->
               wrap "summary" [] (fun () -> txt "Original reference");
               txt (content raw))) raw
         | _ when structured ->
             let groups = List.filter (fun n -> is "analytic" n ||
               is "monogr" n || is "series" n) e.children in
             List.iter (function Text _ -> () | Element group ->
               let authors = List.filter (fun n -> is "author" n || is "editor" n)
                 group.children in
               if authors <> [] then wrap "div" ["class","tei-reference-authors"]
                 (fun () -> List.iteri (fun i n -> if i > 0 then txt ", ";
                   render depth group.name n) authors);
               List.iter (function
                 | Element t when t.name = "title" ->
                     wrap "div" ["class",if attr t "level" = Some "j" then
                       "tei-reference-publication" else "tei-reference-title"]
                       (fun () -> if attr t "level" = Some "j" then publication (content t)
                        else match identifier with Some d -> link (doi_url d) (content t)
                        | None -> txt (content t))
                 | Element t when t.name = "imprint" ->
                     wrap "div" ["class","tei-reference-publication"] (fun () ->
                       elements t.children |> List.iteri (fun i n ->
                         if i > 0 then txt " · ";
                         Option.iter (fun unit -> txt (unit ^ " ")) (attr n "unit");
                         render depth t.name (Element n)))
                 | _ -> ()) group.children) groups;
             List.filter (fun n -> not (is "analytic" n || is "monogr" n ||
               is "series" n || is "idno" n ||
               (match n with Element e -> attr e "type" = Some "raw_reference"
                | _ -> false))) e.children
             |> List.iter (render depth e.name);
             Option.iter (fun n -> wrap "details" [] (fun () ->
               wrap "summary" [] (fun () -> txt "Original reference");
               txt (content n))) raw
         | _ -> (match raw with Some n ->
             wrap "div" ["class","tei-reference-raw"] (fun () ->
               let s = content n in
               (* Restore visible word boundaries without changing the
                  stored evidence or guessing bibliographic fields. *)
               String.iteri (fun i c ->
                 if i > 0 && s.[i-1] >= '0' && s.[i-1] <= '9' &&
                   ((c >= 'A' && c <= 'Z') || Char.code c >= 192) then txt " ";
                 txt (String.make 1 c)) s);
             wrap "details" ["class","tei-original-reference"] (fun () ->
               wrap "summary" [] (fun () -> txt "Original reference"); txt (content n))
             | None -> List.filter (fun n -> not (is "idno" n)) e.children
                 |> List.iter (render depth e.name)));
        (match identifier with
         | Some d when bibl_doi e = None -> txt " "; link (doi_url d) ("DOI: " ^ d)
         | _ -> ());
        Option.iter (fun url -> txt " · "; link url "Publication source in OpenAlex") source;
        List.iter (fun n -> txt " ";
          match Option.map String.lowercase_ascii (attr n "type"),doi (content n) with
          | Some "doi",Some d -> link (doi_url d) ("DOI: " ^ d)
          | _ -> render depth e.name (Element n)) (descendants "idno" e.children);
        backlinks e)
    and journal name = match Option.bind (journal_url name) safe_url with
      | Some url -> link url name | None -> txt name in
    start "article" ["class","tei"];
    Option.iter (fun s -> wrap "h1" [] (fun () -> txt s)) title;
    let rec headings depth nodes = List.concat_map (function
      | Text _ -> []
      | Element e when List.mem e.name ["div";"div1";"div2";"div3";"div4"] ->
          let heading = List.find_map (function
            | Element h when h.name = "head" -> Some (depth,h) | _ -> None) e.children in
          Option.to_list heading @ headings (depth+1) e.children
      | Element e when List.mem e.name ["text";"front";"body";"back"] ->
          headings depth e.children
      | _ -> []) nodes in
    let toc = headings 0 texts in
    if List.length toc > 1 then wrap "details" ["class","tei-contents"] (fun () ->
      wrap "summary" [] (fun () -> txt "Contents");
      wrap "ul" [] (fun () -> List.iter (fun (depth,h) ->
        wrap "li" [] (fun () -> txt (String.make (min 8 depth) ' ');
          link ("#" ^ id h) (content h))) toc));
    List.iter (render 0 "") visible;
    stop "article";
    Ok (Buffer.contents out)
  with
  | Limit -> Error "TEI exceeds configured input, output or nesting limit"
  | Failure e -> Error e
  | End_of_file -> Error "Truncated TEI document"
  | Xmlm.Error ((line,col),e) ->
      Error (Printf.sprintf "%d:%d: %s" line col (Xmlm.error_message e))
