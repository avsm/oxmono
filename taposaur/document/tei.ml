open Taposaur
open Xml

type collect = {
  title : Buffer.t;
  journal : Buffer.t;
  publisher : Buffer.t;
  abstract : Buffer.t;
  forename : Buffer.t;
  surname : Buffer.t;
  mutable forenames : string list;  (* current author, reversed *)
  mutable authors : Author.t list;  (* reversed *)
  mutable imprint_date : string option;
  mutable stmt_date : string option;
  mutable abstract_done : bool;
  mutable title_done : bool;
}

let attr name atts = List.assoc_opt ("", name) atts
let has name v atts = attr name atts = Some v

exception Header_done

(* Element context as a stack of (local-name, attributes), innermost
   first. Namespaces are ignored, the header dialect is unambiguous
   on local names.  Character data is routed to a field buffer by
   the pattern of enclosing elements, which keeps a persName in a
   back-matter reference or a title in the body from polluting the
   header fields.  Parsing runs past the teiHeader to the start of
   the body, because Pub2TEI puts the abstract in text/front where
   GROBID puts it in the header.  Nothing in a reference list is
   reachable before the body. *)
let parse_input i =
  let c = {
    title = Buffer.create 128;
    journal = Buffer.create 64;
    publisher = Buffer.create 64;
    abstract = Buffer.create 1024;
    forename = Buffer.create 32;
    surname = Buffer.create 32;
    forenames = []; authors = [];
    imprint_date = None; stmt_date = None;
    abstract_done = false;
    title_done = false;
  } in
  let stack = ref [] in
  let extra = Hashtbl.create 8 in
  let extra_done = Hashtbl.create 8 in
  let extra_field stack =
    List.find_map (function
      | "biblScope", a ->
          (match attr "unit" a with
           | Some ("volume" | "issue" | "page") as key -> key
           | _ -> None)
      | "idno", a ->
          (match Option.map String.lowercase_ascii (attr "type" a) with
           | Some ("doi" | "pmid" | "pmcid" | "issn") as key -> key
           | _ -> None)
      | _ -> None) stack in
  let extra_buffer key = match Hashtbl.find_opt extra key with
    | Some b -> b
    | None -> let b = Buffer.create 32 in Hashtbl.add extra key b; b in
  (* The document title is the titleStmt one; the analytic
     duplicate and the monogr journal title are other elements. *)
  let rec in_main_title = function
    | ("title", a) :: ("titleStmt", _) :: _ ->
        has "type" "main" a || attr "type" a = None
    | _ :: rest -> in_main_title rest
    | [] -> false
  in
  let target () =
    match !stack with
    (* A title's own markup is still its title. Matching only when
       the title element is innermost dropped the text of every
       <hi> inside it, and the emptiness guard that was meant to
       take the first title and ignore the analytic duplicate then
       dropped everything after it too: a species name in italics
       truncated the title at the word before it. The element is
       searched for up the stack instead, and it is closing rather
       than filling that says the title is finished. *)
    | stack when (not c.title_done) && in_main_title stack -> Some c.title
    | ("title", a) :: ("monogr", _) :: _
      when has "level" "j" a &&
        (has "type" "main" a || attr "type" a = None) -> Some c.journal
    | ("publisher", _) :: ("publicationStmt", _) :: _ -> Some c.publisher
    | ("forename", _) :: ("persName", _) :: ("author", _)
      :: ("analytic", _) :: _ -> Some c.forename
    | ("surname", _) :: ("persName", _) :: ("author", _)
      :: ("analytic", _) :: _ -> Some c.surname
    | stack when (not c.abstract_done)
                 && List.exists (fun (n, _) -> n = "abstract") stack ->
      Some c.abstract
    | _ -> None
  in
  let flush_author () =
    (match non_empty c.surname with
     | None -> ()
     | Some family ->
       let given =
         match List.rev c.forenames with
         | [] -> None
         | fs -> Some (String.concat " " fs)
       in
       c.authors <- { Author.given; family } :: c.authors);
    Buffer.clear c.surname;
    Buffer.clear c.forename;
    c.forenames <- []
  in
  let run () =
    while true do
      match Xmlm.input i with
      | `Dtd _ -> ()
      | `Data d ->
        Option.iter (fun key ->
          if not (Hashtbl.mem extra_done key) then
            Buffer.add_string (extra_buffer key) d) (extra_field !stack);
        (match target () with
         | Some b -> Buffer.add_string b d
         | None -> ())
      | `El_start ((ns, name), atts) ->
        if !stack = [] && (name <> "TEI" ||
          (ns <> "" && ns <> "http://www.tei-c.org/ns/1.0")) then
          failwith "Expected a TEI document";
        let atts =
          List.filter_map
            (fun ((ns, n), v) -> if ns = "" then Some ((ns, n), v) else None)
            atts
        in
        (* Dates carry their value in @when and may be empty
           elements, so read them at the start tag: the publication
           date from the imprint, with the fileDesc publicationStmt
           as fallback (GROBID puts it there for PDF extractions). *)
        if name = "body" then raise Header_done;
        (if name = "date" then
           match !stack with
           | ("imprint", _) :: _
             when has "type" "published" atts && c.imprint_date = None ->
             c.imprint_date <- attr "when" atts
           | ("publicationStmt", _) :: _ when c.stmt_date = None ->
             c.stmt_date <- attr "when" atts
           | _ -> ());
        if name = "biblScope" && has "unit" "page" atts &&
          not (Hashtbl.mem extra_done "page") then
          (match attr "from" atts, attr "to" atts with
           | Some a, Some b -> Buffer.add_string (extra_buffer "page")
               (if a = b then a else a ^ "–" ^ b)
           | Some a, None -> Buffer.add_string (extra_buffer "page") a
           | _ -> ());
        if name = "biblScope" && has "unit" "page" atts &&
          attr "from" atts <> None then Hashtbl.replace extra_done "page" ();
        if List.length !stack >= 256 then raise Limit;
        stack := (name, atts) :: !stack
      | `El_end ->
        match !stack with
        | [] -> raise Header_done
        | (name, attrs) :: rest ->
          if name = "idno" || name = "biblScope" then
            Option.iter (fun key ->
              if non_empty (extra_buffer key) <> None then
                Hashtbl.replace extra_done key ()) (extra_field !stack);
          (* Sentence and paragraph breaks in the abstract carry no
             surrounding whitespace in xml:space="preserve" output;
             reinsert one so sentences do not run together. *)
          if List.exists (fun (n, _) -> n = "abstract") !stack then
            Buffer.add_char c.abstract ' ';
          (match name, rest with
           | "forename", _ ->
             (match non_empty c.forename with
              | Some f -> c.forenames <- f :: c.forenames
              | None -> ());
             Buffer.clear c.forename
           | "author", ("analytic", _) :: _ -> flush_author ()
           | "title", ("titleStmt", _) :: _
             when has "type" "main" attrs || attr "type" attrs = None ->
               c.title_done <- true
           | "abstract", _ ->
             if Buffer.length c.abstract > 0 then c.abstract_done <- true
           | _ -> ());
          stack := rest;
          if name = "front" || rest = [] then raise Header_done
    done
  in
  match run () with
  | () | exception Header_done ->
    let field key = Option.bind (Hashtbl.find_opt extra key) non_empty in
    Ok {
      Bibliography.year = None;
      title = non_empty c.title;
      authors = List.rev c.authors;
      journal = non_empty c.journal;
      publisher = non_empty c.publisher;
      published =
        (match c.imprint_date with Some _ as d -> d | None -> c.stmt_date);
      abstract = non_empty c.abstract;
      volume = field "volume"; issue = field "issue";
      pages = field "page"; issn = field "issn";
      doi = field "doi"; pmcid = field "pmcid"; pmid = field "pmid";
    }
  | exception Xmlm.Error ((line, col), e) ->
    Error (Fmt.str "%d:%d: %s" line col (Xmlm.error_message e))
  | exception Limit -> Error "TEI header exceeds byte or nesting limit"
  | exception End_of_file -> Error "Truncated TEI header"
  | exception Failure message -> Error message
let metadata_string src =
  parse_input (Xmlm.make_input ~strip:false (`String (0, src)))

let metadata ?(max_bytes = 1024 * 1024) flow =
  let input = Xml.input ~max_bytes flow in
  try parse_input input with Limit -> Error "TEI header exceeds byte limit"

let render_html ?(max_bytes = 16 * 1024 * 1024)
    ?(max_output = 32 * 1024 * 1024) flow =
  if max_output <= 0 then invalid_arg "max_output must be positive";
  let input = Xml.input ~max_bytes flow in
  let output = Buffer.create 4096 in
  let add s =
    if String.length s > max_output - Buffer.length output then raise Limit;
    Buffer.add_string output s
  in
  let stack = ref [] and in_text = ref false and saw_tei = ref false in
  let tag = function
    | "p" | "ab" -> Some "p"
    | "head" -> Some "h2"
    | "div" -> Some "section"
    | "list" -> Some "ul"
    | "item" -> Some "li"
    | "quote" -> Some "blockquote"
    | "hi" -> Some "em"
    | "table" -> Some "table"
    | "row" -> Some "tr"
    | "cell" -> Some "td"
    | "figure" -> Some "figure"
    | "figDesc" -> Some "figcaption"
    | "formula" -> Some "code"
    | _ -> None
  in
  try
    add "<article class=\"tei\">";
    let finished = ref false in
    while not !finished do
      match Xmlm.input input with
      | `Dtd _ -> ()
      | `El_start ((ns, name), _) ->
          if !stack = [] then begin
            if name <> "TEI" ||
              (ns <> "" && ns <> "http://www.tei-c.org/ns/1.0")
            then failwith "Expected a TEI document";
            saw_tei := true
          end;
          if List.length !stack >= 256 then raise Limit;
          if name = "text" then in_text := true;
          let emitted = if !in_text then tag name else None in
          Option.iter (fun tag -> add ("<" ^ tag ^ ">")) emitted;
          if !in_text && name = "lb" then add "<br>";
          stack := (name, emitted) :: !stack
      | `Data data when !in_text ->
          add (Htmlit.El.to_string ~doctype:false (Htmlit.El.txt data))
      | `Data _ -> ()
      | `El_end ->
          (match !stack with
           | [] -> failwith "Unexpected closing element"
           | (name, emitted) :: rest ->
               Option.iter (fun tag -> add ("</" ^ tag ^ ">")) emitted;
               stack := rest;
               if name = "text" then in_text := false;
               if rest = [] then finished := true)
    done;
    if not !saw_tei then failwith "Missing TEI document";
    if not (Xmlm.eoi input) then failwith "Trailing XML document";
    add "</article>";
    Ok (Buffer.contents output)
  with
  | Limit -> Error "TEI exceeds configured input, output or nesting limit"
  | Failure e -> Error e
  | End_of_file -> Error "Truncated TEI document"
  | Xmlm.Error ((line, col), e) ->
      Error (Printf.sprintf "%d:%d: %s" line col (Xmlm.error_message e))
