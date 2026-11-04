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
exception Body_done

type inspection = {
  bibliography : Bibliography.t;
  text : bool;
  derived_abstract : bool;
}

(* A body section heading that announces the abstract: "Abstract",
   "Abstract.", "ABSTRACT:", "Summary". Anything longer is a real section. *)
let abstract_heading head =
  let h = String.lowercase_ascii (String.trim head) in
  let h = String.trim (String.map (function
    | '.' | ':' | '-' | '\xe2' | '\x80' | '\x94' -> ' ' | c -> c) h) in
  h = "abstract" || h = "summary" || h = "abstract summary"

let collapse s =
  let b = Buffer.create (String.length s) in
  let space = ref true in
  String.iter (function
    | ' ' | '\n' | '\r' | '\t' ->
        if not !space then (Buffer.add_char b ' '; space := true)
    | c -> Buffer.add_char b c; space := false) s;
  let s = Buffer.contents b in
  let n = String.length s in
  if n > 0 && s.[n - 1] = ' ' then String.sub s 0 (n - 1) else s

(* Element context as a stack of (local-name, attributes), innermost
   first. Namespaces are ignored, the header dialect is unambiguous
   on local names.  Character data is routed to a field buffer by
   the pattern of enclosing elements, which keeps a persName in a
   back-matter reference or a title in the body from polluting the
   header fields.  Parsing runs past the teiHeader into the start of
   the body, because Pub2TEI puts the abstract in text/front where
   GROBID puts it in the header, and because GROBID's header model
   sometimes misses the abstract altogether and leaves it in the body:
   as a section headed "Abstract", or as the unheaded paragraphs before
   the first headed section.  The body scan is bounded to the first few
   top-level sections and stops as soon as the abstract is settled and
   some body text has been seen, so the reference list is never
   reached.  The scan also reports whether the body holds any text at
   all, which tells an empty conversion from a real one. *)
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
  (* Body scan state. [body] is the stack depth of the body element. A
     block is a top-level div, or a run of paragraphs directly under the
     body, which GROBID emits for text before any section. *)
  let body = ref None and blocks = ref 0 and text_found = ref false in
  let head = Buffer.create 64 and para = Buffer.create 4096 in
  let first_para = Buffer.create 2048 and block_paras = ref 0 in
  let headed = ref false and in_block = ref false and head_open = ref false in
  let headed_abstract = ref None and leading = ref None in
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
  (* Only the first blocks can hold a misplaced abstract, and only the
     first unheaded block can be the leading paragraphs. *)
  let max_blocks = 4 in
  let settled () = !headed_abstract <> None ||
    (!blocks >= max_blocks) in
  (* An unheaded leading block is the abstract when it is short, which
     also keeps a structured abstract's several paragraphs together. When
     GROBID has run the abstract and the introduction into one unheaded
     block, only the first paragraph is the abstract. *)
  let close_block () =
    if !in_block then begin
      let text = collapse (Buffer.contents para) in
      let n = String.length text in
      if !headed && abstract_heading (Buffer.contents head) && n > 0 then
        (if !headed_abstract = None then headed_abstract := Some text)
      else if !blocks = 0 && not !headed then begin
        let first = collapse (Buffer.contents first_para) in
        let m = String.length first in
        if n >= 200 && n <= 3000 then leading := Some text
        else if m >= 200 && m <= 6000 then leading := Some first
      end;
      incr blocks;
      Buffer.clear head; Buffer.clear para; Buffer.clear first_para;
      block_paras := 0;
      headed := false; head_open := false; in_block := false
    end;
    if settled () && !text_found then raise Body_done in
  (* Figures, notes, tables and formulae are not prose; their text would
     pad an abstract or make an empty conversion look populated. *)
  let rec skipped = function
    | [] -> false
    | (("figure" | "note" | "table" | "formula"), _) :: _ -> true
    | _ :: rest -> skipped rest in
  let run () =
    while true do
      match Xmlm.input i with
      | `Dtd _ -> ()
      | `Data d ->
        (match !body with
         | None ->
           Option.iter (fun key ->
             if not (Hashtbl.mem extra_done key) then
               Buffer.add_string (extra_buffer key) d) (extra_field !stack);
           (match target () with
            | Some b -> Buffer.add_string b d
            | None -> ())
         | Some depth ->
           let within = List.filteri (fun i _ ->
             i < List.length !stack - depth - 1) !stack in
           if not (skipped within) then begin
             if !head_open then
               (if Buffer.length head < 256 then Buffer.add_string head d)
             else if List.exists (fun (n, _) -> n = "p") within then begin
               if String.trim d <> "" then text_found := true;
               if !in_block && Buffer.length para < 16384 then
                 Buffer.add_string para d;
               if !in_block && !block_paras = 1 &&
                  Buffer.length first_para < 8192 then
                 Buffer.add_string first_para d
             end
           end)
      | `El_start ((ns, name), atts) ->
        if !stack = [] && (name <> "TEI" ||
          (ns <> "" && ns <> "http://www.tei-c.org/ns/1.0")) then
          failwith "Expected a TEI document";
        let atts =
          List.filter_map
            (fun ((ns, n), v) -> if ns = "" then Some ((ns, n), v) else None)
            atts
        in
        (match !body with
         | None ->
           if name = "body" then begin
             if non_empty c.abstract <> None then c.abstract_done <- true;
             body := Some (List.length !stack)
           end;
           (* Dates carry their value in @when and may be empty
              elements, so read them at the start tag: the publication
              date from the imprint, with the fileDesc publicationStmt
              as fallback (GROBID puts it there for PDF extractions). *)
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
             attr "from" atts <> None then Hashtbl.replace extra_done "page" ()
         | Some depth ->
           if List.length !stack = depth + 1 then begin
             if name = "div" then begin
               close_block ();
               in_block := true
             end else if name = "p" && not !in_block then
               in_block := true
           end;
           if name = "p" && !in_block then incr block_paras;
           if List.length !stack = depth + 2 && name = "head" && !in_block
              && Buffer.length para = 0 then (headed := true; head_open := true));
        if List.length !stack >= 256 then raise Limit;
        stack := (name, atts) :: !stack
      | `El_end ->
        match !stack with
        | [] -> raise Header_done
        | (name, attrs) :: rest ->
          (match !body with
           | Some depth ->
             if List.length rest = depth then begin
               close_block (); raise Body_done
             end;
             if name = "head" && !head_open then head_open := false;
             if name = "div" && List.length rest = depth + 1 then
               close_block ();
             (* A paragraph directly under the body ends its block only
                when a section follows, so a run of them stays together.
                Paragraph and sentence breaks carry no whitespace of
                their own, so each becomes a space. *)
             if (name = "p" || name = "s") && !in_block then begin
               Buffer.add_char para ' ';
               if !block_paras = 1 then Buffer.add_char first_para ' '
             end
           | None ->
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
              (* An empty header abstract, as GROBID emits when its model
                 finds none, must not close the field: the Pub2TEI front
                 matter or the body may still supply it. *)
              | "abstract", _ ->
                if non_empty c.abstract <> None then c.abstract_done <- true
              | _ -> ()));
          stack := rest;
          if rest = [] then raise Header_done
    done
  in
  let finish () =
    let field key = Option.bind (Hashtbl.find_opt extra key) non_empty in
    let header = non_empty c.abstract in
    let derived = match !headed_abstract with
      | Some _ as a -> a | None -> !leading in
    let abstract = match header with Some _ -> header | None -> derived in
    Ok {
      bibliography = {
        Bibliography.year = None;
        title = non_empty c.title;
        authors = List.rev c.authors;
        journal = non_empty c.journal;
        publisher = non_empty c.publisher;
        published =
          (match c.imprint_date with Some _ as d -> d | None -> c.stmt_date);
        abstract;
        volume = field "volume"; issue = field "issue";
        pages = field "page"; issn = field "issn";
        doi = field "doi"; pmcid = field "pmcid"; pmid = field "pmid";
      };
      text = !text_found;
      derived_abstract = header = None && derived <> None;
    } in
  match run () with
  | () | exception Header_done | exception Body_done -> finish ()
  (* A body larger than the byte budget still yields the header. *)
  | exception Limit when !body <> None -> finish ()
  | exception Xmlm.Error ((line, col), e) ->
    Error (Fmt.str "%d:%d: %s" line col (Xmlm.error_message e))
  | exception Limit -> Error "TEI header exceeds byte or nesting limit"
  | exception End_of_file -> Error "Truncated TEI header"
  | exception Failure message -> Error message
let inspect_string src =
  parse_input (Xmlm.make_input ~strip:false (`String (0, src)))

let inspect ?(max_bytes = 1024 * 1024) flow =
  let input = Xml.input ~max_bytes flow in
  try parse_input input with Limit -> Error "TEI header exceeds byte limit"

let metadata_string src = Result.map (fun i -> i.bibliography)
  (inspect_string src)
let metadata ?max_bytes flow = Result.map (fun i -> i.bibliography)
  (inspect ?max_bytes flow)

exception Output_limit

(* The body is read as blocks: a heading, paragraph or list item opens one
   and its closing tag ends it. Elements that inspect ignores (figures,
   notes, tables, formulae) are skipped as whole subtrees, so a figure
   caption inside a paragraph never joins the prose. Reading stops at the
   end of the body, before any back matter. *)
let body_text_input ~max_output i =
  if max_output <= 0 then invalid_arg "max_output must be positive";
  let out = Buffer.create 65536 and para = Buffer.create 4096 in
  let stack = ref [] and body = ref None and block = ref None and skip = ref 0 in
  let flush () =
    let text = collapse (Buffer.contents para) in
    Buffer.clear para;
    if text <> "" then begin
      if Buffer.length out > 0 then Buffer.add_string out "\n\n";
      Buffer.add_string out text;
      if Buffer.length out > max_output then raise Output_limit
    end in
  let depth () = List.length !stack in
  let run () =
    while true do
      match Xmlm.input i with
      | `Dtd _ -> ()
      | `Data d ->
        if !body <> None && !block <> None && !skip = 0 then
          if Buffer.length para < max_output then Buffer.add_string para d
      | `El_start ((ns, name), _) ->
        if !stack = [] && (name <> "TEI" ||
          (ns <> "" && ns <> "http://www.tei-c.org/ns/1.0")) then
          failwith "Expected a TEI document";
        (match !body with
         | None -> if name = "body" then body := Some (depth ())
         | Some _ ->
           if !skip > 0 then incr skip
           else (match name with
             | "figure" | "note" | "table" | "formula" -> skip := 1
             | "head" | "p" | "item" when !block = None ->
               flush (); block := Some (depth ())
             | _ -> ()));
        if depth () >= 256 then raise Limit;
        stack := name :: !stack
      | `El_end ->
        match !stack with
        | [] -> raise Header_done
        | name :: rest ->
          stack := rest;
          (match !body with
           | Some d when depth () = d -> flush (); raise Body_done
           | Some _ ->
             if !skip > 0 then decr skip
             else begin
               (match !block with
                | Some d when depth () = d -> flush (); block := None
                | _ -> ());
               if name = "s" || name = "lb" then Buffer.add_char para ' '
             end
           | None -> ());
          if rest = [] then raise Header_done
    done in
  let finish () =
    Ok (if Buffer.length out = 0 then None else Some (Buffer.contents out)) in
  match run () with
  | () | exception Header_done | exception Body_done -> finish ()
  | exception Output_limit -> Error "TEI body text exceeds output limit"
  | exception Limit -> Error "TEI exceeds byte or nesting limit"
  | exception Xmlm.Error ((line, col), e) ->
    Error (Fmt.str "%d:%d: %s" line col (Xmlm.error_message e))
  | exception End_of_file -> Error "Truncated TEI document"
  | exception Failure message -> Error message

let body_text ?(max_bytes = 16 * 1024 * 1024) ?(max_output = 8 * 1024 * 1024)
    flow =
  let input = Xml.input ~max_bytes flow in
  try body_text_input ~max_output input
  with Limit -> Error "TEI exceeds byte limit"

let body_text_string ?(max_output = 8 * 1024 * 1024) src =
  body_text_input ~max_output
    (Xmlm.make_input ~strip:false (`String (0, src)))

let validate_string ?(max_bytes = 16 * 1024 * 1024) text =
  if max_bytes < 1 then invalid_arg "max_bytes must be positive";
  if String.length text > max_bytes then Error "TEI exceeds validation limit"
  else
    let input = Xmlm.make_input (`String (0, text)) in
    let depth = ref 0 and root = ref false in
    try
      while not (Xmlm.eoi input) do
        match Xmlm.input input with
        | `Dtd _ -> ()
        | `El_start ((ns, name), _) ->
            if !depth = 0 then begin
              if !root then failwith "Multiple document roots";
              if name <> "TEI" ||
                 (ns <> "" && ns <> "http://www.tei-c.org/ns/1.0") then
                failwith "Expected TEI root";
              root := true
            end;
            incr depth;
            if !depth > 256 then raise Limit
        | `El_end -> decr depth
        | `Data _ -> ()
      done;
      if not !root || !depth <> 0 then Error "Incomplete TEI document"
      else Ok ()
    with
    | Limit -> Error "TEI exceeds validation nesting limit"
    | Failure e -> Error e
    | End_of_file -> Error "Truncated TEI document"
    | Xmlm.Error ((line, col), e) ->
        Error (Fmt.str "%d:%d: %s" line col (Xmlm.error_message e))

let render_html = Tei_html.render
