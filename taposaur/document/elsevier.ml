open Taposaur
open Xml


let namespace = "http://www.elsevier.com/xml/svapi/article/dtd"
let field (ns, name) =
  match ns, name with
  | "http://purl.org/dc/elements/1.1/",
    ("title" | "creator" | "description") -> Some name
  | "http://prismstandard.org/namespaces/basic/2.0/",
    ("doi" | "publicationName" | "publisher" | "coverDate" | "volume" |
     "issueIdentifier" | "pageRange" | "startingPage" | "endingPage" |
     "issn") -> Some name
  | _ -> None

let author value =
  match String.index_opt value ',' with
  | None -> { Author.given = None; family = value }
  | Some i ->
      let family = String.trim (String.sub value 0 i) in
      let given = String.trim
        (String.sub value (i + 1) (String.length value - i - 1)) in
      { Author.family; given = if given = "" then None else Some given }

let metadata ?(max_bytes = 1024 * 1024) flow =
  let input = Xml.input ~max_bytes flow in
  let stack = ref [] and fields = ref [] in
  let current = ref None and core = ref false in
  let buffer = Buffer.create 256 in
  let exception Done in
  let run () =
    while true do
      match Xmlm.input input with
      | `Dtd _ -> ()
      | `El_start (tag, _) ->
          if !stack = [] && tag <>
            (namespace, "full-text-retrieval-response") then
            failwith "Unsupported publisher XML metadata";
          if List.length !stack >= 256 then raise Limit;
          if !stack = [namespace, "full-text-retrieval-response"] &&
            tag = (namespace, "coredata") then core := true;
          (match !stack with
           | (ns, "coredata") :: _ when ns = namespace && !core ->
               current := field tag; Buffer.clear buffer
           | _ -> ());
          stack := tag :: !stack
      | `Data s ->
          if !current <> None then Buffer.add_string buffer s
      | `El_end ->
          (match !stack with
           | (ns, "coredata") :: _ when ns = namespace && !core ->
               raise Done
           | _ :: ((ns, "coredata") :: _ as rest)
             when ns = namespace && !core ->
               Option.iter (fun key ->
                 Option.iter (fun value ->
                   fields := (key, value) :: !fields)
                   (Xml.non_empty buffer)) !current;
               current := None; stack := rest
           | _ :: rest ->
               stack := rest;
               if rest = [] then failwith "Publisher metadata not found"
           | [] -> failwith "Unexpected closing element")
    done
  in
  try
    (try run () with Done -> ());
    let fields = List.rev !fields in
    let get key = List.assoc_opt key fields in
    let pages = match get "pageRange" with
      | Some _ as value -> value
      | None -> match get "startingPage", get "endingPage" with
        | Some a, Some b when a <> b -> Some (a ^ "–" ^ b)
        | a, _ -> a in
    Ok { Bibliography.empty with
      title = get "title"; doi = get "doi";
      authors = List.filter_map (fun (key, value) ->
        if key = "creator" then Some (author value) else None) fields;
      journal = get "publicationName"; publisher = get "publisher";
      published = get "coverDate"; abstract = get "description";
      volume = get "volume"; issue = get "issueIdentifier";
      pages; issn = get "issn" }
  with
  | Limit -> Error "Publisher metadata exceeds byte or nesting limit"
  | Failure message -> Error message
  | End_of_file -> Error "Truncated publisher metadata"
  | Xmlm.Error ((line, col), e) ->
      Error (Printf.sprintf "%d:%d: %s" line col (Xmlm.error_message e))
