type feed = {
  title : string;
  xml_url : string;
  html_url : string option;
  groups : string list;
}

type t = { title : string option; feeds : feed list }

exception Invalid of string

let reject message = raise (Invalid message)

let web_url text =
  match Uriz.of_string text with
  | Null -> reject "invalid OPML URL"
  | This url -> (
      match (Uriz.scheme url, Uriz.host url, Uriz.userinfo url) with
      | This ("http" | "https"), This host, Null when host <> "" -> text
      | _ -> reject "OPML URLs must be absolute HTTP(S) without credentials")

let decode ?(max_bytes = 2 * 1024 * 1024) ?(max_depth = 32) ?(max_feeds = 10000)
    source =
  if max_bytes < 0 || max_depth < 1 || max_feeds < 0 then
    invalid_arg "Sortal_feed.Opml.decode: invalid limit";
  try
    if String.length source > max_bytes then reject "OPML byte limit exceeded";
    let input = Xmlm.make_input ~strip:false (`String (0, source)) in
    let title = ref None and feeds = ref [] and count = ref 0 in
    let seen = Hashtbl.create 64 in
    let attr attrs name = List.assoc_opt ("", name) attrs in
    let label attrs =
      match (attr attrs "title", attr attrs "text") with
      | Some s, _ when s <> "" -> s
      | _, Some s -> s
      | _ -> ""
    in
    let body_seen = ref false in
    let rec element depth path groups ((ns, name), attrs) =
      if depth > max_depth then reject "OPML depth limit exceeded";
      if depth = 1 then begin
        if ns <> "" || name <> "opml" then reject "expected OPML root";
        match attr attrs "version" with
        | Some ("1.0" | "1.1" | "2.0") -> ()
        | _ -> reject "unsupported OPML version"
      end;
      let path = (ns, name) :: path in
      if path = [ ("", "body"); ("", "opml") ] then begin
        if !body_seen then reject "duplicate OPML body";
        body_seen := true
      end;
      let rec outline_path = function
        | [ ("", "body"); ("", "opml") ] -> true
        | ("", "outline") :: rest -> outline_path rest
        | _ -> false
      in
      let in_body = outline_path path in
      let groups =
        if ns = "" && name = "outline" && in_body then begin
          let label = label attrs in
          (match attr attrs "xmlUrl" with
          | Some url ->
              incr count;
              if !count > max_feeds then reject "OPML feed limit exceeded";
              let xml_url = web_url url in
              let html_url = Option.map web_url (attr attrs "htmlUrl") in
              if not (Hashtbl.mem seen xml_url) then begin
                Hashtbl.add seen xml_url ();
                feeds :=
                  {
                    title = (if label = "" then url else label);
                    xml_url;
                    html_url;
                    groups = List.rev groups;
                  }
                  :: !feeds
              end
          | None -> ());
          if label = "" then groups else label :: groups
        end
        else groups
      in
      let text = Buffer.create 32 in
      let rec children () =
        match Xmlm.input input with
        | `El_start tag ->
            element (depth + 1) path groups tag;
            children ()
        | `Data data ->
            Buffer.add_string text data;
            children ()
        | `El_end -> ()
        | `Dtd _ -> reject "unexpected OPML DTD"
      in
      children ();
      if path = [ ("", "title"); ("", "head"); ("", "opml") ] then
        title := Some (String.trim (Buffer.contents text))
    in
    (match Xmlm.input input with
    | `Dtd None -> ()
    | _ -> reject "OPML DTDs are forbidden");
    (match Xmlm.input input with
    | `El_start tag -> element 1 [] [] tag
    | _ -> reject "expected OPML root");
    if not (Xmlm.eoi input) then reject "trailing OPML content";
    if not !body_seen then reject "missing OPML body";
    Ok { title = !title; feeds = List.rev !feeds }
  with
  | Invalid message -> Error message
  | Xmlm.Error ((line, col), error) ->
      Error
        (Printf.sprintf "OPML at %d:%d: %s" line col (Xmlm.error_message error))
