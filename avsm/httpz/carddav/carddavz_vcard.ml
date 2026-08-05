(* carddavz_vcard.ml - vCard 4.0 parser/serializer per RFC 6350 *)

type param_value =
  | Ptext of string
  | Pquoted of string

type param = {
  pname : string;
  pvalues : param_value list;
}

type property = {
  group : string option;
  name : string;
  params : param list;
  value : string;
}

type t = {
  properties : property list;
}

(* Phase 1: Unfold content lines (RFC 6350 Section 3.2)
   CRLF followed by a single space or tab is a continuation. *)
let unfold s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    if !i + 2 < len &&
       Char.equal (String.get s !i) '\r' &&
       Char.equal (String.get s (!i + 1)) '\n' &&
       (Char.equal (String.get s (!i + 2)) ' ' || Char.equal (String.get s (!i + 2)) '\t')
    then
      i := !i + 3  (* skip CRLF + WSP *)
    else begin
      Buffer.add_char buf (String.get s !i);
      incr i
    end
  done;
  Buffer.contents buf

(* Split on CRLF to get individual content lines *)
let split_lines s =
  let lines = ref [] in
  let len = String.length s in
  let start = ref 0 in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len &&
       Char.equal (String.get s !i) '\r' &&
       Char.equal (String.get s (!i + 1)) '\n'
    then begin
      lines := String.sub s !start (!i - !start) :: !lines;
      i := !i + 2;
      start := !i
    end else
      incr i
  done;
  if !start < len then
    lines := String.sub s !start (len - !start) :: !lines;
  List.rev !lines

(* Parse a single parameter value (possibly quoted) *)
let parse_param_value s =
  let len = String.length s in
  if len >= 2 && Char.equal (String.get s 0) '"' && Char.equal (String.get s (len - 1)) '"'
  then Pquoted (String.sub s 1 (len - 2))
  else Ptext s

(* Split a string on a character, returning list of substrings *)
let split_on_char c s =
  let result = ref [] in
  let len = String.length s in
  let start = ref 0 in
  for i = 0 to len - 1 do
    if Char.equal (String.get s i) c then begin
      result := String.sub s !start (i - !start) :: !result;
      start := i + 1
    end
  done;
  result := String.sub s !start (len - !start) :: !result;
  List.rev !result

(* Parse parameters from the name;params portion *)
let parse_params parts =
  List.map (fun part ->
    match String.index_opt part '=' with
    | Some eq_pos ->
      let pname = String.uppercase_ascii (String.sub part 0 eq_pos) in
      let pvalue_str = String.sub part (eq_pos + 1) (String.length part - eq_pos - 1) in
      let pvalues = split_on_char ',' pvalue_str |> List.map parse_param_value in
      { pname; pvalues }
    | None ->
      { pname = String.uppercase_ascii part; pvalues = [] })
    parts

(* Parse a content line: [group"."]name[";param"]*":"value *)
let parse_content_line line =
  (* Find the colon separating name+params from value.
     Be careful: quoted param values can contain colons. *)
  let len = String.length line in
  let in_quotes = ref false in
  let colon_pos = ref (-1) in
  let i = ref 0 in
  while !i < len && !colon_pos < 0 do
    let c = String.get line !i in
    if Char.equal c '"' then in_quotes := not !in_quotes
    else if Char.equal c ':' && not !in_quotes then colon_pos := !i;
    incr i
  done;
  if !colon_pos < 0 then None
  else begin
    let name_params = String.sub line 0 !colon_pos in
    let value = String.sub line (!colon_pos + 1) (len - !colon_pos - 1) in
    let parts = split_on_char ';' name_params in
    match parts with
    | [] -> None
    | name_part :: param_parts ->
      let group, name =
        match String.index_opt name_part '.' with
        | Some dot_pos ->
          (Some (String.sub name_part 0 dot_pos),
           String.uppercase_ascii (String.sub name_part (dot_pos + 1) (String.length name_part - dot_pos - 1)))
        | None -> (None, String.uppercase_ascii name_part)
      in
      let params = parse_params param_parts in
      Some { group; name; params; value }
  end

let parse s =
  let unfolded = unfold s in
  let lines = split_lines unfolded in
  (* Find BEGIN:VCARD and END:VCARD boundaries *)
  let in_vcard = ref false in
  let props = ref [] in
  List.iter (fun line ->
    if String.length line = 0 then ()
    else
      let upper = String.uppercase_ascii line in
      if String.equal upper "BEGIN:VCARD" then begin
        in_vcard := true;
        props := []
      end else if String.equal upper "END:VCARD" then
        in_vcard := false
      else if !in_vcard then
        match parse_content_line line with
        | Some prop -> props := prop :: !props
        | None -> ())
    lines;
  if !in_vcard then Error "Unterminated VCARD"
  else Ok { properties = List.rev !props }

let to_string vcard =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "BEGIN:VCARD\r\n";
  List.iter (fun prop ->
    begin match prop.group with
    | Some g -> Buffer.add_string buf g; Buffer.add_char buf '.'
    | None -> ()
    end;
    Buffer.add_string buf prop.name;
    List.iter (fun param ->
      Buffer.add_char buf ';';
      Buffer.add_string buf param.pname;
      if param.pvalues <> [] then begin
        Buffer.add_char buf '=';
        let first = ref true in
        List.iter (fun v ->
          if not !first then Buffer.add_char buf ',';
          first := false;
          match v with
          | Ptext s -> Buffer.add_string buf s
          | Pquoted s ->
            Buffer.add_char buf '"';
            Buffer.add_string buf s;
            Buffer.add_char buf '"')
          param.pvalues
      end)
      prop.params;
    Buffer.add_char buf ':';
    Buffer.add_string buf prop.value;
    Buffer.add_string buf "\r\n")
    vcard.properties;
  Buffer.add_string buf "END:VCARD\r\n";
  Buffer.contents buf

(* Accessors *)
let find_prop name vcard =
  List.find_opt (fun p -> String.equal p.name name) vcard.properties

let find_all_props name vcard =
  List.filter (fun p -> String.equal p.name name) vcard.properties

let uid vcard = Option.map (fun p -> p.value) (find_prop "UID" vcard)
let fn vcard = Option.map (fun p -> p.value) (find_prop "FN" vcard)
let version vcard = Option.map (fun p -> p.value) (find_prop "VERSION" vcard)
let emails vcard = List.map (fun p -> p.value) (find_all_props "EMAIL" vcard)
let tels vcard = List.map (fun p -> p.value) (find_all_props "TEL" vcard)
