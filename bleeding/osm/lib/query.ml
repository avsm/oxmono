type selector = Nodes | Ways | Relations | Nwr

type filter =
  | Has of string
  | Missing of string
  | Eq of string * string
  | Regex of string * string * bool

type scope =
  | Global
  | Bbox of Bounds.t
  | Around of Point.t * float
  | Area of Reference.t

type selection = { selector : selector; filters : filter list; scope : scope }
type detail = Ids | Tags | Body | Meta
type geometry = No_geometry | Center | Bounds | Full
type 'a t = { ql : string; codec : 'a Jsont.t }

let quote s =
  let b = Buffer.create (String.length s + 2) in
  Buffer.add_char b '"';
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | c when Char.code c < 32 || Char.code c = 127 ->
          invalid_arg "QL strings must not contain control characters"
      | c -> Buffer.add_char b c)
    s;
  Buffer.add_char b '"';
  Buffer.contents b

let key s =
  if s = "" then invalid_arg "Tag key must not be empty";
  ignore (quote s)

let has k =
  key k;
  Has k

let missing k =
  key k;
  Missing k

let eq k v =
  key k;
  ignore (quote v);
  Eq (k, v)

let matches ?(case_sensitive = true) k pattern =
  key k;
  ignore (quote pattern);
  Regex (k, pattern, case_sensitive)

let global = Global
let bbox b = Bbox b

let around ~radius point =
  if (not (Float.is_finite radius)) || radius < 0. then
    invalid_arg "radius must be finite and nonnegative";
  Around (point, radius)

let area reference =
  match reference.Reference.kind with
  | Node -> invalid_arg "An area requires a way or relation"
  | Way | Relation -> Area reference

let select ?(filters = []) ~scope selector = { selector; filters; scope }

let selector = function
  | Nodes -> "node"
  | Ways -> "way"
  | Relations -> "relation"
  | Nwr -> "nwr"

let filter = function
  | Has key -> "[" ^ quote key ^ "]"
  | Missing key -> "[!" ^ quote key ^ "]"
  | Eq (key, value) -> "[" ^ quote key ^ "=" ^ quote value ^ "]"
  | Regex (key, value, case_sensitive) ->
      "[" ^ quote key ^ "~" ^ quote value
      ^ if case_sensitive then "]" else ",i]"

let scope i = function
  | Global -> ("", "")
  | Bbox b ->
      ( "",
        Printf.sprintf "(%.17g,%.17g,%.17g,%.17g)" b.south b.west b.north b.east
      )
  | Around (p, radius) ->
      ("", Printf.sprintf "(around:%.17g,%.17g,%.17g)" radius p.lat p.lon)
  | Area r ->
      let name = "area" ^ string_of_int i in
      ( Printf.sprintf "%s(%Ld);map_to_area->.%s;\n"
          (Reference.kind_name r.kind)
          r.id name,
        "(area." ^ name ^ ")" )

let statements selections =
  if selections = [] then invalid_arg "At least one selection is required";
  (* Each area selection has its own set so union branches cannot overwrite it. *)
  let statements =
    List.mapi
      (fun i s ->
        let prefix, scope = scope i s.scope in
        ( prefix,
          selector s.selector
          ^ String.concat "" (List.map filter s.filters)
          ^ scope ^ ";\n" ))
      selections
  in
  String.concat "" (List.map fst statements)
  ^ "(\n"
  ^ String.concat "" (List.map snd statements)
  ^ ");\n"

let settings timeout =
  if timeout < 1 then invalid_arg "timeout must be positive";
  Printf.sprintf "[out:json][timeout:%d];\n" timeout

let elements ?(timeout = 25) ?(detail = Body) ?(geometry = No_geometry) ?limit
    ?(recurse = false) selections =
  Option.iter
    (fun n -> if n < 1 then invalid_arg "limit must be positive")
    limit;
  let detail =
    match detail with
    | Ids -> "ids"
    | Tags -> "tags"
    | Body -> "body"
    | Meta -> "meta"
  in
  let geometry =
    match geometry with
    | No_geometry -> ""
    | Center -> " center"
    | Bounds -> " bb"
    | Full -> " geom"
  in
  let limit = match limit with None -> "" | Some n -> " " ^ string_of_int n in
  {
    ql =
      settings timeout ^ statements selections
      ^ (if recurse then "(._; >>;);\n" else "")
      ^ "out " ^ detail ^ geometry ^ limit ^ ";";
    codec = Dataset.jsont;
  }

let count_codec =
  Jsont.map Dataset.jsont
    ~dec:(fun result ->
      match Dataset.elements result with
      | [ Element.Count count ] -> count
      | _ ->
          Jsont.Error.msgf Jsont.Meta.none "Expected exactly one count element")
    ~enc:(fun count ->
      let raw =
        Jsont.Json.object'
          [
            ( ("elements", Jsont.Meta.none),
              Jsont.Json.list [ count.Element.raw ] );
          ]
      in
      Codec.read Dataset.jsont raw)

let count ?(timeout = 25) selections =
  {
    ql = settings timeout ^ statements selections ^ "out count;";
    codec = count_codec;
  }

let to_ql t = t.ql
let jsont t = t.codec

let references ?(timeout = 25) ?(recurse = false) references =
  if references = [] then invalid_arg "At least one reference is required";
  let statements =
    List.map
      (fun r ->
        Printf.sprintf "%s(%Ld);\n" (Reference.kind_name r.Reference.kind) r.id)
      references
  in
  {
    ql =
      settings timeout ^ "(\n"
      ^ String.concat "" statements
      ^ ");\n"
      ^ (if recurse then "(._; >>;);\n" else "")
      ^ "out body;";
    codec = Dataset.jsont;
  }
