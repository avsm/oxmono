(* SPDX-License-Identifier: ISC *)
open Json

let directory = ".tangled/workflows"

(* Any regular file is a workflow, as in the reference spindle. Its name is
   shown and logged, so it must be printable UTF-8. *)
let rec valid_name name =
  name <> "" && name <> "." && name <> ".."
  && String.length name <= 255
  && String.is_valid_utf_8 name
  && String.for_all (fun c -> c <> '/' && c >= ' ' && c <> '\127') name
  && printable name

(* No C1 control, line or paragraph separator, or bidirectional control,
   which could disguise a name where it is shown. *)
and printable name =
  let rec go i =
    i >= String.length name
    ||
    let d = String.get_utf_8_uchar name i in
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    (not
       ((u >= 0x80 && u <= 0x9f)
       || u = 0x2028 || u = 0x2029 || u = 0x200e || u = 0x200f
       || (u >= 0x202a && u <= 0x202e)
       || (u >= 0x2066 && u <= 0x2069)))
    && go (i + Uchar.utf_decode_length d)
  in
  go 0

(* [braces p] is [p] with each [{a,b}] group expanded, at most 256
   patterns, or [None] for more. A group without its closing brace is
   literal text. *)
let braces p =
  let rec expand acc = function
    | [] -> Some (List.rev acc)
    | p :: rest -> (
        if List.length acc > 256 then None
        else
          match String.index_opt p '{' with
          | None -> expand (p :: acc) rest
          | Some open_ -> (
              match String.index_from_opt p open_ '}' with
              | None -> expand (p :: acc) rest
              | Some close ->
                  let before = String.sub p 0 open_
                  and after =
                    String.sub p (close + 1) (String.length p - close - 1)
                  in
                  let alts =
                    String.split_on_char ','
                      (String.sub p (open_ + 1) (close - open_ - 1))
                  in
                  expand acc (List.map (fun a -> before ^ a ^ after) alts @ rest)
              ))
  in
  expand [] [ p ]

(* Glob matching as doublestar does it: [*] and [?] stay within a path
   segment, a [**] segment crosses them, and [\[...\]] selects. Each pair of
   positions is decided once, so matching takes time quadratic in the
   lengths, whatever the pattern. *)
let glob p s =
  let pl = String.length p and sl = String.length s in
  let memo = Hashtbl.create 64 in
  let rec go pi si =
    match Hashtbl.find_opt memo (pi, si) with
    | Some r -> r
    | None ->
        let r = step pi si in
        Hashtbl.replace memo (pi, si) r;
        r
  and step pi si =
    if pi = pl then si = sl
    else
      match p.[pi] with
      | '*'
        when pi + 1 < pl
             && p.[pi + 1] = '*'
             && (pi = 0 || p.[pi - 1] = '/')
             && (pi + 2 = pl || p.[pi + 2] = '/') ->
          (* A whole [**] segment matches any number of segments, and the
             rest of the pattern starts at a segment boundary. *)
          if pi + 2 = pl then true
          else
            let rest = pi + 3 in
            let rec try_from i =
              i <= sl
              && (((i = si || s.[i - 1] = '/') && go rest i) || try_from (i + 1))
            in
            try_from si
      | '*' ->
          let next =
            if pi + 1 < pl && p.[pi + 1] = '*' then pi + 2 else pi + 1
          in
          let rec try_from i =
            i <= sl
            && (go next i || (i < sl && s.[i] <> '/' && try_from (i + 1)))
          in
          try_from si
      | '?' -> si < sl && s.[si] <> '/' && go (pi + 1) (si + 1)
      | '[' -> (
          match String.index_from_opt p (pi + 1) ']' with
          | None -> si < sl && s.[si] = '[' && go (pi + 1) (si + 1)
          | Some close ->
              si < sl
              && s.[si] <> '/'
              &&
              let body = String.sub p (pi + 1) (close - pi - 1) in
              let negate = body <> "" && (body.[0] = '^' || body.[0] = '!') in
              let body =
                if negate then String.sub body 1 (String.length body - 1)
                else body
              in
              let c = s.[si] in
              let rec member i =
                i < String.length body
                &&
                if i + 2 < String.length body && body.[i + 1] = '-' then
                  (c >= body.[i] && c <= body.[i + 2]) || member (i + 3)
                else c = body.[i] || member (i + 1)
              in
              member 0 <> negate && go (close + 1) (si + 1))
      | '\\' when pi + 1 < pl ->
          si < sl && s.[si] = p.[pi + 1] && go (pi + 2) (si + 1)
      | c -> si < sl && s.[si] = c && go (pi + 1) (si + 1)
  in
  go 0 0

let matches name patterns =
  List.exists
    (fun p ->
      match braces p with
      | Some ps -> List.exists (fun p -> glob p name) ps
      | None -> false)
    patterns

(* A YAML scalar or list of scalars. *)
let string_list = function
  | None -> []
  | Some (Jsont.String (s, _)) -> [ s ]
  | Some (Jsont.Array (xs, _)) -> List.map string xs
  | Some _ -> invalid "expected a string or a list of strings"

let scalar = function
  | Jsont.String (s, _) -> s
  | Jsont.Number (n, _) when Float.is_integer n -> Printf.sprintf "%.0f" n
  | Jsont.Number (n, _) -> Printf.sprintf "%g" n
  | Jsont.Bool (b, _) -> string_of_bool b
  | Jsont.Null _ -> ""
  | _ -> invalid "environment values must be scalars"

let environment = function
  | None | Some (Jsont.Null _) -> []
  | Some v -> List.map (fun (k, v) -> (k, scalar v)) (members v)

let trigger_kind trigger =
  match get "$type" trigger with
  | "org.tangled.event.push" -> "push"
  | "org.tangled.event.pullRequest" -> "pull_request"
  | "org.tangled.ci.trigger.manual" -> "manual"
  | other -> other

let short ref_ prefix =
  if String.starts_with ~prefix ref_ then
    Some
      (String.sub ref_ (String.length prefix)
         (String.length ref_ - String.length prefix))
  else None

let constraint_matches trigger c =
  let kind = trigger_kind trigger in
  List.mem kind (string_list (field "event" c))
  && (match kind with
    | "pull_request" ->
        let target =
          Option.fold ~none:"" ~some:string (field "targetBranch" trigger)
        in
        matches target (string_list (field "branch" c))
        &&
        let action =
          Option.fold ~none:"opened" ~some:string (field "action" trigger)
        in
        let types =
          match string_list (field "types" c) with
          | [] -> [ "opened"; "reopened"; "synchronize" ]
          | types -> types
        in
        List.mem action types
    | "push" -> (
        let ref_ = get "ref" trigger in
        match (short ref_ "refs/heads/", short ref_ "refs/tags/") with
        | Some branch, _ -> matches branch (string_list (field "branch" c))
        | None, Some tag -> matches tag (string_list (field "tag" c))
        | None, None -> false)
    | _ -> true)
  (* Knots report no changed paths, so a [paths] filter skips a push and
     never matches anything else, as in the reference spindle. *)
  && (string_list (field "paths" c) = [] || kind = "push")

let workflow_matches trigger w =
  trigger_kind trigger = "manual"
  ||
  match field "when" w with
  | None | Some (Jsont.Null _) -> true
  | Some (Jsont.Array ([], _)) -> true
  | Some cs -> List.exists (constraint_matches trigger) (list cs)

let nsid = function
  | "push" -> "org.tangled.event.push"
  | "pull_request" -> "org.tangled.event.pullRequest"
  | "manual" -> "org.tangled.ci.trigger.manual"
  | "schedule" -> "org.tangled.ci.trigger.schedule"
  | other -> other

let definition ~repo ~commit name w =
  let triggers =
    match field "when" w with
    | Some (Jsont.Array (cs, _)) ->
        List.concat_map
          (fun c -> List.map nsid (string_list (field "event" c)))
          cs
    | _ -> []
  in
  obj
    [
      ("id", str name);
      ("name", str name);
      ( "source",
        obj
          [
            ("$type", str "org.tangled.ci.defs#fileSource");
            ("repo", str repo);
            ("commit", str commit);
            ("path", str (directory ^ "/" ^ name));
          ] );
      ("triggers", arr (List.map str triggers));
    ]

let parse contents =
  if String.length contents > 262144 then invalid "workflow exceeds 256 KiB";
  match
    Yamlt.decode_string ~max_depth:32 ~max_nodes:65536 Jsont.json contents
  with
  | Ok (Jsont.Object _ as w) -> w
  | Ok _ -> invalid "a workflow must be a mapping"
  | Error e -> invalid e

let job ~repo ~commit name contents w =
  if field "engine" w = None then invalid "missing engine";
  let steps =
    match field "steps" w with
    | None | Some (Jsont.Null _) -> []
    | Some steps ->
        List.mapi
          (fun i step ->
            Job.step
              ~env:(environment (field "environment" step))
              (Option.fold
                 ~none:(Printf.sprintf "step %d" (i + 1))
                 ~some:string (field "name" step))
              (* YAML reads [true] or [1] as other than text, which the
                 reference spindle takes as the command's text. *)
              (scalar (required "command" step)))
          (list steps)
  in
  let clone =
    match Option.bind (field "clone" w) (field "skip") with
    | Some (Jsont.Bool (true, _)) -> false
    | _ -> true
  in
  Job.workflow
    ~env:(environment (field "environment" w))
    ~clone
    ~definition:(definition ~repo ~commit name w)
    ~yaml:contents name steps

let definitions ~repo ~commit files =
  List.filter_map
    (fun (name, contents) ->
      if not (valid_name name) then None
      else
        match parse contents with
        | w -> Some (definition ~repo ~commit name w)
        | exception Invalid _ -> None)
    files

let select ~repo ~commit ~trigger ~names files =
  List.fold_left
    (fun (jobs, errors) (name, contents) ->
      if names <> [] && not (List.mem name names) then (jobs, errors)
      else if not (valid_name name) then
        (jobs, (name, "unsupported workflow file name") :: errors)
      else
        match
          let w = parse contents in
          if workflow_matches trigger w then
            Some (job ~repo ~commit name contents w)
          else None
        with
        | Some j -> (j :: jobs, errors)
        | None -> (jobs, errors)
        | exception Invalid e -> (jobs, (name, e) :: errors))
    ([], []) files
  |> fun (jobs, errors) -> (List.rev jobs, List.rev errors)
