module M = Httpz.Multipart
module H = Httpz.Header_name

type part = M.part = {
  name : string;
  filename : string option;
  content_type : string option;
  headers : (string * string) list;
  off : int;
  len : int;
}

let content (req : Req.t @ local) (p : part) =
  let content = Pct.copy (Req.body req) p.off p.len in
  content

let of_req ?(max_parts = 256) (req : Req.t @ local) =
  if max_parts < 0 then
    invalid_arg "Proffer.Multipart.of_req: max_parts is negative";
  let ct = Req.header req H.Content_type in
  let boundary =
    match ct with None -> None | Some ct -> M.boundary_of_content_type ct
  in
  match boundary with
  | None -> Error (Httpz.Media.Unsupported (Req.globalize_opt ct))
  | Some boundary -> (
      match M.parse ~max_parts ~boundary (Req.body req) with
      | Ok parts -> Ok parts
      | Error "too many parts" ->
        Error
          (Httpz.Media.Malformed
             (Httpz.Media.malformed
                (Printf.sprintf "body has more than %d multipart parts" max_parts)))
      | Error reason ->
          Error (Httpz.Media.Malformed (Httpz.Media.malformed reason)))

let rec find_field parts name =
  match parts with
  | [] -> None
  | (p : part) :: rest ->
      if Option.is_none p.filename && String.equal p.name name then Some p
      else find_field rest name

let field (req : Req.t @ local) parts name =
  match find_field parts name with
  | None -> None
  | Some p -> Some (content req p)

let rec file parts name =
  match parts with
  | [] -> None
  | (p : part) :: rest ->
      if Option.is_some p.filename && String.equal p.name name then Some p
      else file rest name

(* Written as a loop rather than through [List.filter_map]: the request is
   local, so it cannot cross into a closure that library takes at global. *)
let fields (req : Req.t @ local) parts =
  let rec go acc = function
    | [] -> List.rev acc
    | (p : part) :: rest ->
        if Option.is_some p.filename then go acc rest
        else go ((p.name, content req p) :: acc) rest
  in
  let r = go [] parts in
  r
