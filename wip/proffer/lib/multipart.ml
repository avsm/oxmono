module M = Httpz_media.Multipart
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

let content_local (req : Req.t @ local) (p : part) = exclave_
  Pct.sub_local (Req.body req) p.off p.len

let of_req ?(max_parts = 256) (req : Req.t @ local) =
  (* Duplicates the check M.parse itself makes, for a message naming this
     function rather than [Httpz_media.Multipart.parse]. *)
  if max_parts < 0 then
    invalid_arg "Proffer.Multipart.of_req: max_parts is negative";
  let ct = Req.header req H.Content_type in
  let boundary =
    match ct with None -> None | Some ct -> M.boundary_of_content_type ct
  in
  match boundary with
  | None -> Error (Httpz_media.Unsupported (Req.globalize_opt ct))
  | Some boundary -> (
      let body = Req.body req in
      match M.parse ~max_parts ~boundary body with
      | Ok parts -> Ok parts
      | Error reason ->
        (* M.parse's mli promises only "a short reason", not a stable string,
           so the part-cap case is detected structurally instead of by matching
           its wording: a body that parses cleanly with room for one more part
           had no other defect, so the cap was the reason. The retry is capped
           at [max_parts + 1], not unbounded, so it cannot be used to defeat
           the part-count limit it is diagnosing. *)
        let capped =
          max_parts < max_int
          &&
          match M.parse ~max_parts:(max_parts + 1) ~boundary body with
          | Ok _ -> true
          | Error _ -> false
        in
        if capped
        then
          Error
            (Httpz_media.Malformed
               (Httpz_media.malformed
                  (Printf.sprintf "body has more than %d multipart parts" max_parts)))
        else Error (Httpz_media.Malformed (Httpz_media.malformed reason)))

let rec find_by ~want_file parts name =
  match parts with
  | [] -> None
  | (p : part) :: rest ->
    if Option.is_some p.filename = want_file && String.equal p.name name
    then Some p
    else find_by ~want_file rest name
;;

let find_field parts name = find_by ~want_file:false parts name
let file parts name = find_by ~want_file:true parts name

let field (req : Req.t @ local) parts name =
  match find_field parts name with
  | None -> None
  | Some p -> Some (content req p)
;;

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
