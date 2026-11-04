let root xml =
  let input = Xmlm.make_input (`String (0, xml)) in
  let rec next () = match Xmlm.input input with
    | `El_start ((_, name), _) -> Some name
    | `Dtd _ -> next ()
    | `Data _ | `El_end -> None in
  try next () with Xmlm.Error _ -> None
let find_all needle text =
  let n = String.length needle in
  let rec loop i acc = if i + n > String.length text then List.rev acc
    else if String.sub text i n = needle then loop (i + n) (i :: acc)
    else loop (i + 1) acc in
  loop 0 []
let unwrap xml =
  if root xml <> Some "response" then Ok xml else
  (* Start tags are matched textually, since the envelope has already
     parsed as XML and holds at most one article by construction. *)
  let starts = List.filter (fun i ->
    i + 8 < String.length xml &&
    (match xml.[i + 8] with ' ' | '>' | '\n' | '\t' | '\r' | '/' -> true
     | _ -> false)) (find_all "<article" xml) in
  let ends = find_all "</article>" xml in
  match starts, List.rev ends with
  | [start], last :: _ when last > start ->
      let stop = last + String.length "</article>" in
      Ok ("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          ^ String.sub xml start (stop - start))
  | [], _ | _, [] -> Error "Publisher response contains no article"
  | _ -> Error "Publisher response contains more than one article"
