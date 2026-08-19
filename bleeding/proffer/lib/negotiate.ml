type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]

let of_media s : media =
  match s with
  | "text/html" -> `Html
  | "text/markdown" -> `Markdown
  | "application/json" -> `Json
  | "application/xml" | "application/atom+xml" -> `Xml
  | other -> `Other other

(* Written out rather than left to the polymorphic compare, which is not
   portable, so a handler using it could not be stored in a route. *)
let media_equal (a : media) (b : media) =
  match (a, b) with
  | `Html, `Html | `Markdown, `Markdown | `Json, `Json | `Xml, `Xml -> true
  | `Other a, `Other b -> String.equal a b
  | (`Html | `Markdown | `Json | `Xml | `Other _), _ -> false

let quality params =
  List.fold_left
    (fun acc p ->
      match String.split_on_char '=' p with
      | [ k; value ] when String.equal (String.trim k) "q" -> (
          match float_of_string_opt (String.trim value) with
          | Some f -> f
          | None -> acc)
      | _ -> acc)
    1.0 params

let parse_one s =
  match String.split_on_char ';' s with
  | [] -> None
  | media :: params ->
      let media = String.trim media in
      if String.equal media "" then None
      else Some (of_media (String.lowercase_ascii media), quality params)

(* The sort is stable, so two types the client gave the same q keep the order
   it wrote them in, which is the order it prefers them in. *)
let of_accept = function
  | None -> []
  | Some accept ->
      String.split_on_char ',' accept
      |> List.filter_map parse_one
      |> List.stable_sort (fun (_, a) (_, b) -> Float.compare b a)
      |> List.map fst

let rec assoc m = function
  | [] -> None
  | (m', h) :: tl -> if media_equal m m' then Some h else assoc m tl

(* The client's order decides, not the site's, so a site may list its variants
   in whatever order reads best. Walking off the end takes the first variant,
   which is the site's own preference. *)
let rec pick variants = function
  | [] -> ( match variants with (_, h) :: _ -> Some h | [] -> None)
  | m :: rest -> (
      match assoc m variants with
      | Some h -> Some h
      | None -> pick variants rest)

let v variants env req =
  match pick variants (of_accept (Req.header req "accept")) with
  | Some h -> Resp.vary "Accept" (h env req)
  | None -> Resp.not_found ()
