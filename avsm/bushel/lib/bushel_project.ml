(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Project entry type for Bushel *)

type t = {
  slug : string;
  title : string;
  start : int;          (** Start year *)
  finish : int option;  (** End year, None if ongoing *)
  tags : string list;
  ideas : string;       (** Ideas page reference *)
  body : string;
}

type ts = t list

(** {1 Accessors} *)

let slug { slug; _ } = slug
let title { title; _ } = title
let start { start; _ } = start
let finish { finish; _ } = finish
let tags { tags; _ } = tags
let ideas { ideas; _ } = ideas
let body { body; _ } = body

(** {1 Comparison} *)

let compare a b =
  match Int.compare a.start b.start with
  | 0 -> Int.compare (Option.value ~default:9999 b.finish) (Option.value ~default:9999 a.finish)
  | n -> n

(** {1 Lookup} *)

let lookup projects slug = List.find_opt (fun p -> p.slug = slug) projects

(** {1 Parsing} *)

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  (* Extract slug from filename *)
  let slug =
    match Frontmatter.fname fm with
    | Some fname ->
      (match Frontmatter.slug_of_fname fname with
       | Ok (s, _) -> s
       | Error _ -> "")
    | None -> ""
  in
  (* Extract date to get start year *)
  let start =
    match Frontmatter.find "date" fm with
    | Some (`String s) ->
      (try
         match String.split_on_char '-' s with
         | y :: _ -> int_of_string y
         | _ -> 2000
       with _ -> 2000)
    | _ -> 2000
  in
  (* Extract finish year *)
  let finish =
    match Frontmatter.find_string "finish" fm with
    | Some s ->
      (try
         match String.split_on_char '-' s with
         | y :: _ -> Some (int_of_string y)
         | _ -> None
       with _ -> None)
    | None -> None
  in
  let title = Frontmatter.find_string "title" fm |> Option.value ~default:"" in
  let tags = Frontmatter.find_strings "tags" fm in
  let ideas = Frontmatter.find_string "ideas" fm |> Option.value ~default:"" in
  let body = Frontmatter.body fm in
  Ok { slug; title; start; finish; tags; ideas; body }

(** {1 Pretty Printing} *)

let pp ppf p =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Project";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string p.slug;
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title p);
  pf ppf "%a: %d@," (styled `Bold string) "Start" p.start;
  (match p.finish with
   | Some year -> pf ppf "%a: %d@," (styled `Bold string) "Finish" year
   | None -> pf ppf "%a: ongoing@," (styled `Bold string) "Finish");
  let t = tags p in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  pf ppf "%a: %a@," (styled `Bold string) "Ideas" string (ideas p);
  pf ppf "@,";
  pf ppf "%a:@," (styled `Bold string) "Body";
  pf ppf "%a@," string (body p);
  pf ppf "@]"
