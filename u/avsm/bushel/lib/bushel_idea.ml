(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

type level =
  | Any
  | PartII
  | MPhil
  | PhD
  | Postdoc

let level_of_string = function
  | "Any" | "any" -> Any
  | "PartII" | "partii" -> PartII
  | "MPhil" | "mphil" -> MPhil
  | "PhD" | "phd" -> PhD
  | "postdoc" | "Postdoc" -> Postdoc
  | _ -> Any

let level_to_string = function
  | Any -> "Any"
  | PartII -> "PartII"
  | MPhil -> "MPhil"
  | PhD -> "PhD"
  | Postdoc -> "postdoctoral"

type status =
  | Available
  | Discussion
  | Ongoing
  | Completed
  | Expired

let status_of_string = function
  | "Available" | "available" -> Available
  | "Discussion" | "discussion" -> Discussion
  | "Ongoing" | "ongoing" -> Ongoing
  | "Completed" | "completed" -> Completed
  | "Expired" | "expired" -> Expired
  | _ -> Available

let status_to_string = function
  | Available -> "Available"
  | Discussion -> "Discussion"
  | Ongoing -> "Ongoing"
  | Completed -> "Completed"
  | Expired -> "Expired"

type t = {
  slug : string;
  title : string;
  level : level;
  project : string;
  status : status;
  month : int;
  year : int;
  supervisors : Sortal_schema.Contact.t list;
  students : Sortal_schema.Contact.t list;
  supervisor_handles : string list;
  student_handles : string list;
  reading : string;
  body : string;
  url : string option;
  tags : string list;
  social : Bushel_types.social option;
}

type ts = t list

let slug { slug; _ } = slug
let title { title; _ } = title
let level { level; _ } = level
let project { project; _ } = project
let status { status; _ } = status
let year { year; _ } = year
let month { month; _ } = month
let supervisors { supervisors; _ } = supervisors
let students { students; _ } = students
let supervisor_handles { supervisor_handles; _ } = supervisor_handles
let student_handles { student_handles; _ } = student_handles
let reading { reading; _ } = reading
let body { body; _ } = body
let url { url; _ } = url
let tags { tags; _ } = tags
let social { social; _ } = social

let compare a b =
  match Stdlib.compare a.status b.status with
  | 0 ->
    (match a.status with
     | Completed -> Int.compare b.year a.year
     | _ ->
       match Stdlib.compare a.level b.level with
       | 0 ->
         (match Int.compare b.year a.year with
          | 0 -> Int.compare b.month a.month
          | n -> n)
       | n -> n)
  | n -> n

let level_jsont : level Jsont.t =
  Jsont.of_of_string ~kind:"level"
    (fun s -> Ok (level_of_string s))
    ~enc:level_to_string

let status_jsont : status Jsont.t =
  Jsont.of_of_string ~kind:"status"
    (fun s -> Ok (status_of_string s))
    ~enc:status_to_string

let jsont : t Jsont.t =
  let open Jsont in
  let open Jsont.Object in
  let make title date level project status supervisor_handles student_handles tags reading url social =
    let (year, month, _) = date in
    { slug = ""; title; level; project; status;
      month; year; supervisors = []; students = [];
      supervisor_handles; student_handles; reading;
      body = ""; url; tags; social }
  in
  map ~kind:"Idea" make
  |> mem "title" string ~enc:(fun i -> i.title)
  |> mem "date" Bushel_types.ptime_date_jsont ~dec_absent:(fun () -> (2000, 1, 1))
       ~enc:(fun i -> (i.year, i.month, 1))
  |> mem "level" level_jsont ~enc:(fun i -> i.level)
  |> mem "project" string ~enc:(fun i -> i.project)
  |> mem "status" status_jsont ~enc:(fun i -> i.status)
  |> mem "supervisors" (list string) ~dec_absent:(fun () -> []) ~enc:(fun i -> i.supervisor_handles)
  |> mem "students" (list string) ~dec_absent:(fun () -> []) ~enc:(fun i -> i.student_handles)
  |> mem "tags" (list string) ~dec_absent:(fun () -> []) ~enc:(fun i -> i.tags)
  |> mem "reading" string ~dec_absent:(fun () -> "") ~enc:(fun i -> i.reading)
  |> mem "url" Bushel_types.string_option_jsont ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none ~enc:(fun i -> i.url)
  |> mem "social" (option Bushel_types.social_jsont) ~dec_absent:(fun () -> None)
       ~enc_omit:Option.is_none ~enc:(fun i -> i.social)
  |> finish

let of_frontmatter (fm : Frontmatter.t) : (t, string) result =
  let slug, date_opt =
    match Frontmatter.fname fm with
    | Some fname ->
      (match Frontmatter.slug_of_fname fname with
       | Ok (s, d) -> (s, d)
       | Error _ -> ("", None))
    | None -> ("", None)
  in
  match Frontmatter.decode jsont fm with
  | Error e -> Error e
  | Ok i ->
    let year, month =
      if i.year <> 2000 || i.month <> 1 then (i.year, i.month)
      else match date_opt with
        | Some d -> let (y, m, _) = Ptime.to_date d in (y, m)
        | None -> (2000, 1)
    in
    Ok { i with
         slug;
         year;
         month;
         body = Frontmatter.body fm }

let resolve_handle contacts handle =
  let h = if String.length handle > 0 && handle.[0] = '@'
    then String.sub handle 1 (String.length handle - 1)
    else handle
  in
  List.find_opt (fun c -> Sortal_schema.Contact.handle c = h) contacts

let resolve_contacts contacts idea =
  let resolve handles =
    List.filter_map (resolve_handle contacts) handles
  in
  { idea with
    supervisors = resolve idea.supervisor_handles;
    students = resolve idea.student_handles }

let resolve_all_contacts contacts ideas =
  List.map (resolve_contacts contacts) ideas

let pp ppf i =
  let open Fmt in
  pf ppf "@[<v>";
  pf ppf "%a: %a@," (styled `Bold string) "Type" (styled `Cyan string) "Idea";
  pf ppf "%a: %a@," (styled `Bold string) "Slug" string i.slug;
  pf ppf "%a: %a@," (styled `Bold string) "Title" string (title i);
  pf ppf "%a: %a@," (styled `Bold string) "Level" string (level_to_string (level i));
  pf ppf "%a: %a@," (styled `Bold string) "Status" string (status_to_string (status i));
  pf ppf "%a: %a@," (styled `Bold string) "Project" string (project i);
  pf ppf "%a: %04d-%02d@," (styled `Bold string) "Date" (year i) i.month;
  let sups = supervisors i in
  if sups <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Supervisors"
      (list ~sep:comma string) (List.map Sortal_schema.Contact.handle sups);
  let studs = students i in
  if studs <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Students"
      (list ~sep:comma string) (List.map Sortal_schema.Contact.handle studs);
  (match i.url with
   | Some url -> pf ppf "%a: %a@," (styled `Bold string) "URL" string url
   | None -> ());
  let t = i.tags in
  if t <> [] then
    pf ppf "%a: @[<h>%a@]@," (styled `Bold string) "Tags" (list ~sep:comma string) t;
  let r = reading i in
  if r <> "" then begin
    pf ppf "@,";
    pf ppf "%a:@," (styled `Bold string) "Reading";
    pf ppf "%a@," string r;
  end;
  pf ppf "@,";
  pf ppf "%a:@," (styled `Bold string) "Body";
  pf ppf "%a@," string (body i);
  pf ppf "@]"
