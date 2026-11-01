(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type 'a t = {
  content_type : string;
  version : string;
  decode : string -> ('a, string) result;
  encode : 'a -> (string, string) result;
}

let v ?(content_type = "text/calendar") ?(version = "2.0") ~decode ~encode () =
  { content_type; version; decode; encode }

let map ~decode ~encode c =
  {
    content_type = c.content_type;
    version = c.version;
    decode = (fun s -> Result.bind (c.decode s) decode);
    encode = (fun b -> Result.bind (encode b) c.encode);
  }

let raw = v ~decode:Result.ok ~encode:Result.ok ()

let ical =
  v ~decode:Ical.one_of_string ~encode:(fun x -> Ok (Ical.to_string x)) ()

let calendar_data c =
  Caldav_calendar_data.v ~content_type:c.content_type ~version:c.version ()

let uid c v =
  match Result.bind (c.encode v) Ical.one_of_string with
  | Error _ -> None
  | Ok cal -> Ical.uid cal
