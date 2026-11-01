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

let v ?(content_type = "text/vcard") ?(version = "4.0") ~decode ~encode () =
  { content_type; version; decode; encode }

let map ~decode ~encode c =
  {
    content_type = c.content_type;
    version = c.version;
    decode = (fun s -> Result.bind (c.decode s) decode);
    encode = (fun b -> Result.bind (encode b) c.encode);
  }

let raw = v ~decode:Result.ok ~encode:Result.ok ()

let vcard =
  v ~decode:Vcard.one_of_string ~encode:(fun x -> Ok (Vcard.to_string x)) ()

let address_data c =
  Carddav_address_data.v ~content_type:c.content_type ~version:c.version ()

let uid c v =
  match Result.bind (c.encode v) Vcard.one_of_string with
  | Error _ -> None
  | Ok card -> Option.map Vcard.Property.text (Vcard.find card "UID")
