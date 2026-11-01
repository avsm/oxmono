(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let name = Httpz_dav.carddav "address-data"
let allprop_name = Httpz_dav.carddav "allprop"
let prop_name = Httpz_dav.carddav "prop"
let attr_content_type = ("", "content-type")
let attr_version = ("", "version")
let attr_novalue = ("", "novalue")
let attr_name = ("", "name")
let ( let* ) = Result.bind

type t = {
  content_type : string option;
  version : string option;
  props : [ `All | `Props of (string * bool) list ];
}

let v ?content_type ?version ?(props = []) () =
  { content_type; version; props = `Props props }

let vcard4 =
  { content_type = Some "text/vcard"; version = Some "4.0"; props = `Props [] }

let equal_props a b =
  match (a, b) with
  | `All, `All -> true
  | `Props x, `Props y ->
      List.equal
        (fun (n0, v0) (n1, v1) -> String.equal n0 n1 && Bool.equal v0 v1)
        x y
  | (`All | `Props _), _ -> false

let equal (a : t) (b : t) =
  Option.equal String.equal a.content_type b.content_type
  && Option.equal String.equal a.version b.version
  && equal_props a.props b.props

let to_xml t =
  let attrs =
    (match t.content_type with
      | None | Some "text/vcard" -> []
      | Some c -> [ (attr_content_type, c) ])
    @
    match t.version with
    | None | Some "3.0" -> []
    | Some v -> [ (attr_version, v) ]
  in
  let children =
    match t.props with
    | `All -> [ Httpz_dav.empty allprop_name ]
    | `Props ps ->
        List.map
          (fun (n, novalue) ->
            let attrs =
              (attr_name, n)
              :: (if novalue then [ (attr_novalue, "yes") ] else [])
            in
            Httpz_dav.el ~attrs prop_name [])
          ps
  in
  Httpz_dav.el ~attrs name children

let prop_of_xml p =
  match Httpz_dav.attr attr_name p with
  | None -> Error "a prop has no name"
  | Some n ->
      Ok
        ( n,
          match Httpz_dav.attr attr_novalue p with
          | Some "yes" -> true
          | _ -> false )

let rec all_ok = function
  | [] -> Ok []
  | x :: xs ->
      let* x = x in
      let* xs = all_ok xs in
      Ok (x :: xs)

let of_xml x =
  if not (Httpz_dav.is name x) then
    Error "the document is not a CARDDAV:address-data"
  else
    let content_type = Httpz_dav.attr attr_content_type x in
    let version = Httpz_dav.attr attr_version x in
    match Httpz_dav.find allprop_name x with
    | Some _ -> Ok { content_type; version; props = `All }
    | None ->
        let* props =
          all_ok (List.map prop_of_xml (Httpz_dav.children prop_name x))
        in
        Ok { content_type; version; props = `Props props }

(* RFC 6352 Section 10.4: an XML parser normalises CRLF to LF, so a bare line
   feed in a response's address-data stands for a carriage return and line
   feed. A server that emitted the carriage return anyway leaves the pair
   alone, so each line feed is judged on its own rather than the whole value on
   whether any pair survived. *)
let restore_crlf s =
  let b = Buffer.create (String.length s + 16) in
  String.iteri
    (fun i c ->
      if c = '\n' && not (i > 0 && s.[i - 1] = '\r') then
        Buffer.add_string b "\r\n"
      else Buffer.add_char b c)
    s;
  Buffer.contents b

let data p =
  if not (Httpz_dav.is name p) then None
  else Some (restore_crlf (Httpz_dav.content p))

let content_type_of p =
  ( Option.value ~default:"text/vcard" (Httpz_dav.attr attr_content_type p),
    Option.value ~default:"3.0" (Httpz_dav.attr attr_version p) )
