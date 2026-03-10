(* carddavz_store.ml - File-backed address book store using Eio *)

open Webdavz

type t = {
  root : [ `Dir ] Eio.Path.t;
}

let create root = { root }

(* Resolve a path string to an Eio.Path.t relative to root *)
let resolve t path =
  (* Strip leading slash *)
  let rel = if String.length path > 0 && Char.equal (String.get path 0) '/'
    then String.sub path 1 (String.length path - 1)
    else path
  in
  Eio.Path.(t.root / rel)

let exists t ~path =
  let p = resolve t path in
  match Eio.Path.kind ~follow:true p with
  | `Not_found -> false
  | _ -> true

let is_collection t ~path =
  let p = resolve t path in
  match Eio.Path.kind ~follow:true p with
  | `Directory -> true
  | _ -> false

let read t ~path =
  let p = resolve t path in
  try Some (Eio.Path.load p)
  with _ -> None

let write t ~path ~content_type:_ data =
  let p = resolve t path in
  Eio.Path.save ~create:(`Or_truncate 0o644) p data

let delete t ~path =
  let p = resolve t path in
  try
    (* Check if it's a directory or file *)
    begin match Eio.Path.kind ~follow:true p with
    | `Directory ->
      (* Remove directory contents first *)
      Eio.Path.read_dir p |> List.iter (fun name ->
        let child = Eio.Path.(p / name) in
        Eio.Path.unlink child);
      Eio.Path.rmdir p
    | `Not_found -> ()
    | _ -> Eio.Path.unlink p
    end;
    true
  with _ -> false

let mkdir t ~path =
  let p = resolve t path in
  Eio.Path.mkdir ~perm:0o755 p

let children t ~path =
  let p = resolve t path in
  try Eio.Path.read_dir p |> List.sort String.compare
  with _ -> []

(* Generate a simple ETag from content hash *)
let etag_of_content content =
  let h = Hashtbl.hash content in
  Printf.sprintf "\"%08x\"" h

let get_properties t ~path =
  let open Xml in
  let is_coll = is_collection t ~path in
  let base_props = [
    (Prop.displayname, [pcdata (Filename.basename path)]);
  ] in
  let resourcetype =
    if is_coll then
      (Prop.resourcetype, [dav_node "collection" []])
    else
      (Prop.resourcetype, [])
  in
  let content_props =
    if is_coll then []
    else
      match read t ~path with
      | Some content ->
        [
          (Prop.getcontentlength,
           [pcdata (string_of_int (String.length content))]);
          (Prop.getetag, [pcdata (etag_of_content content)]);
          (Prop.getcontenttype, [pcdata "text/vcard"]);
        ]
      | None -> []
  in
  resourcetype :: base_props @ content_props
