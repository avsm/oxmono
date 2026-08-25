(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  get : key:string -> Base_bigstring.t option;
  get_range : key:string -> Byte_range.t -> Base_bigstring.t option;
  get_ranges : key:string -> Byte_range.t list -> Base_bigstring.t list option;
  size : key:string -> int option;
  ranged : bool;
  set : (key:string -> Base_bigstring.t -> unit) option;
  erase : (key:string -> unit) option;
  list : (prefix:string -> string list) option;
}

let has_prefix ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.equal (String.sub s 0 n) prefix

let memory () =
  let h : (string, Base_bigstring.t) Hashtbl.t = Hashtbl.create 16 in
  (* Both directions copy. The [bytes] codec gives a decoded slab a view
     of the buffer the store handed back, so without the copy on [get] a
     write through the slab would reach into the stored object, and
     without the one on [set] a later write through the slab a caller
     encoded from would. *)
  let get ~key = Option.map Base_bigstring.copy (Hashtbl.find_opt h key) in
  let slice b r =
    let pos, len = Byte_range.resolve ~size:(Base_bigstring.length b) r in
    Base_bigstring.sub b ~pos ~len
  in
  let get_range ~key r =
    Option.map (fun b -> slice b r) (Hashtbl.find_opt h key)
  in
  let get_ranges ~key rs =
    Option.map (fun b -> List.map (slice b) rs) (Hashtbl.find_opt h key)
  in
  let size ~key = Option.map Base_bigstring.length (Hashtbl.find_opt h key) in
  let set ~key b = Hashtbl.replace h key (Base_bigstring.copy b) in
  let erase ~key = Hashtbl.remove h key in
  let list ~prefix =
    let keys =
      Hashtbl.fold
        (fun k _ acc -> if has_prefix ~prefix k then k :: acc else acc)
        h []
    in
    List.sort String.compare keys
  in
  {
    get;
    get_range;
    get_ranges;
    size;
    ranged = true;
    set = Some set;
    erase = Some erase;
    list = Some list;
  }

let get_json t ~key =
  match t.get ~key with
  | None -> Error.raise_ (Error.Store (Printf.sprintf "%s: not found" key))
  | Some b -> (
      match Jsont_bytesrw.decode_string Jsont.json (Base_bigstring.to_string b)
      with
      | Ok j -> j
      | Error m ->
          Error.raise_ (Error.Metadata (Printf.sprintf "%s: %s" key m)))
