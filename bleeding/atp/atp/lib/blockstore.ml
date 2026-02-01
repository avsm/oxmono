(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type error = [ `Block_not_found of Cid.t | `Block_io_error of string ]

let pp_error ppf = function
  | `Block_not_found cid -> Fmt.pf ppf "block not found: %a" Cid.pp cid
  | `Block_io_error s -> Fmt.pf ppf "blockstore I/O error: %s" s

type Eio.Exn.err += E of error

let () = Eio_error.register_pp pp_error (function E e -> Some e | _ -> None)
let raise_error e = Eio_error.raise_ (E e)

class type readable = object
  method get : Cid.t -> string option
  method get_exn : Cid.t -> string
  method has : Cid.t -> bool
  method get_many : Cid.t list -> Block_map.with_missing
end

class type writable = object
  inherit readable
  method put : Cid.t -> string -> unit
  method put_many : Block_map.t -> unit
  method delete : Cid.t -> unit
  method delete_many : Cid.t list -> unit
  method sync : unit
end

(* Memory blockstore *)
class memory_store init =
  object
    val mutable blocks : Block_map.t = init
    method get cid = Block_map.get cid blocks

    method get_exn cid =
      match Block_map.get cid blocks with
      | Some data -> data
      | None -> raise_error (`Block_not_found cid)

    method has cid = Block_map.has cid blocks
    method get_many cids = Block_map.get_many cids blocks
    method put cid data = blocks <- Block_map.set cid data blocks
    method put_many new_blocks = blocks <- Block_map.merge blocks new_blocks
    method delete cid = blocks <- Block_map.remove cid blocks

    method delete_many cids =
      List.iter (fun cid -> blocks <- Block_map.remove cid blocks) cids

    method sync = ()
  end

let memory ?(init = Block_map.empty) () = (new memory_store init :> writable)

(* Filesystem blockstore *)
let block_path dir cid =
  let s = Cid.to_string cid in
  let prefix = String.sub s 0 (min 2 (String.length s)) in
  Eio.Path.(dir / prefix / (s ^ ".block"))

class filesystem_store dir =
  object (self)
    method get cid =
      let path = block_path dir cid in
      try Some (Eio.Path.load path)
      with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> None

    method get_exn cid =
      try
        match self#get cid with
        | Some data -> data
        | None -> raise_error (`Block_not_found cid)
      with Eio.Io _ as ex ->
        let bt = Printexc.get_raw_backtrace () in
        Eio.Exn.reraise_with_context ex bt "loading block %a from filesystem"
          Cid.pp cid

    method has cid =
      let path = block_path dir cid in
      match Eio.Path.kind ~follow:true path with
      | `Regular_file -> true
      | _ -> false
      | exception Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> false

    method get_many cids =
      let blocks, missing =
        List.fold_left
          (fun (blocks, missing) cid ->
            match self#get cid with
            | Some data -> (Block_map.set cid data blocks, missing)
            | None -> (blocks, cid :: missing))
          (Block_map.empty, []) cids
      in
      { Block_map.blocks; missing = List.rev missing }

    method put cid data =
      let path = block_path dir cid in
      let parent = Eio.Path.split path |> Option.map fst in
      Option.iter
        (fun p ->
          try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 p with _ -> ())
        parent;
      Eio.Path.save ~create:(`Or_truncate 0o644) path data

    method put_many new_blocks =
      Block_map.iter (fun cid data -> self#put cid data) new_blocks

    method delete cid =
      let path = block_path dir cid in
      try Eio.Path.unlink path
      with Eio.Io (Eio.Fs.E (Eio.Fs.Not_found _), _) -> ()

    method delete_many cids = List.iter self#delete cids
    method sync = ()
  end

let filesystem dir = (new filesystem_store dir :> writable)

(* Overlay blockstore *)
class overlay_store (top : #readable) (bottom : #readable) =
  object
    method get cid =
      match top#get cid with
      | Some _ as result -> result
      | None -> bottom#get cid

    method get_exn cid =
      match top#get cid with Some data -> data | None -> bottom#get_exn cid

    method has cid = top#has cid || bottom#has cid

    method get_many cids =
      let from_top = top#get_many cids in
      if from_top.missing = [] then from_top
      else
        let from_bottom = bottom#get_many from_top.missing in
        {
          blocks = Block_map.merge from_top.blocks from_bottom.blocks;
          missing = from_bottom.missing;
        }
  end

let overlay ~top ~bottom = (new overlay_store top bottom :> readable)

(* Cached blockstore *)
class cached_store capacity (store : #writable) =
  object (self)
    val cache : (Cid.t, string) Hashtbl.t = Hashtbl.create capacity
    val mutable access_order : Cid.t list = []

    (* Remove a CID from access order *)
    method private remove_from_order cid =
      access_order <- List.filter (fun c -> not (Cid.equal c cid)) access_order

    (* Move CID to front of access order (most recently used) *)
    method private touch cid =
      self#remove_from_order cid;
      access_order <- cid :: access_order

    method private evict_if_needed =
      if Hashtbl.length cache >= capacity then
        match List.rev access_order with
        | [] -> ()
        | oldest :: rest ->
            Hashtbl.remove cache oldest;
            access_order <- List.rev rest

    method private cache_hit cid = self#touch cid

    method get cid =
      match Hashtbl.find_opt cache cid with
      | Some data ->
          self#cache_hit cid;
          Some data
      | None ->
          store#get cid
          |> Option.map (fun data ->
              self#evict_if_needed;
              Hashtbl.add cache cid data;
              access_order <- cid :: access_order;
              data)

    method get_exn cid =
      match self#get cid with
      | Some data -> data
      | None -> raise_error (`Block_not_found cid)

    method has cid = Hashtbl.mem cache cid || store#has cid

    method get_many cids =
      let blocks, remaining =
        List.fold_left
          (fun (blocks, remaining) cid ->
            match Hashtbl.find_opt cache cid with
            | Some data ->
                self#cache_hit cid;
                (Block_map.set cid data blocks, remaining)
            | None -> (blocks, cid :: remaining))
          (Block_map.empty, []) cids
      in
      if remaining = [] then { Block_map.blocks; missing = [] }
      else
        let from_store = store#get_many (List.rev remaining) in
        Block_map.iter
          (fun cid data ->
            self#evict_if_needed;
            Hashtbl.add cache cid data;
            access_order <- cid :: access_order)
          from_store.blocks;
        {
          blocks = Block_map.merge blocks from_store.blocks;
          missing = from_store.missing;
        }

    method put cid data =
      store#put cid data;
      self#evict_if_needed;
      Hashtbl.replace cache cid data;
      self#touch cid

    method put_many new_blocks =
      store#put_many new_blocks;
      Block_map.iter
        (fun cid data ->
          self#evict_if_needed;
          Hashtbl.replace cache cid data;
          self#touch cid)
        new_blocks

    method delete cid =
      store#delete cid;
      Hashtbl.remove cache cid;
      self#remove_from_order cid

    method delete_many cids =
      store#delete_many cids;
      List.iter
        (fun cid ->
          Hashtbl.remove cache cid;
          self#remove_from_order cid)
        cids

    method sync = store#sync
  end

let cached ?(capacity = 1000) store =
  (new cached_store capacity store :> writable)

(* Read-only wrapper *)
class read_only_store (store : #readable) =
  object
    method get = store#get
    method get_exn = store#get_exn
    method has = store#has
    method get_many = store#get_many
  end

let read_only store = (new read_only_store store :> readable)
