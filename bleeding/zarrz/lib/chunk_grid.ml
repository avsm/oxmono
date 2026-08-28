(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type t = {
  array_shape : int array;
  chunk_shape : int array;
  grid_shape : int array;
}

let ceil_div a b = (a + b - 1) / b

let v ~array_shape ~chunk_shape =
  let n = Array.length chunk_shape in
  if Array.length array_shape <> n then
    Error
      (Printf.sprintf
         "chunk grid: %d chunk dimensions for a %d dimensional array" n
         (Array.length array_shape))
  else if Array.exists (fun s -> s <= 0) chunk_shape then
    Error "chunk grid: chunk lengths must be positive"
  else if Array.exists (fun s -> s < 0) array_shape then
    Error "chunk grid: array lengths must not be negative"
  else
    let array_shape = Array.copy array_shape in
    let chunk_shape = Array.copy chunk_shape in
    Ok
      {
        array_shape;
        chunk_shape;
        grid_shape = Array.map2 ceil_div array_shape chunk_shape;
      }

let dimensionality t = Array.length t.chunk_shape
let array_shape t = Array.copy t.array_shape
let chunk_shape t = Array.copy t.chunk_shape
let grid_shape t = Array.copy t.grid_shape

let check t what i =
  if Array.length i <> dimensionality t then
    invalid_arg
      (Printf.sprintf "Chunk_grid: %s has %d dimensions, the grid has %d" what
         (Array.length i) (dimensionality t))

let chunk_origin t i =
  check t "chunk index" i;
  Array.mapi (fun d x -> x * t.chunk_shape.(d)) i

let chunk_indices t j =
  check t "array index" j;
  Array.mapi (fun d x -> x / t.chunk_shape.(d)) j

let clip t i =
  check t "chunk index" i;
  let n = dimensionality t in
  let rec outside d =
    d < n && (i.(d) < 0 || i.(d) >= t.grid_shape.(d) || outside (d + 1))
  in
  if outside 0 then None
  else
    let start = Array.mapi (fun d x -> x * t.chunk_shape.(d)) i in
    let shape =
      Array.init n (fun d ->
          let avail = t.array_shape.(d) - start.(d) in
          if avail < t.chunk_shape.(d) then avail else t.chunk_shape.(d))
    in
    Some (start, shape)

let chunks_overlapping t ~start ~shape f =
  check t "subset start" start;
  check t "subset shape" shape;
  let n = dimensionality t in
  if n = 0 then f [||]
  else
    (* The chunk index box, closed at both ends. *)
    let lo = Array.make n 0 and hi = Array.make n 0 in
    let mutable empty = false in
    for d = 0 to n - 1 do
      if shape.(d) <= 0 || t.grid_shape.(d) = 0 then empty <- true
      else begin
        let first = start.(d) / t.chunk_shape.(d) in
        let last =
          min
            ((start.(d) + shape.(d) - 1) / t.chunk_shape.(d))
            (t.grid_shape.(d) - 1)
        in
        lo.(d) <- first;
        hi.(d) <- last;
        if first > last then empty <- true
      end
    done;
    if not empty then begin
      (* Odometer over the box. The last dimension varies fastest, so
         the indices come out in C order. *)
      let cur = Array.copy lo in
      let rec next d =
        if d < 0 then false
        else if cur.(d) < hi.(d) then begin
          cur.(d) <- cur.(d) + 1;
          true
        end
        else begin
          cur.(d) <- lo.(d);
          next (d - 1)
        end
      in
      let rec loop () =
        f (Array.copy cur);
        if next (n - 1) then loop ()
      in
      loop ()
    end

(* A reader cannot skip a chunk grid it does not know, so the spec does
   not allow the chunk grid extension point to carry
   must_understand false. *)
let of_ext e ~array_shape =
  if not e.Ext.must_understand then
    Error "chunk grid: must_understand must be true"
  else if not (String.equal e.Ext.name "regular") then
    Error (Printf.sprintf "chunk grid: unsupported name %S" e.Ext.name)
  else
    match e.Ext.config with
    | None -> Error "chunk grid: regular requires a chunk_shape configuration"
    | Some (Jsont.Object (mems, _)) -> (
        match
          List.find_opt (fun ((n, _), _) -> not (String.equal n "chunk_shape"))
            mems
        with
        | Some ((n, _), _) ->
            Error
              (Printf.sprintf "chunk grid: unknown configuration member %S" n)
        | None -> (
            match Jsont.Json.find_mem "chunk_shape" mems with
            | None -> Error "chunk grid: missing chunk_shape"
            | Some (_, Jsont.Array (l, _)) -> (
                let exception Bad in
                (* A [jsont] number is a float, so bound the value at
                   2^52 to keep [int_of_float] exact. *)
                let dim j =
                  match j with
                  | Jsont.Number (f, _)
                    when Float.is_integer f && f > 0.0 && f <= ldexp 1.0 52 ->
                      int_of_float f
                  | _ -> raise Bad
                in
                match Array.of_list (List.map dim l) with
                | chunk_shape -> v ~array_shape ~chunk_shape
                | exception Bad ->
                    Error "chunk grid: chunk_shape must hold positive integers")
            | Some _ -> Error "chunk grid: chunk_shape must be an array"))
    | Some _ -> Error "chunk grid: configuration must be an object"

let to_ext t =
  let shape =
    Jsont.Json.list
      (List.map
         (fun s -> Jsont.Json.number (float_of_int s))
         (Array.to_list t.chunk_shape))
  in
  Ext.v "regular"
    ~config:(Jsont.Json.object' [ (Jsont.Json.name "chunk_shape", shape) ])
