(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Bytesrw
module Rw = Cbor_rw
module Cbor = Cbor

module Error = struct
  type path = segment list

  and segment =
    | Root
    | Index of int
    | Key of string
    | Key_cbor of Cbor.t
    | Tag of int

  let pp_segment ppf = function
    | Root -> Format.fprintf ppf "$"
    | Index i -> Format.fprintf ppf "[%d]" i
    | Key k -> Format.fprintf ppf ".%s" k
    | Key_cbor k -> Format.fprintf ppf "[%a]" Cbor.pp k
    | Tag n -> Format.fprintf ppf "<%d>" n

  let pp_path ppf path = List.iter (pp_segment ppf) (List.rev path)
  let path_to_string path = Format.asprintf "%a" pp_path path

  type kind =
    | Type_mismatch of { expected : string; got : string }
    | Missing_member of string
    | Unknown_member of string
    | Duplicate_member of string
    | Out_of_range of { value : string; range : string }
    | Invalid_value of string
    | Parse_error of string
    | Custom of string

  type t = { path : path; kind : kind }

  let make path kind = { path; kind }

  let pp_kind ppf = function
    | Type_mismatch { expected; got } ->
        Format.fprintf ppf "type mismatch: expected %s, got %s" expected got
    | Missing_member name ->
        Format.fprintf ppf "missing required member: %s" name
    | Unknown_member name -> Format.fprintf ppf "unknown member: %s" name
    | Duplicate_member name -> Format.fprintf ppf "duplicate member: %s" name
    | Out_of_range { value; range } ->
        Format.fprintf ppf "value %s out of range %s" value range
    | Invalid_value msg -> Format.fprintf ppf "invalid value: %s" msg
    | Parse_error msg -> Format.fprintf ppf "parse error: %s" msg
    | Custom msg -> Format.fprintf ppf "%s" msg

  let pp ppf { path; kind } =
    Format.fprintf ppf "%a: %a" pp_path path pp_kind kind

  let to_string e = Format.asprintf "%a" pp e

  exception Decode of t
end

type 'a t = {
  encode : 'a -> Cbor.t;
  decode : Error.path -> Cbor.t -> ('a, Error.t) result;
}

let type_name (v : Cbor.t) =
  match v with
  | Int _ -> "integer"
  | Bytes _ -> "bytes"
  | Text _ -> "text"
  | Array _ -> "array"
  | Map _ -> "map"
  | Tag _ -> "tag"
  | Bool _ -> "boolean"
  | Null -> "null"
  | Undefined -> "undefined"
  | Simple _ -> "simple"
  | Float _ -> "float"

let type_error path expected v =
  Error (Error.make path (Type_mismatch { expected; got = type_name v }))

(* Base codecs *)

let null =
  {
    encode = (fun () -> Cbor.Null);
    decode =
      (fun path v ->
        match v with Cbor.Null -> Ok () | _ -> type_error path "null" v);
  }

let bool =
  {
    encode = (fun b -> Cbor.Bool b);
    decode =
      (fun path v ->
        match v with Cbor.Bool b -> Ok b | _ -> type_error path "boolean" v);
  }

let int =
  {
    encode = (fun n -> Cbor.Int (Z.of_int n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if Z.fits_int n then Ok (Z.to_int n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      {
                        value = Z.to_string n;
                        range = Printf.sprintf "[%d, %d]" min_int max_int;
                      }))
        | _ -> type_error path "integer" v);
  }

let int32 =
  {
    encode = (fun n -> Cbor.Int (Z.of_int32 n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if
              Z.geq n (Z.of_int32 Int32.min_int)
              && Z.leq n (Z.of_int32 Int32.max_int)
            then Ok (Z.to_int32 n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      {
                        value = Z.to_string n;
                        range =
                          Printf.sprintf "[%ld, %ld]" Int32.min_int
                            Int32.max_int;
                      }))
        | _ -> type_error path "integer" v);
  }

let int64 =
  {
    encode = (fun n -> Cbor.Int (Z.of_int64 n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if Z.fits_int64 n then Ok (Z.to_int64 n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      {
                        value = Z.to_string n;
                        range =
                          Printf.sprintf "[%Ld, %Ld]" Int64.min_int
                            Int64.max_int;
                      }))
        | _ -> type_error path "integer" v);
  }

let float =
  {
    encode = (fun f -> Cbor.Float f);
    decode =
      (fun path v ->
        match v with
        | Cbor.Float f -> Ok f
        | Cbor.Int n -> Ok (Z.to_float n)
        | _ -> type_error path "float" v);
  }

let string =
  {
    encode = (fun s -> Cbor.Text s);
    decode =
      (fun path v ->
        match v with Cbor.Text s -> Ok s | _ -> type_error path "text" v);
  }

let bytes =
  {
    encode = (fun s -> Cbor.Bytes s);
    decode =
      (fun path v ->
        match v with Cbor.Bytes s -> Ok s | _ -> type_error path "bytes" v);
  }

let any = { encode = Fun.id; decode = (fun _path v -> Ok v) }

(* Nullable *)

let nullable c =
  {
    encode =
      (fun opt -> match opt with None -> Cbor.Null | Some x -> c.encode x);
    decode =
      (fun path v ->
        match v with
        | Cbor.Null -> Ok None
        | _ -> Result.map Option.some (c.decode path v));
  }

let option ~default c =
  {
    encode = c.encode;
    decode =
      (fun path v ->
        match v with Cbor.Null -> Ok default | _ -> c.decode path v);
  }

(* Numeric variants *)

let uint =
  {
    encode = (fun n -> Cbor.Int (Z.of_int n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if Z.sign n >= 0 && Z.fits_int n then Ok (Z.to_int n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      {
                        value = Z.to_string n;
                        range = Printf.sprintf "[0, %d]" max_int;
                      }))
        | _ -> type_error path "integer" v);
  }

let uint32 =
  {
    encode = (fun n -> Cbor.Int (Z.of_int32 n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if Z.sign n >= 0 && Z.leq n (Z.of_string "4294967295") then
              Ok (Z.to_int32 n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      { value = Z.to_string n; range = "[0, 4294967295]" }))
        | _ -> type_error path "integer" v);
  }

let uint64 =
  {
    encode = (fun n -> Cbor.Int (Z.of_int64 n));
    decode =
      (fun path v ->
        match v with
        | Cbor.Int n ->
            if Z.sign n >= 0 && Z.fits_int64 n then Ok (Z.to_int64 n)
            else
              Error
                (Error.make path
                   (Out_of_range
                      { value = Z.to_string n; range = "[0, 2^63-1]" }))
        | _ -> type_error path "integer" v);
  }

let number = float

(* Arrays *)

let array c =
  {
    encode = (fun items -> Cbor.Array (List.map c.encode items));
    decode =
      (fun path v ->
        match v with
        | Cbor.Array items ->
            let rec loop i acc = function
              | [] -> Ok (List.rev acc)
              | x :: xs -> (
                  let path' = Error.Index i :: path in
                  match c.decode path' x with
                  | Ok v -> loop (i + 1) (v :: acc) xs
                  | Error e -> Error e)
            in
            loop 0 [] items
        | _ -> type_error path "array" v);
  }

let array_of ~len c =
  {
    encode =
      (fun items ->
        if List.length items <> len then
          failwith (Printf.sprintf "Expected array of length %d" len);
        Cbor.Array (List.map c.encode items));
    decode =
      (fun path v ->
        match v with
        | Cbor.Array items when List.length items = len ->
            let rec loop i acc = function
              | [] -> Ok (List.rev acc)
              | x :: xs -> (
                  let path' = Error.Index i :: path in
                  match c.decode path' x with
                  | Ok v -> loop (i + 1) (v :: acc) xs
                  | Error e -> Error e)
            in
            loop 0 [] items
        | Cbor.Array items ->
            Error
              (Error.make path
                 (Invalid_value
                    (Printf.sprintf "expected array of length %d, got %d" len
                       (List.length items))))
        | _ -> type_error path "array" v);
  }

let tuple2 ca cb =
  {
    encode = (fun (a, b) -> Cbor.Array [ ca.encode a; cb.encode b ]);
    decode =
      (fun path v ->
        match v with
        | Cbor.Array [ va; vb ] -> (
            match ca.decode (Error.Index 0 :: path) va with
            | Error e -> Error e
            | Ok a -> (
                match cb.decode (Error.Index 1 :: path) vb with
                | Error e -> Error e
                | Ok b -> Ok (a, b)))
        | Cbor.Array _ ->
            Error (Error.make path (Invalid_value "expected 2-element array"))
        | _ -> type_error path "array" v);
  }

let tuple3 ca cb cc =
  {
    encode =
      (fun (a, b, c) -> Cbor.Array [ ca.encode a; cb.encode b; cc.encode c ]);
    decode =
      (fun path v ->
        match v with
        | Cbor.Array [ va; vb; vc ] -> (
            match ca.decode (Error.Index 0 :: path) va with
            | Error e -> Error e
            | Ok a -> (
                match cb.decode (Error.Index 1 :: path) vb with
                | Error e -> Error e
                | Ok b -> (
                    match cc.decode (Error.Index 2 :: path) vc with
                    | Error e -> Error e
                    | Ok c -> Ok (a, b, c))))
        | Cbor.Array _ ->
            Error (Error.make path (Invalid_value "expected 3-element array"))
        | _ -> type_error path "array" v);
  }

let tuple4 ca cb cc cd =
  {
    encode =
      (fun (a, b, c, d) ->
        Cbor.Array [ ca.encode a; cb.encode b; cc.encode c; cd.encode d ]);
    decode =
      (fun path v ->
        match v with
        | Cbor.Array [ va; vb; vc; vd ] -> (
            match ca.decode (Error.Index 0 :: path) va with
            | Error e -> Error e
            | Ok a -> (
                match cb.decode (Error.Index 1 :: path) vb with
                | Error e -> Error e
                | Ok b -> (
                    match cc.decode (Error.Index 2 :: path) vc with
                    | Error e -> Error e
                    | Ok c -> (
                        match cd.decode (Error.Index 3 :: path) vd with
                        | Error e -> Error e
                        | Ok d -> Ok (a, b, c, d)))))
        | Cbor.Array _ ->
            Error (Error.make path (Invalid_value "expected 4-element array"))
        | _ -> type_error path "array" v);
  }

(* Maps *)

let assoc kc vc =
  {
    encode =
      (fun pairs ->
        Cbor.Map (List.map (fun (k, v) -> (kc.encode k, vc.encode v)) pairs));
    decode =
      (fun path v ->
        match v with
        | Cbor.Map pairs ->
            let rec loop acc = function
              | [] -> Ok (List.rev acc)
              | (ck, cv) :: rest -> (
                  let path_k = Error.Key_cbor ck :: path in
                  match kc.decode path_k ck with
                  | Error e -> Error e
                  | Ok k -> (
                      match vc.decode path_k cv with
                      | Error e -> Error e
                      | Ok v -> loop ((k, v) :: acc) rest))
            in
            loop [] pairs
        | _ -> type_error path "map" v);
  }

let string_map vc = assoc string vc
let int_map vc = assoc int vc

(* Object codec module *)

module Obj = struct
  type enc = (string * Cbor.t) list

  let field name (v : Cbor.t) (acc : enc) : enc = (name, v) :: acc

  let find_remove key pairs =
    let rec loop acc = function
      | [] -> (None, List.rev acc)
      | (k, v) :: rest when k = key -> (Some v, List.rev_append acc rest)
      | kv :: rest -> loop (kv :: acc) rest
    in
    loop [] pairs

  type (_, _) mem =
    | Return : 'a -> ('o, 'a) mem
    | Mem : {
        name : string;
        get : 'o -> 'x;
        codec : 'x t;
        cont : 'x -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem
    | Mem_opt : {
        name : string;
        get : 'o -> 'x option;
        codec : 'x t;
        cont : 'x option -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem
    | Mem_default : {
        name : string;
        get : 'o -> 'x;
        codec : 'x t;
        default : 'x;
        cont : 'x -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem

  let return v = Return v
  let mem name get codec = Mem { name; get; codec; cont = (fun x -> Return x) }

  let mem_opt name get codec =
    Mem_opt { name; get; codec; cont = (fun x -> Return x) }

  let mem_default name get ~default codec =
    Mem_default { name; get; codec; default; cont = (fun x -> Return x) }

  let rec ( let* ) : type o a b. (o, a) mem -> (a -> (o, b) mem) -> (o, b) mem =
   fun m f ->
    match m with
    | Return a -> f a
    | Mem r ->
        Mem
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }
    | Mem_opt r ->
        Mem_opt
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }
    | Mem_default r ->
        Mem_default
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }

  let rec decode_mem : type o a.
      Error.path ->
      (string * Cbor.t) list ->
      (o, a) mem ->
      (a * (string * Cbor.t) list, Error.t) result =
   fun path pairs m ->
    match m with
    | Return a -> Ok (a, pairs)
    | Mem { name; codec; cont; _ } -> (
        match find_remove name pairs with
        | None, _ -> Error (Error.make path (Missing_member name))
        | Some v, remaining -> (
            let path' = Error.Key name :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont x)))
    | Mem_opt { name; codec; cont; _ } -> (
        match find_remove name pairs with
        | None, remaining -> decode_mem path remaining (cont None)
        | Some Cbor.Null, remaining -> decode_mem path remaining (cont None)
        | Some v, remaining -> (
            let path' = Error.Key name :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont (Some x))))
    | Mem_default { name; codec; default; cont; _ } -> (
        match find_remove name pairs with
        | None, remaining -> decode_mem path remaining (cont default)
        | Some Cbor.Null, remaining -> decode_mem path remaining (cont default)
        | Some v, remaining -> (
            let path' = Error.Key name :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont x)))

  let rec encode_mem : type o a. o -> (o, a) mem -> enc -> enc =
   fun o m acc ->
    match m with
    | Return _ -> acc
    | Mem { name; get; codec; cont } ->
        let v = get o in
        let acc = field name (codec.encode v) acc in
        encode_mem o (cont v) acc
    | Mem_opt { name; get; codec; cont } ->
        let v = get o in
        let acc =
          match v with None -> acc | Some x -> field name (codec.encode x) acc
        in
        encode_mem o (cont v) acc
    | Mem_default { name; get; codec; cont; _ } ->
        let v = get o in
        let acc = field name (codec.encode v) acc in
        encode_mem o (cont v) acc

  let finish (m : ('o, 'o) mem) : 'o t =
    {
      encode =
        (fun v ->
          let fields = encode_mem v m [] in
          Cbor.Map (List.map (fun (k, v) -> (Cbor.Text k, v)) (List.rev fields)));
      decode =
        (fun path v ->
          match v with
          | Cbor.Map pairs -> (
              let text_pairs =
                List.filter_map
                  (fun (k, v) ->
                    match k with Cbor.Text s -> Some (s, v) | _ -> None)
                  pairs
              in
              match decode_mem path text_pairs m with
              | Error e -> Error e
              | Ok (result, _remaining) -> Ok result)
          | _ -> type_error path "map" v);
    }
end

(* Integer-keyed object codec module *)

module Obj_int = struct
  type enc = (int * Cbor.t) list

  let field key (v : Cbor.t) (acc : enc) : enc = (key, v) :: acc

  let find_remove key pairs =
    let rec loop acc = function
      | [] -> (None, List.rev acc)
      | (k, v) :: rest when k = key -> (Some v, List.rev_append acc rest)
      | kv :: rest -> loop (kv :: acc) rest
    in
    loop [] pairs

  type (_, _) mem =
    | Return : 'a -> ('o, 'a) mem
    | Mem : {
        key : int;
        get : 'o -> 'x;
        codec : 'x t;
        cont : 'x -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem
    | Mem_opt : {
        key : int;
        get : 'o -> 'x option;
        codec : 'x t;
        cont : 'x option -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem
    | Mem_default : {
        key : int;
        get : 'o -> 'x;
        codec : 'x t;
        default : 'x;
        cont : 'x -> ('o, 'a) mem;
      }
        -> ('o, 'a) mem

  let return v = Return v
  let mem key get codec = Mem { key; get; codec; cont = (fun x -> Return x) }

  let mem_opt key get codec =
    Mem_opt { key; get; codec; cont = (fun x -> Return x) }

  let mem_default key get ~default codec =
    Mem_default { key; get; codec; default; cont = (fun x -> Return x) }

  let rec ( let* ) : type o a b. (o, a) mem -> (a -> (o, b) mem) -> (o, b) mem =
   fun m f ->
    match m with
    | Return a -> f a
    | Mem r ->
        Mem
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }
    | Mem_opt r ->
        Mem_opt
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }
    | Mem_default r ->
        Mem_default
          {
            r with
            cont =
              (fun x ->
                let* y = r.cont x in
                f y);
          }

  let rec decode_mem : type o a.
      Error.path ->
      (int * Cbor.t) list ->
      (o, a) mem ->
      (a * (int * Cbor.t) list, Error.t) result =
   fun path pairs m ->
    match m with
    | Return a -> Ok (a, pairs)
    | Mem { key; codec; cont; _ } -> (
        match find_remove key pairs with
        | None, _ ->
            Error (Error.make path (Missing_member (string_of_int key)))
        | Some v, remaining -> (
            let path' = Error.Key (string_of_int key) :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont x)))
    | Mem_opt { key; codec; cont; _ } -> (
        match find_remove key pairs with
        | None, remaining -> decode_mem path remaining (cont None)
        | Some Cbor.Null, remaining -> decode_mem path remaining (cont None)
        | Some v, remaining -> (
            let path' = Error.Key (string_of_int key) :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont (Some x))))
    | Mem_default { key; codec; default; cont; _ } -> (
        match find_remove key pairs with
        | None, remaining -> decode_mem path remaining (cont default)
        | Some Cbor.Null, remaining -> decode_mem path remaining (cont default)
        | Some v, remaining -> (
            let path' = Error.Key (string_of_int key) :: path in
            match codec.decode path' v with
            | Error e -> Error e
            | Ok x -> decode_mem path remaining (cont x)))

  let rec encode_mem : type o a. o -> (o, a) mem -> enc -> enc =
   fun o m acc ->
    match m with
    | Return _ -> acc
    | Mem { key; get; codec; cont } ->
        let v = get o in
        let acc = field key (codec.encode v) acc in
        encode_mem o (cont v) acc
    | Mem_opt { key; get; codec; cont } ->
        let v = get o in
        let acc =
          match v with None -> acc | Some x -> field key (codec.encode x) acc
        in
        encode_mem o (cont v) acc
    | Mem_default { key; get; codec; cont; _ } ->
        let v = get o in
        let acc = field key (codec.encode v) acc in
        encode_mem o (cont v) acc

  let finish (m : ('o, 'o) mem) : 'o t =
    {
      encode =
        (fun v ->
          let fields = encode_mem v m [] in
          Cbor.Map
            (List.map
               (fun (k, v) -> (Cbor.Int (Z.of_int k), v))
               (List.rev fields)));
      decode =
        (fun path v ->
          match v with
          | Cbor.Map pairs -> (
              let int_pairs =
                List.filter_map
                  (fun (k, v) ->
                    match k with
                    | Cbor.Int n when Z.fits_int n -> Some (Z.to_int n, v)
                    | _ -> None)
                  pairs
              in
              match decode_mem path int_pairs m with
              | Error e -> Error e
              | Ok (result, _remaining) -> Ok result)
          | _ -> type_error path "map" v);
    }
end

(* Tags *)

let tag n c =
  {
    encode = (fun v -> Cbor.Tag (n, c.encode v));
    decode =
      (fun path v ->
        match v with
        | Cbor.Tag (m, content) when m = n ->
            c.decode (Error.Tag n :: path) content
        | Cbor.Tag (m, _) ->
            Error
              (Error.make path
                 (Invalid_value
                    (Printf.sprintf "expected tag %d, got tag %d" n m)))
        | _ -> type_error path (Printf.sprintf "tag(%d)" n) v);
  }

let tag_opt n c =
  {
    encode = (fun v -> Cbor.Tag (n, c.encode v));
    decode =
      (fun path v ->
        match v with
        | Cbor.Tag (m, content) when m = n ->
            c.decode (Error.Tag n :: path) content
        | _ -> c.decode path v);
  }

(* Transformations *)

let map decode_f encode_f c =
  {
    encode = (fun v -> c.encode (encode_f v));
    decode =
      (fun path v ->
        match c.decode path v with
        | Error e -> Error e
        | Ok x -> Ok (decode_f x));
  }

let conv decode_f encode_f c =
  {
    encode = (fun v -> c.encode (encode_f v));
    decode =
      (fun path v ->
        match c.decode path v with
        | Error e -> Error e
        | Ok x -> (
            match decode_f x with
            | Ok y -> Ok y
            | Error msg -> Error (Error.make path (Custom msg))));
  }

let const v c =
  {
    encode = (fun _ -> c.encode ());
    decode =
      (fun path cbor ->
        match c.decode path cbor with Error e -> Error e | Ok () -> Ok v);
  }

(* Variants *)

module Variant = struct
  type 'a case =
    | Case : int * 'b t * ('b -> 'a) * ('a -> 'b option) -> 'a case
    | Case0 : int * 'a * ('a -> bool) -> 'a case

  let case tag c inject project = Case (tag, c, inject, project)
  let case0 tag v is_v = Case0 (tag, v, is_v)

  let variant cases =
    {
      encode =
        (fun v ->
          let rec find = function
            | [] -> failwith "No matching variant case for encoding"
            | Case (tag, c, _, project) :: rest -> (
                match project v with
                | Some x -> Cbor.Tag (tag, c.encode x)
                | None -> find rest)
            | Case0 (tag, _, is_v) :: rest ->
                if is_v v then Cbor.Tag (tag, Cbor.Null) else find rest
          in
          find cases);
      decode =
        (fun path v ->
          match v with
          | Cbor.Tag (tag, content) ->
              let rec try_cases = function
                | [] ->
                    Error
                      (Error.make path
                         (Invalid_value
                            (Printf.sprintf "unknown tag %d in variant" tag)))
                | Case (t, c, inject, _) :: _rest when t = tag -> (
                    match c.decode (Error.Tag t :: path) content with
                    | Error e -> Error e
                    | Ok x -> Ok (inject x))
                | Case0 (t, v, _) :: _ when t = tag -> Ok v
                | _ :: rest -> try_cases rest
              in
              try_cases cases
          | _ -> type_error path "tag" v);
    }
end

module Variant_key = struct
  type 'a case =
    | Case : string * 'b t * ('b -> 'a) * ('a -> 'b option) -> 'a case
    | Case0 : string * 'a * ('a -> bool) -> 'a case

  let case key c inject project = Case (key, c, inject, project)
  let case0 key v is_v = Case0 (key, v, is_v)

  let variant cases =
    {
      encode =
        (fun v ->
          let rec find = function
            | [] -> failwith "No matching variant case for encoding"
            | Case (key, c, _, project) :: rest -> (
                match project v with
                | Some x -> Cbor.Map [ (Cbor.Text key, c.encode x) ]
                | None -> find rest)
            | Case0 (key, _, is_v) :: rest ->
                if is_v v then Cbor.Map [ (Cbor.Text key, Cbor.Null) ]
                else find rest
          in
          find cases);
      decode =
        (fun path v ->
          match v with
          | Cbor.Map [ (Cbor.Text key, content) ] ->
              let rec try_cases = function
                | [] ->
                    Error
                      (Error.make path
                         (Invalid_value
                            (Printf.sprintf "unknown key %S in variant" key)))
                | Case (k, c, inject, _) :: _rest when k = key -> (
                    match c.decode (Error.Key k :: path) content with
                    | Error e -> Error e
                    | Ok x -> Ok (inject x))
                | Case0 (k, v, _) :: _ when k = key -> Ok v
                | _ :: rest -> try_cases rest
              in
              try_cases cases
          | Cbor.Map _ ->
              Error
                (Error.make path
                   (Invalid_value "variant map must have exactly one key"))
          | _ -> type_error path "map" v);
    }
end

(* Recursive types *)

let fix f =
  let rec self =
    lazy
      (f
         {
           encode = (fun v -> (Lazy.force self).encode v);
           decode = (fun path v -> (Lazy.force self).decode path v);
         })
  in
  Lazy.force self

(* Decoding *)

let decode_cbor c v = c.decode [ Error.Root ] v

let decode_cbor_exn c v =
  match decode_cbor c v with Ok x -> x | Error e -> raise (Error.Decode e)

let decode c reader =
  let dec = Rw.make_decoder reader in
  try
    let cbor = Rw.read_cbor dec in
    decode_cbor c cbor
  with
  | Failure msg -> Error (Error.make [ Error.Root ] (Parse_error msg))
  | End_of_file ->
      Error (Error.make [ Error.Root ] (Parse_error "unexpected end of input"))

let decode_exn c reader =
  match decode c reader with Ok x -> x | Error e -> raise (Error.Decode e)

let decode_string c s =
  let reader = Bytes.Reader.of_string s in
  decode c reader

let decode_string_exn c s =
  match decode_string c s with Ok x -> x | Error e -> raise (Error.Decode e)

(* Encoding *)

let encode_cbor c v = c.encode v

let encode c v ~eod writer =
  let enc = Rw.make_encoder writer in
  let cbor = c.encode v in
  Rw.write_cbor enc cbor;
  Rw.flush_encoder enc;
  if eod then Bytes.Writer.write writer Bytes.Slice.eod

let encode_string c v =
  let buf = Buffer.create 256 in
  let writer = Bytes.Writer.of_buffer buf in
  encode c v ~eod:false writer;
  Buffer.contents buf
