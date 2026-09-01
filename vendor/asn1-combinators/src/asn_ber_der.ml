(* Copyright (c) 2014-2017 David Kaloper Meršinjak. All rights reserved.
   See LICENSE.md. *)

open Asn_core

module Prim   = Asn_prim
module Writer = Asn_writer
module Int64  = Prim.Int64


let (@?) oa a = match oa with Some x -> x | None -> a

module Seq = struct

  type 'r f = { f : 'a. 'a -> 'a asn -> 'r -> 'r }

  let rec fold_with_value : type a. 'r f -> 'r -> a -> a sequence -> 'r
  = fun f r a -> function
    | Last (Required (_, asn)) -> f.f a asn r
    | Last (Optional (_, asn)) ->
      ( match a with None -> r | Some a' -> f.f a' asn r )
    | Pair (Required (_, asn), asns) ->
        let (a1, a2) = a in f.f a1 asn (fold_with_value f r a2 asns)
    | Pair (Optional (_, asn), asns) ->
        match a with
        | (None   , a2) -> fold_with_value f r a2 asns
        | (Some a1, a2) -> f.f a1 asn (fold_with_value f r a2 asns)
end

module R = struct

  module G = Generic

  let rec map_portable (f @ portable) = function
    | [] -> []
    | x :: xs -> f x :: map_portable f xs

  type config = { strict : bool }

  type coding =
    | Primitive   of int
    | Constructed of int
    | Constructed_indefinite

  module Header @ portable = struct

    let error cs fmt =
      parse_error ("Header: at %a: " ^^ fmt) pp_octets cs

    let ck_redundant cs cfg (n : int) limit =
      if cfg.strict && n < limit then error cs "redundant form"

    let big_tag ~off cs =
      let rec go acc = function
        | 8 -> error cs "big tag: too long"
        | i ->
            let b = String.get_uint8 cs (off + i) in
            let x = Int64.of_int (b land 0x7f) in
            match (Int64.(acc lsl 7 + x), b land 0x80) with
            | (0L,  _) -> error cs "big tag: leading 0"
            | (acc, 0) ->
              ( match Int64.to_nat_checked acc with
                | Some x -> (x, succ i)
                | None   -> error cs "big tag: overflow: %Li" acc)
            | (acc, _) -> go acc (succ i) in
      go 0L 0

    let big_len ~off cfg cs = function
      | 0 -> error cs "empty length"
      | n ->
        let rec f cs i = function
          | 0 -> 0L
          | n -> match String.get_uint8 cs (off + i) with
            | 0 when cfg.strict -> error cs "redundant length"
            | 0 -> f cs (i + 1) (n - 1)
            | _ when n > 8 -> error cs "length overflow"
            | x -> g (Int64.of_int x) cs (i + 1) (n - 1)
        and g acc cs i = function
          | 0 -> acc
          | n ->
            let v = String.get_uint8 cs (off + i) in
            let acc = Int64.(acc lsl 8 + of_int v) in
            g acc cs (i + 1) (n - 1)
        in
        match f cs 0 n |> Int64.to_nat_checked with
        | Some x -> x
        | None -> error cs "length overflow"

    let parse cfg cs off =
      let t0 = String.get_uint8 cs off in
      let tag_v, off_len =
        match t0 land 0x1f with
        | 0x1f ->
          let (n, i) = big_tag ~off:(off + 1) cs in
          ck_redundant cs cfg n 0x1f;
          n, i + 1
        | x -> x, 1
      in
      let l0 = String.get_uint8 cs (off + off_len) in
      let lbody = l0 land 0x7f in
      let len, off_end =
        if l0 <= 0x80 then
          lbody, off_len + 1
        else
          let n = big_len ~off:(off + off_len + 1) cfg cs lbody in
          ck_redundant cs cfg n 0x7f;
          n, off_len + 1 + lbody
      in
      let tag = match t0 land 0xc0 with
        | 0x00 -> Tag.Universal        tag_v
        | 0x40 -> Tag.Application      tag_v
        | 0x80 -> Tag.Context_specific tag_v
        | _    -> Tag.Private          tag_v
      and coding =
        (* according to layman's guide to a subset of ASN.1, BER, and DER, there
           are three possibilities in BER (DER restricts this further):
           - (a) primitive + definitive length
           - (b) constructed + definitive length
           - (c) constructed + indefinite length *)
        match t0 land 0x20, l0 with
        | 0, 0x80 -> error cs "primitive and indefinite length"
        | 0, _    -> Primitive len
        | _, 0x80 -> Constructed_indefinite
        | _       -> Constructed len
      in
      tag, off + off_end, coding
  end

  module Gen @ portable = struct
    let eof1 off cs = String.length cs - off = 0
    and eof2 off cs = String.get_uint16_be cs off = 0

    let split_off cs off n =
      let k = off + n in
      String.sub cs off n, k

    let rec children cfg eof acc cs off =
      if eof off cs then
        List.rev acc, off
      else
        let g, off' = node cfg cs off in
        children cfg eof (g::acc) cs off'

    and node cfg cs off =
      let (tag, off, coding) = Header.parse cfg cs off in
      match coding with
      | Primitive n ->
          let hd, off = split_off cs off n in
          G.Prim (tag, hd), off
      | Constructed n ->
          let hd, off = split_off cs off n in
          let gs, _ = children cfg eof1 [] hd 0 in
          G.Cons (tag, gs), off
      | Constructed_indefinite when cfg.strict ->
          parse_error "Constructed indefinite form"
      | Constructed_indefinite ->
          let gs, off = children cfg eof2 [] cs off in
          G.Cons (tag, gs), off + 2

    let parse cfg cs =
      try node cfg cs 0 with Invalid_argument msg ->
        parse_error "Unexpected EOF (msg %s): %a" msg pp_octets cs
  end


  let err_type ?(form=`Both) t g =
    parse_error "Type mismatch: expected: (%a %a) got: %a"
      G.pp_form_name form Tag.pp t G.pp_tag g

  let primitive t (f @ portable) : _ @ portable = function
    | G.Prim (t1, bs) when Tag.equal t t1 -> f bs
    | g -> err_type ~form:`Prim t g

  let constructed t (f @ portable) : _ @ portable = function
    | G.Cons (t1, gs) when Tag.equal t t1 -> f gs
    | g -> err_type ~form:`Cons t g

  let string_like c t (of_octets @ portable) (concat @ portable) : _ @ portable =
    let rec p = function
      | G.Prim (t1, bs) when Tag.equal t t1 -> of_octets bs
      | G.Cons (t1, gs) when Tag.equal t t1 && not c.strict ->
          concat (List.map p gs)
      | g -> err_type t g in
    p

  let c_prim : type a. config -> tag -> a prim -> (G.t -> a) @ portable =
    fun c tag -> function
    | Bool       -> primitive tag Prim.Boolean.of_octets
    | Int        -> primitive tag Prim.Integer.of_octets
    | Bits       -> string_like c tag Prim.Bits.of_octets Prim.Bits.concat
    | Octets     -> string_like c tag Prim.Octets.of_octets Prim.Octets.concat
    | Null       -> primitive tag Prim.Null.of_octets
    | OID        -> primitive tag Prim.OID.of_octets
    | CharString ->
        string_like c tag Prim.Gen_string.of_octets Prim.Gen_string.concat

  let peek asn : _ @ portable =
    match tag_set asn with
    | [tag] -> fun g -> Tag.equal (G.tag g) tag
    | tags  -> fun g ->
        let tag = G.tag g in List.exists (fun t -> Tag.equal t tag) tags

  type opt = config

  let rec c_asn : type a. a asn -> opt:opt -> (G.t -> a) @ portable =
    fun asn ~opt ->

    let rec go : type a. ?t:tag -> a asn -> (G.t -> a) @ portable =
      fun ?t -> function
      | Iso { project = f; syntax = a; _ } -> f &. go ?t a
      | Fix ({ unfold = fa } as body) ->
          fun g -> (go ?t (fa (Fix body))) g
      | Sequence s       -> constructed (t @? seq_tag) (c_seq s ~opt)
      | Sequence_of a    ->
          let p = c_asn a ~opt in
          constructed (t @? seq_tag) (fun values -> map_portable p values)
      | Set s            -> constructed (t @? set_tag) (c_set s ~opt)
      | Set_of a         ->
          let p = c_asn a ~opt in
          constructed (t @? set_tag) (fun values -> map_portable p values)
      | Implicit (t0, a) -> go ~t:(t @? t0) a
      | Explicit (t0, a) -> constructed (t @? t0) (c_explicit a ~opt)
      | Choice (a1, a2)  ->
          let (p1, p2) = (c_asn a1 ~opt, c_asn a2 ~opt)
          and accepts1 = peek a1 in
          fun g -> if accepts1 g then L (p1 g) else R (p2 g)
      | Prim p -> c_prim opt (t @? tag_of_p p) p in

    go asn

  and c_explicit : type a. a asn -> opt:opt -> (G.t list -> a) @ portable =
    fun a ~opt ->

    let p = c_asn a ~opt in function
      | [g] -> p g
      | gs  -> parse_error "EXPLICIT: sequence: %a" (pp_dump_list G.pp_tag) gs

  and c_seq : type a. a sequence -> opt:opt -> (G.t list -> a) @ portable =
    fun s ~opt ->

    let rec seq : type a. a sequence -> (G.t list -> a) @ portable = function
      | Pair (e, s) ->
          let (p1, p2) = (element e, c_seq s ~opt) in
          fun gs -> let (r, gs') = p1 gs in (r, p2 gs')
      | Last e ->
          let p = element e in fun gs ->
            match p gs with (a, []) -> a | (_, gs) ->
              parse_error "SEQUENCE: trailing: %a" (pp_dump_list G.pp_tag) gs

    and element : type a. a element ->
      (G.t list -> a * G.t list) @ portable = function
      | Required (lbl, a) ->
          let p = c_asn a ~opt in (function
            | g::gs -> (p g, gs)
            | []    -> parse_error "SEQUENCE: missing required: %s" (label lbl))
      | Optional (_, a) ->
          let (p, accepts) = (c_asn a ~opt, peek a) in
          function | g::gs when accepts g -> (Some (p g), gs)
                   | gs                   -> (None, gs)
    in seq s

  and c_set : type a. a sequence -> opt:opt -> (G.t list -> a) @ portable =
    fun s ~opt ->
    let take (accepts @ portable) values =
      let rec scan before = function
        | [] -> None
        | value :: after when accepts value ->
          Some (value, List.rev_append before after)
        | value :: after -> scan (value :: before) after
      in
      scan [] values
    in
    let rec sequence : type a. a sequence -> (G.t list -> a) @ portable =
      function
      | Last elt ->
        let parser = element elt in
        fun values ->
          let value, remaining = parser values in
          (match remaining with
          | [] -> value
          | values ->
            parse_error "SET: unexpected: %a" (pp_dump_list G.pp_tag) values)
      | Pair (head, tail) ->
        let parse_head = element head
        and parse_tail = sequence tail in
        fun values ->
          let head, remaining = parse_head values in
          head, parse_tail remaining
    and element : type a. a element ->
      (G.t list -> a * G.t list) @ portable = function
      | Required (lbl, syntax) ->
        let parser = c_asn syntax ~opt
        and accepts = peek syntax in
        fun values ->
          (match take accepts values with
          | Some (value, remaining) -> parser value, remaining
          | None -> parse_error "SET: missing required: %s" (label lbl))
      | Optional (_, syntax) ->
        let parser = c_asn syntax ~opt
        and accepts = peek syntax in
        fun values ->
          (match take accepts values with
          | Some (value, remaining) -> Some (parser value), remaining
          | None -> None, values)
    in
    sequence s

  let compile cfg asn =
    let p = c_asn asn ~opt:cfg in
    (fun cs ->
       let g, off = Gen.parse cfg cs in
       let remaining =
         if String.length cs - off = 0 then
           ""
         else
           String.sub cs off (String.length cs - off)
       in
       p g, remaining : _ @ portable)

  let compile_ber asn : _ @ portable = compile { strict = false } asn
  let compile_der asn : _ @ portable = compile { strict = true } asn

end

module W = struct

  let (<+>) = Writer.(<+>)

  let e_big_tag tag =
    let cons x = function [] -> [x] | xs -> (x lor 0x80)::xs in
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (cons (n land 0x7f) acc) (n lsr 7) in
    loop [] tag

  let e_big_length length =
    let rec loop acc = function
      | 0 -> acc
      | n -> loop (n land 0xff :: acc) (n lsr 8) in
    loop [] length

  let e_header tag mode len =

    let (klass, tagn) =
      let open Tag in
      match tag with
      | Universal n        -> (0x00, n)
      | Application n      -> (0x40, n)
      | Context_specific n -> (0x80, n)
      | Private n          -> (0xc0, n) in

    let constructed = match mode with
      | `Primitive   -> 0x00
      | `Constructed -> 0x20 in

    ( if tagn < 0x1f then
        Writer.of_byte (klass lor constructed lor tagn)
      else
        Writer.of_byte (klass lor constructed lor 0x1f) <+>
        Writer.of_list (e_big_tag tagn) )
    <+>
    ( if len <= 0x7f then
        Writer.of_byte len
      else
        let body = Writer.of_list (e_big_length len) in
        Writer.of_byte (0x80 lor Writer.len body) <+> body )


  type conf = { der : bool }

  let e_constructed tag body =
    e_header tag `Constructed (Writer.len body) <+> body

  let e_primitive tag body =
    e_header tag `Primitive (Writer.len body) <+> body

  let assert_length ?constr f a = match constr with
    | None   -> ()
    | Some n ->
        let n' = f a in
        if n <> n' then invalid_arg "Encode: length: expected %d, got %d" n n'

  let rec encode : type a. conf -> tag option -> a -> a asn -> Writer.t
  = fun conf tag a -> function

    | Iso { inject = g; syntax = asn; _ } -> encode conf tag (g a) asn

    | Fix ({ unfold = fa } as body) -> encode conf tag a (fa (Fix body))

    | Sequence asns ->
        e_constructed (tag @? seq_tag) (e_seq conf a asns)

    | Sequence_of asn -> (* size/stack? *)
        e_constructed (tag @? seq_tag) @@
          Writer.concat (List.map (fun e -> encode conf None e asn) a)

    | Set asns ->
        let h_sorted conf a asns =
          let fn = { Seq.f = fun a asn xs ->
            ( Asn_core.tag a asn, encode conf None a asn ) :: xs } in
          Writer.concat @@
            List.map snd @@
              List.sort (fun (t1, _) (t2, _) -> compare t1 t2) @@
                Seq.fold_with_value fn [] a asns
        in
        e_constructed (tag @? set_tag) @@
          if conf.der then h_sorted conf a asns else e_seq conf a asns

    | Set_of asn ->
        let ws = List.map (fun e -> encode conf None e asn) a in
        let body =
          Writer.concat @@
            if conf.der then
              List.( ws |> map  Writer.to_octets
                        |> sort Writer.lex_compare
                        |> map  Writer.of_octets )
            else ws
        in
        e_constructed (tag @? set_tag) body

    | Choice (asn1, asn2) ->
      ( match a with
        | L a' -> encode conf tag a' asn1
        | R b' -> encode conf tag b' asn2 )

    | Implicit (t, asn) ->
        encode conf (Some (tag @? t)) a asn

    | Explicit (t, asn) ->
        e_constructed (tag @? t) (encode conf None a asn)

    | Prim p -> e_prim tag a p

  and e_seq : type a. conf -> a -> a sequence -> Writer.t = fun conf ->
    let f = { Seq.f = fun e asn w -> encode conf None e asn <+> w } in
    Seq.fold_with_value f Writer.empty

  and e_prim : type a. tag option -> a -> a prim -> Writer.t = fun tag a prim ->
    let encode = e_primitive
      (match tag with Some x -> x | None -> tag_of_p prim) in
    let encode_s (type a) ?length a (module P : Prim.Prim_s with type t = a) =
      assert_length ?constr:length P.length a;
      encode (P.to_writer a) in
    match prim with
    | Bool       -> encode @@ Prim.Boolean.to_writer a
    | Int        -> encode @@ Prim.Integer.to_writer a
    | Bits       -> encode @@ Prim.Bits.to_writer a
    | Octets     -> encode_s a (module Prim.Octets)
    | Null       -> encode @@ Prim.Null.to_writer a
    | OID        -> encode @@ Prim.OID.to_writer a
    | CharString -> encode @@ Prim.Gen_string.to_writer a


  let ber_to_writer asn a = encode { der = false } None a asn

  let der_to_writer asn a = encode { der = true } None a asn

end
