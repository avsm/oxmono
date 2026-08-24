(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Ia = Stdlib_stable.Iarray
module I64u = Stdlib_upstream_compatible.Int64_u

type size = Fixed of int | Bounded of int | Unbounded
type repr = { dtype : Dtype.t; shape : int array }

type a2a = {
  name : string;
  encoded_repr : repr -> repr;
  encode : Slab.t -> Slab.t;
  decode : Slab.t -> repr -> Slab.t;
}

type a2b = {
  name : string;
  encoded_size : repr -> size;
  encode : Slab.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> repr -> Slab.t;
  partial_decode : (Byte_source.t -> repr -> Subset.t -> Slab.t) option;
}

type b2b = {
  name : string;
  encoded_size : size -> size;
  encode : Base_bigstring.t -> Base_bigstring.t;
  decode : Base_bigstring.t -> decoded_size:size -> Base_bigstring.t;
}

type bound = A2a of a2a | A2b of a2b | B2b of b2b

type resolver = Ext.t -> dtype:Dtype.t -> fill_value:Fill_value.t ->
  (bound, string) result option

type chain = { a2a : a2a list; a2b : a2b; b2b : b2b list; exts : Ext.t list }

(* {1 Shared helpers} *)

let err_str m = Error.raise_ (Error.Codec m)
let err fmt = Format.kasprintf err_str fmt

let product a =
  let n = Array.length a in
  let acc = ref 1 in
  for d = 0 to n - 1 do
    let dim = a.(d) in
    if dim < 0 then invalid_arg "Zarrz.Codec: negative dimension";
    if dim <> 0 && !acc > max_int / dim then
      invalid_arg "Zarrz.Codec: shape overflows an int";
    acc := !acc * dim
  done;
  !acc

let[@inline] u8 b off = Char.code (Base_bigstring.get b off)

let[@inline] set_u8 b off v =
  Base_bigstring.set b off (Char.unsafe_chr (v land 0xff))

let get_u32_le b off =
  let low = u8 b off lor (u8 b (off + 1) lsl 8) lor (u8 b (off + 2) lsl 16) in
  Int32.logor (Int32.of_int low)
    (Int32.shift_left (Int32.of_int (u8 b (off + 3))) 24)

let set_u32_le b off (v : int32) =
  for i = 0 to 3 do
    set_u8 b (off + i)
      (Int32.to_int (Int32.logand (Int32.shift_right_logical v (8 * i)) 0xffl))
  done

(* [copy_region] copies the [shape] block at [src_start] of the C-order
   array of extent [src_outer] into the block at [dst_start] of the
   C-order array of extent [dst_outer]. The trailing dimensions that both
   blocks span in full are coalesced, exactly as {!Subset.iter_runs} does
   for one array, so every iteration of the loop is a single memcpy and
   no element is ever touched individually. *)
let copy_region ~esize ~src ~src_outer ~src_start ~dst ~dst_outer ~dst_start
    ~shape =
  let r = Array.length shape in
  if r = 0 then
    Base_bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:esize
  else if product shape > 0 then begin
    let sstride = Array.make r 1 and dstride = Array.make r 1 in
    for d = r - 2 downto 0 do
      sstride.(d) <- sstride.(d + 1) * src_outer.(d + 1);
      dstride.(d) <- dstride.(d + 1) * dst_outer.(d + 1)
    done;
    let split = ref (r - 1) and len = ref shape.(r - 1) in
    while
      !split > 0
      && shape.(!split) = src_outer.(!split)
      && shape.(!split) = dst_outer.(!split)
    do
      decr split;
      len := !len * shape.(!split)
    done;
    let split = !split and len = !len in
    let soff = ref 0 and doff = ref 0 in
    for d = 0 to r - 1 do
      soff := !soff + (src_start.(d) * sstride.(d));
      doff := !doff + (dst_start.(d) * dstride.(d))
    done;
    let runs = ref 1 in
    for d = 0 to split - 1 do
      runs := !runs * shape.(d)
    done;
    let idx = Array.make (max split 1) 0 in
    for _ = 1 to !runs do
      Base_bigstring.blit ~src ~src_pos:(!soff * esize) ~dst
        ~dst_pos:(!doff * esize) ~len:(len * esize);
      let d = ref (split - 1) and carry = ref true in
      while !carry && !d >= 0 do
        let i = !d in
        idx.(i) <- idx.(i) + 1;
        soff := !soff + sstride.(i);
        doff := !doff + dstride.(i);
        if idx.(i) = shape.(i) then begin
          soff := !soff - (idx.(i) * sstride.(i));
          doff := !doff - (idx.(i) * dstride.(i));
          idx.(i) <- 0;
          decr d
        end
        else carry := false
      done
    done
  end

(* {1 Configuration decoding}

   A configuration member the codec does not know is rejected rather than
   ignored: an unrecognised member changes what the bytes mean often
   enough that guessing is worse than failing. *)

let check_members ~known mems =
  match
    List.find_opt (fun ((n, _), _) -> not (List.mem n known)) mems
  with
  | Some ((n, _), _) ->
      Error (Printf.sprintf "unknown configuration member %S" n)
  | None -> Ok ()

let mem_int mems name =
  match Jsont.Json.find_mem name mems with
  | None -> Ok None
  | Some (_, Jsont.Number (f, _))
    when Float.is_integer f && Float.abs f <= 4503599627370496.0 ->
      Ok (Some (int_of_float f))
  | Some _ -> Error (Printf.sprintf "%s must be an integer" name)

let mem_bool mems name =
  match Jsont.Json.find_mem name mems with
  | None -> Ok None
  | Some (_, Jsont.Bool (b, _)) -> Ok (Some b)
  | Some _ -> Error (Printf.sprintf "%s must be a boolean" name)

let mem_string mems name =
  match Jsont.Json.find_mem name mems with
  | None -> Ok None
  | Some (_, Jsont.String (s, _)) -> Ok (Some s)
  | Some _ -> Error (Printf.sprintf "%s must be a string" name)

let mem_ints mems name =
  match Jsont.Json.find_mem name mems with
  | None -> Ok None
  | Some (_, Jsont.Array (l, _)) ->
      let exception Bad in
      (try
         Ok
           (Some
              (Array.of_list
                 (List.map
                    (fun j ->
                      match j with
                      | Jsont.Number (f, _)
                        when Float.is_integer f
                             && Float.abs f <= 4503599627370496.0 ->
                          int_of_float f
                      | _ -> raise Bad)
                    l)))
       with Bad -> Error (Printf.sprintf "%s must hold integers" name))
  | Some _ -> Error (Printf.sprintf "%s must be an array" name)

let mem_exts mems name =
  match Jsont.Json.find_mem name mems with
  | None -> Ok None
  | Some (_, j) -> (
      match Jsont.Json.decode (Jsont.list Ext.jsont) j with
      | Ok l -> Ok (Some l)
      | Error e -> Error (Printf.sprintf "%s: %s" name e))

let ( let* ) = Result.bind

(* {1 The bytes codec} *)

module Bytes_codec = struct
  (* The width the declared endianness applies to. A complex value is a
     pair of reals and swaps each half on its own, and a raw type is an
     opaque byte string that never swaps. *)
  let swap_width (d : Dtype.t) =
    match d with
    | Complex64 -> 4
    | Complex128 -> 8
    | Raw _ | Bool | Int8 | Uint8 -> 1
    | d -> Dtype.size d

  let swap_into ~width ~src ~dst =
    let n = Base_bigstring.length src in
    let i = ref 0 in
    while !i < n do
      for k = 0 to width - 1 do
        Base_bigstring.set dst (!i + k)
          (Base_bigstring.get src (!i + width - 1 - k))
      done;
      i := !i + width
    done

  (* [endian] is [None] when the configuration left it out, which only a
     data type of a single byte per component may do. *)
  let swaps endian dt =
    let width = swap_width dt in
    if width = 1 then (1, false)
    else
      match endian with
      | None ->
          err "bytes: endian is required for data type %s" (Dtype.name dt)
      | Some big -> (width, big <> Sys.big_endian)

  let make ext ~dtype =
    let mems = Ext.config_mems ext in
    let* () = check_members ~known:[ "endian" ] mems in
    let* endian =
      match Jsont.Json.find_mem "endian" mems with
      | None -> Ok None
      | Some (_, Jsont.String ("little", _)) -> Ok (Some false)
      | Some (_, Jsont.String ("big", _)) -> Ok (Some true)
      | Some (_, Jsont.String (s, _)) ->
          Error (Printf.sprintf "endian %S is not \"little\" or \"big\"" s)
      | Some _ -> Error "endian must be a string"
    in
    if Option.is_none endian && swap_width dtype > 1 then
      Error
        (Printf.sprintf "endian is required for data type %s"
           (Dtype.name dtype))
    else
      let encoded_size r = Fixed (product r.shape * Dtype.size r.dtype) in
      (* The result aliases the slab's buffer whenever the declared
         endianness is the host's, which is what makes a whole chunk
         write a single pass. A caller must not mutate the slab
         afterwards and expect the encoded bytes to stand still. *)
      let encode slab =
        let buf = Slab.bigstring slab in
        let width, swap = swaps endian (Slab.dtype slab) in
        if not swap then buf
        else begin
          let dst = Base_bigstring.create (Base_bigstring.length buf) in
          swap_into ~width ~src:buf ~dst;
          dst
        end
      in
      (* Likewise the decoded slab is a view of the input buffer when no
         swap is needed, so the store's read lands in the slab with no
         copy at all. *)
      let decode buf r =
        let want = product r.shape * Dtype.size r.dtype in
        let got = Base_bigstring.length buf in
        if got <> want then
          err "bytes: %d encoded bytes for a %d byte chunk" got want;
        let width, swap = swaps endian r.dtype in
        let shape = Ia.of_array r.shape in
        if not swap then Slab.of_bigstring r.dtype shape buf
        else begin
          let dst = Base_bigstring.create want in
          swap_into ~width ~src:buf ~dst;
          Slab.of_bigstring r.dtype shape dst
        end
      in
      Ok
        (A2b { name = "bytes"; encoded_size; encode; decode;
               partial_decode = None })
end

(* {1 The transpose codec} *)

module Transpose = struct
  (* [order] is validated as a permutation of its own indices at
     construction, because the rank it must match is only known from the
     representation passed to each call. *)
  let permutation order =
    let n = Array.length order in
    let seen = Array.make n false in
    let ok = ref true in
    Array.iter
      (fun o ->
        if o < 0 || o >= n || seen.(o) then ok := false else seen.(o) <- true)
      order;
    !ok

  let inverse order =
    let n = Array.length order in
    let inv = Array.make n 0 in
    Array.iteri (fun i o -> inv.(o) <- i) order;
    inv

  (* [permute ~esize ~src ~src_shape ~order ~dst] writes into [dst] the
     array whose dimension [i] is dimension [order.(i)] of [src]. The
     trailing dimensions the permutation leaves in place are coalesced
     into one memcpy, so an identity order is a single copy and a
     reversal degrades to one memcpy per element. *)
  let permute ~esize ~src ~src_shape ~order ~dst =
    let n = Array.length order in
    let dst_shape = Array.init n (fun i -> src_shape.(order.(i))) in
    if n = 0 then
      Base_bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:esize
    else if product dst_shape > 0 then begin
      let src_stride = Array.make n 1 in
      for d = n - 2 downto 0 do
        src_stride.(d) <- src_stride.(d + 1) * src_shape.(d + 1)
      done;
      let stride = Array.init n (fun i -> src_stride.(order.(i))) in
      let k = ref n in
      while !k > 0 && order.(!k - 1) = !k - 1 do
        decr k
      done;
      let k = !k in
      let run = ref 1 in
      for d = k to n - 1 do
        run := !run * dst_shape.(d)
      done;
      let run = !run in
      let runs = ref 1 in
      for d = 0 to k - 1 do
        runs := !runs * dst_shape.(d)
      done;
      let idx = Array.make (max k 1) 0 in
      let soff = ref 0 and doff = ref 0 in
      for _ = 1 to !runs do
        Base_bigstring.blit ~src ~src_pos:(!soff * esize) ~dst
          ~dst_pos:(!doff * esize) ~len:(run * esize);
        doff := !doff + run;
        let d = ref (k - 1) and carry = ref true in
        while !carry && !d >= 0 do
          let i = !d in
          idx.(i) <- idx.(i) + 1;
          soff := !soff + stride.(i);
          if idx.(i) = dst_shape.(i) then begin
            soff := !soff - (idx.(i) * stride.(i));
            idx.(i) <- 0;
            decr d
          end
          else carry := false
        done
      done
    end

  let check_rank order shape =
    let n = Array.length order in
    if Array.length shape <> n then
      err "transpose: order has %d entries for a rank %d array" n
        (Array.length shape)

  (* [apply ~order slab] is [slab] with dimension [i] taken from
     dimension [order.(i)], which is the direction the specification
     gives for encoding. *)
  let apply ~order slab =
    let esize = Dtype.size (Slab.dtype slab) in
    let src_shape = Ia.to_array (Slab.shape slab) in
    check_rank order src_shape;
    let dst_shape =
      Array.init (Array.length order) (fun i -> src_shape.(order.(i)))
    in
    let dst = Slab.create (Slab.dtype slab) (Ia.of_array dst_shape) in
    permute ~esize ~src:(Slab.bigstring slab) ~src_shape ~order
      ~dst:(Slab.bigstring dst);
    dst

  let make ext =
    let mems = Ext.config_mems ext in
    let* () = check_members ~known:[ "order" ] mems in
    let* order = mem_ints mems "order" in
    match order with
    | None -> Error "order is required"
    | Some order when Array.length order = 0 -> Error "order is empty"
    | Some order when not (permutation order) ->
        Error "order is not a permutation of its own indices"
    | Some order ->
        let inv = inverse order in
        let encoded_repr r =
          check_rank order r.shape;
          { r with
            shape =
              Array.init (Array.length order) (fun i -> r.shape.(order.(i)))
          }
        in
        let encode slab = apply ~order slab in
        let decode slab r =
          let out = apply ~order:inv slab in
          if Ia.to_array (Slab.shape out) <> r.shape then
            err "transpose: the inverse order does not restore the chunk shape";
          out
        in
        Ok (A2a { name = "transpose"; encoded_repr; encode; decode })
end

(* {1 The gzip codec} *)

module Gzip = struct
  (* Decoding is the one shot [De.Inf.Ns.inflate] under RFC 1952 framing
     parsed here, so a chunk is inflated straight into the slab's buffer
     with no intermediate window and no second copy. Encoding cannot use
     the matching [De.Def.Ns.deflate]: its uncompressed block writer runs
     past the end of the destination on input it fails to compress, so
     the streaming [Gz.Def] encoder does that side. *)

  let header_size = 10
  let trailer_size = 8

  let crc32 b off len =
    Checkseum.Crc32.(to_int32 (digest_bigstring b off len default))

  let compress ~level src =
    let n = Base_bigstring.length src in
    let w = De.Lz77.make_window ~bits:15 in
    let q = De.Queue.create 0x1000 in
    let i = De.bigstring_create De.io_buffer_size in
    let o = De.bigstring_create De.io_buffer_size in
    let out = ref (Base_bigstring.create (max 64 (n + (n / 8) + 64))) in
    let olen = ref 0 and pos = ref 0 in
    let refill b =
      let len = min (n - !pos) (De.bigstring_length b) in
      Base_bigstring.blit ~src ~src_pos:!pos ~dst:b ~dst_pos:0 ~len;
      pos := !pos + len;
      len
    in
    let flush b len =
      if !olen + len > Base_bigstring.length !out then begin
        let cap = max (2 * Base_bigstring.length !out) (!olen + len) in
        let bigger = Base_bigstring.create cap in
        Base_bigstring.blit ~src:!out ~src_pos:0 ~dst:bigger ~dst_pos:0
          ~len:!olen;
        out := bigger
      end;
      Base_bigstring.blit ~src:b ~src_pos:0 ~dst:!out ~dst_pos:!olen ~len;
      olen := !olen + len
    in
    (* A zero modification time and an unknown operating system keep the
       encoding a function of the input alone. *)
    let config = Gz.Higher.configuration Gz.Unknown (fun () -> 0l) in
    Gz.Higher.compress ~level ~w ~q ~refill ~flush () config i o;
    if !olen = Base_bigstring.length !out then !out
    else Base_bigstring.sub !out ~pos:0 ~len:!olen

  (* [body_off src] is the offset of the deflate stream, past the fixed
     header and whatever optional fields the flags announce. *)
  let body_off src =
    let n = Base_bigstring.length src in
    if n < header_size + trailer_size then err "gzip: truncated stream";
    if u8 src 0 <> 0x1f || u8 src 1 <> 0x8b then err "gzip: bad magic";
    if u8 src 2 <> 8 then err "gzip: unsupported compression method";
    let flg = u8 src 3 in
    if flg land 0xe0 <> 0 then err "gzip: reserved flag bits are set";
    let limit = n - trailer_size in
    let off = ref header_size in
    let need k = if !off + k > limit then err "gzip: truncated header" in
    if flg land 0x04 <> 0 then begin
      need 2;
      let xlen = u8 src !off lor (u8 src (!off + 1) lsl 8) in
      off := !off + 2;
      need xlen;
      off := !off + xlen
    end;
    let skip_string () =
      let stop = ref false in
      while not !stop do
        need 1;
        let c = u8 src !off in
        incr off;
        if c = 0 then stop := true
      done
    in
    if flg land 0x08 <> 0 then skip_string ();
    if flg land 0x10 <> 0 then skip_string ();
    if flg land 0x02 <> 0 then begin
      need 2;
      off := !off + 2
    end;
    !off

  let decompress ~decoded_size src =
    let n = Base_bigstring.length src in
    let off = body_off src in
    let isize = Int32.to_int (get_u32_le src (n - 4)) land 0xffffffff in
    let want =
      match decoded_size with
      | Fixed m -> m
      | Bounded m ->
          if isize > m then
            err "gzip: the stream declares %d bytes, at most %d allowed" isize
              m;
          isize
      | Unbounded -> isize
    in
    let dst = Base_bigstring.create want in
    (if want > 0 then
       let body = Bigarray.Array1.sub src off (n - trailer_size - off) in
       match De.Inf.Ns.inflate body dst with
       | Error e -> err "gzip: %a" De.Inf.Ns.pp_error e
       | Ok (_, w) ->
           if w <> want then
             err "gzip: inflated %d bytes where %d were expected" w want);
    if want land 0xffffffff <> isize then
      err "gzip: the stream declares %d bytes, %d were inflated" isize want;
    let expected = get_u32_le src (n - 8) in
    let got = crc32 dst 0 want in
    if not (Int32.equal expected got) then
      Error.raise_ (Error.Checksum_mismatch { expected; got });
    dst

  let make ext =
    let mems = Ext.config_mems ext in
    let* () = check_members ~known:[ "level" ] mems in
    let* level = mem_int mems "level" in
    (* The oracle makes [level] mandatory, with no default anywhere in
       its metadata layer, so a configuration without it is refused. *)
    match level with
    | None -> Error "level is required"
    | Some level when level < 0 || level > 9 ->
        Error (Printf.sprintf "level %d is outside [0, 9]" level)
    | Some level ->
      (* The oracle's bound, which is zlib's [deflateBound] plus the
         eighteen bytes of framing. *)
      let ceil_div a b = (a + b - 1) / b in
      let encoded_size = function
        | Fixed n | Bounded n ->
            Bounded (n + 18 + ceil_div n 8 + ceil_div n 64 + 5)
        | Unbounded -> Unbounded
      in
      Ok
        (B2b
           {
             name = "gzip";
             encoded_size;
             encode = compress ~level;
             decode = (fun src ~decoded_size -> decompress ~decoded_size src);
           })
end

(* {1 The zstd codec} *)

module Zstd = struct
  (* libzstd clamps a level to the range it supports, but the oracle
     refuses one outside this range outright and so does this. *)
  let min_level = -131072
  let max_level = 22

  let of_error f = try f () with Zstdz.Error (_, name) -> err "zstd: %s" name

  let make ext =
    let mems = Ext.config_mems ext in
    let* () = check_members ~known:[ "level"; "checksum" ] mems in
    let* level = mem_int mems "level" in
    let* checksum = mem_bool mems "checksum" in
    (* [level] is mandatory, as in the oracle. [checksum] is defaulted to
       [false] rather than demanded, which is what the oracle does when
       it lifts a numcodecs configuration, and accepts the shorter form
       some writers emit. *)
    let* level =
      match level with
      | None -> Error "level is required"
      | Some l when l < min_level || l > max_level ->
          Error
            (Printf.sprintf "level %d is outside [%d, %d]" l min_level
               max_level)
      | Some l -> Ok l
    in
    let checksum = Option.value checksum ~default:false in
    (* One context per bound codec. libzstd contexts are not thread safe
       and carry no lock, so a chain must not be shared across domains.
       Eio fibers of one domain are safe: a fiber cannot switch inside a
       C call. The contexts are forced on first use so that binding a
       chain that is never run allocates nothing in libzstd. *)
    let cctx = lazy (Zstdz.create_cctx ()) in
    let dctx = lazy (Zstdz.create_dctx ()) in
    let encode src =
      let src_len = Base_bigstring.length src in
      let bound = Zstdz.compress_bound src_len in
      let dst = Base_bigstring.create bound in
      let w =
        of_error (fun () ->
            Zstdz.compress ~level ~checksum (Lazy.force cctx) ~src ~src_off:0
              ~src_len ~dst ~dst_off:0 ~dst_len:bound)
      in
      if w = bound then dst else Base_bigstring.sub dst ~pos:0 ~len:w
    in
    let decode src ~decoded_size =
      let src_len = Base_bigstring.length src in
      let declared =
        I64u.to_int64 (Zstdz.content_size src ~off:0 ~len:src_len)
      in
      if Int64.equal declared (-2L) then err "zstd: unreadable frame header";
      let known =
        if Int64.compare declared 0L < 0 then None
        else if Int64.compare declared (Int64.of_int max_int) > 0 then
          err "zstd: the frame declares more bytes than an int can hold"
        else Some (Int64.to_int declared)
      in
      let want =
        match (decoded_size, known) with
        | Fixed m, Some d when d <> m ->
            err "zstd: the frame declares %d bytes where %d were expected" d m
        | Fixed m, _ -> m
        | Bounded m, Some d when d > m ->
            err "zstd: the frame declares %d bytes, at most %d allowed" d m
        | Bounded _, Some d -> d
        | Bounded m, None -> m
        | Unbounded, Some d -> d
        | Unbounded, None -> err "zstd: the frame does not declare its size"
      in
      let dst = Base_bigstring.create want in
      let w =
        of_error (fun () ->
            Zstdz.decompress (Lazy.force dctx) ~src ~src_off:0 ~src_len ~dst
              ~dst_off:0 ~dst_len:want)
      in
      match decoded_size with
      | Fixed m ->
          if w <> m then
            err "zstd: decompressed %d bytes where %d were expected" w m;
          dst
      | _ -> if w = want then dst else Base_bigstring.sub dst ~pos:0 ~len:w
    in
    let encoded_size = function
      | Fixed n | Bounded n -> Bounded (Zstdz.compress_bound n)
      | Unbounded -> Unbounded
    in
    Ok (B2b { name = "zstd"; encoded_size; encode; decode })
end

(* {1 The blosc codec}

   Blosc is a container rather than a compressor: it splits the input
   into blocks, optionally shuffles the bytes or bits of each block so
   that the like-numbered bytes of an element sit together, and hands
   each block to an inner compressor named by [cname]. The frame records
   the decompressed size, the type size and the shuffle applied, so
   decoding needs none of the configuration and this codec reads a frame
   whatever parameters wrote it. *)

module Blosc = struct
  (* The compressors the Zarr configuration may name, which is the
     oracle's [BloscCompressor] enum. A build of the C library may lack
     some of them, so a name in this list is still checked against
     {!Bloscz.compressors} before the codec is bound. *)
  let cnames = [ "blosclz"; "lz4"; "lz4hc"; "snappy"; "zlib"; "zstd" ]

  let of_error f = try f () with Bloscz.Error (_, m) -> err "blosc: %s" m

  let make ext ~dtype =
    let mems = Ext.config_mems ext in
    let* () =
      check_members
        ~known:[ "cname"; "clevel"; "shuffle"; "typesize"; "blocksize" ] mems
    in
    let* cname = mem_string mems "cname" in
    let* clevel = mem_int mems "clevel" in
    let* shuffle = mem_string mems "shuffle" in
    let* typesize = mem_int mems "typesize" in
    let* blocksize = mem_int mems "blocksize" in
    let* cname =
      match cname with
      | None -> Error "cname is required"
      | Some c when not (List.mem c cnames) ->
          Error
            (Printf.sprintf "cname %S is not one of %s" c
               (String.concat ", " cnames))
      | Some c when not (List.mem c (Bloscz.compressors ())) ->
          Error
            (Printf.sprintf
               "cname %S is not in this build of blosc, which has %s" c
               (String.concat ", " (Bloscz.compressors ())))
      | Some c -> Ok c
    in
    (* [clevel] is mandatory, as in the oracle, which has no default for
       it anywhere in its metadata layer. *)
    let* clevel =
      match clevel with
      | None -> Error "clevel is required"
      | Some l when l < 0 || l > 9 ->
          Error (Printf.sprintf "clevel %d is outside [0, 9]" l)
      | Some l -> Ok l
    in
    let* shuffle =
      match shuffle with
      | None | Some "noshuffle" -> Ok `No
      | Some "shuffle" -> Ok `Byte
      | Some "bitshuffle" -> Ok `Bit
      | Some s ->
          Error
            (Printf.sprintf
               "shuffle %S is not \"noshuffle\", \"shuffle\" or \"bitshuffle\""
               s)
    in
    (* The type size the shuffle filter permutes around. The oracle
       demands it in the configuration whenever a shuffle is asked for
       and this defaults it to the data type size instead, which is what
       the oracle itself substitutes when it lifts a Zarr V2
       configuration. Under [`No] the value reaches the frame header and
       nothing reads it back, so only the encoded bytes depend on the
       choice. *)
    let* typesize =
      match typesize with
      | None -> Ok (Dtype.size dtype)
      | Some t when t < 1 ->
          Error (Printf.sprintf "typesize %d is below 1" t)
      | Some t -> Ok t
    in
    let* blocksize =
      match blocksize with
      | None -> Ok 0
      | Some b when b < 0 ->
          Error (Printf.sprintf "blocksize %d is negative" b)
      | Some b -> Ok b
    in
    let encode src =
      let src_len = Base_bigstring.length src in
      let dst_len = src_len + Bloscz.max_overhead in
      let dst = Base_bigstring.create dst_len in
      let n =
        of_error (fun () ->
            Bloscz.compress cname ~level:clevel ~shuffle ~blocksize ~typesize
              ~src ~src_off:0 ~src_len ~dst ~dst_off:0 ~dst_len)
      in
      if n = dst_len then dst else Base_bigstring.sub dst ~pos:0 ~len:n
    in
    (* The frame length comes from the header rather than from the
       buffer, because a chain that ends in blosc may be handed trailing
       bytes, and [Bloscz.validate] insists on the exact length. The
       validation is what makes decoding a stranger's bytes safe: it
       checks the block offsets against the frame before the decompressor
       follows them. *)
    let decode src ~decoded_size =
      let src_len = Base_bigstring.length src in
      if src_len < Bloscz.max_overhead then
        err "blosc: %d bytes cannot hold a frame header" src_len;
      let ~nbytes:_, ~cbytes, ~blocksize:_ =
        Bloscz.buffer_sizes src ~off:0 ~len:src_len
      in
      if cbytes < Bloscz.max_overhead || cbytes > src_len then
        err "blosc: the frame declares %d bytes, %d are present" cbytes
          src_len;
      let nbytes =
        match Bloscz.validate src ~off:0 ~len:cbytes with
        | Some n -> n
        | None -> err "blosc: the frame is malformed"
      in
      let want =
        match decoded_size with
        | Fixed m when nbytes <> m ->
            err "blosc: the frame declares %d bytes where %d were expected"
              nbytes m
        | Fixed m -> m
        | Bounded m when nbytes > m ->
            err "blosc: the frame declares %d bytes, at most %d allowed"
              nbytes m
        | Bounded _ | Unbounded -> nbytes
      in
      let dst = Base_bigstring.create want in
      let w =
        of_error (fun () ->
            Bloscz.decompress ~src ~src_off:0 ~src_len:cbytes ~dst ~dst_off:0
              ~dst_len:want)
      in
      if w <> want then
        err "blosc: decompressed %d bytes where %d were expected" w want;
      dst
    in
    let encoded_size = function
      | Fixed n | Bounded n -> Bounded (n + Bloscz.max_overhead)
      | Unbounded -> Unbounded
    in
    Ok (B2b { name = "blosc"; encoded_size; encode; decode })
end

(* {1 The crc32c codec} *)

module Crc32c = struct
  let crc b off len =
    Checkseum.Crc32c.(to_int32 (digest_bigstring b off len default))

  let make ext =
    let* () = check_members ~known:[] (Ext.config_mems ext) in
    let encoded_size = function
      | Fixed n -> Fixed (n + 4)
      | Bounded n -> Bounded (n + 4)
      | Unbounded -> Unbounded
    in
    let encode src =
      let n = Base_bigstring.length src in
      let dst = Base_bigstring.create (n + 4) in
      Base_bigstring.blit ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:n;
      set_u32_le dst n (crc src 0 n);
      dst
    in
    (* The verified payload is a view of the input, so a chain that ends
       in crc32c still hands the store's buffer to the codec below it. *)
    let decode src ~decoded_size:_ =
      let n = Base_bigstring.length src in
      if n < 4 then err "crc32c: %d bytes cannot hold a checksum" n;
      let payload = Bigarray.Array1.sub src 0 (n - 4) in
      let expected = get_u32_le src (n - 4) in
      let got = crc payload 0 (n - 4) in
      if Int32.equal expected got then payload
      else Error.raise_ (Error.Checksum_mismatch { expected; got })
    in
    Ok (B2b { name = "crc32c"; encoded_size; encode; decode })
end

(* {1 Sharding}

   The shard index is a [uint64] array of shape [chunks_per_shard @@ [2]]
   holding, for the C-order linear index [i] of an inner chunk, its byte
   offset in the shard at [2 * i] and its byte length at [2 * i + 1]. An
   entry of all ones in both halves means the inner chunk was never
   written and reads as the fill value. *)

let shard_absent = -1L
let shard_sentinel = String.make 8 '\xff'

let shard_default_codecs =
  [
    Ext.v "bytes"
      ~config:
        (Jsont.Json.object'
           [ (Jsont.Json.name "endian", Jsont.Json.string "little") ]);
  ]

let shard_default_index_codecs = shard_default_codecs @ [ Ext.v "crc32c" ]

(* [shard_grid ~chunk_shape shape] is the number of inner chunks along
   each dimension. The inner shape must divide the shard shape exactly:
   the sharding codec has no notion of a partial inner chunk. *)
let shard_grid ~chunk_shape shape =
  let r = Array.length chunk_shape in
  if Array.length shape <> r then
    err "sharding_indexed: chunk_shape has rank %d for a rank %d shard" r
      (Array.length shape);
  Array.init r (fun d ->
      let cs = chunk_shape.(d) and s = shape.(d) in
      if cs <= 0 then err "sharding_indexed: chunk_shape must be positive";
      if s mod cs <> 0 then
        err "sharding_indexed: dimension %d of the shard is %d, which %d does \
             not divide"
          d s cs;
      s / cs)

let shard_index_repr grid =
  let r = Array.length grid in
  { dtype = Dtype.Uint64;
    shape = Array.init (r + 1) (fun i -> if i < r then grid.(i) else 2) }

(* [shard_entry slab i] is the offset and length of inner chunk [i], or
   [None] when the entry is the absent sentinel. *)
let shard_entry slab i =
  let off = I64u.to_int64 (Slab.U64.get slab (2 * i)) in
  let len = I64u.to_int64 (Slab.U64.get slab ((2 * i) + 1)) in
  if Int64.equal off shard_absent && Int64.equal len shard_absent then None
  else
    let to_int what v =
      if Int64.compare v 0L < 0 || Int64.compare v (Int64.of_int max_int) > 0
      then err "sharding_indexed: inner chunk %d has an unusable %s" i what
      else Int64.to_int v
    in
    Some (to_int "offset" off, to_int "length" len)

let shard_set_entry slab i v =
  Slab.U64.set slab i (I64u.of_int64 v)

(* {1 The chain} *)

(* The built-in codecs. [sharding_indexed] binds its inner and index
   chains through {!chain_of_exts}, which is why this is one recursive
   group with the chain operations. Known limitation: a nested chain sees
   only the built-ins, because a resolver is not threaded through the
   codec metadata. A user codec inside a shard is a follow-up. *)
let rec builtins : resolver =
 fun ext ~dtype ~fill_value ->
  match ext.Ext.name with
  | "bytes" -> Some (Bytes_codec.make ext ~dtype)
  | "transpose" -> Some (Transpose.make ext)
  | "gzip" -> Some (Gzip.make ext)
  | "zstd" -> Some (Zstd.make ext)
  | "blosc" -> Some (Blosc.make ext ~dtype)
  | "crc32c" -> Some (Crc32c.make ext)
  | "sharding_indexed" -> Some (sharding ext ~dtype ~fill_value)
  | _ -> None

and sharding ext ~dtype ~fill_value =
  let mems = Ext.config_mems ext in
  let* () =
    check_members
      ~known:[ "chunk_shape"; "codecs"; "index_codecs"; "index_location" ]
      mems
  in
  let* chunk_shape = mem_ints mems "chunk_shape" in
  let* chunk_shape =
    match chunk_shape with
    | None -> Error "chunk_shape is required"
    | Some cs when Array.exists (fun d -> d <= 0) cs ->
        Error "chunk_shape must hold positive integers"
    | Some cs -> Ok cs
  in
  let* inner_exts = mem_exts mems "codecs" in
  let inner_exts = Option.value inner_exts ~default:shard_default_codecs in
  let* index_exts = mem_exts mems "index_codecs" in
  let index_exts =
    Option.value index_exts ~default:shard_default_index_codecs
  in
  let* location = mem_string mems "index_location" in
  let* at_start =
    match location with
    | None | Some "end" -> Ok false
    | Some "start" -> Ok true
    | Some s ->
        Error (Printf.sprintf "index_location %S is not \"start\" or \"end\"" s)
  in
  let* inner = chain_of_exts ~dtype ~fill_value inner_exts in
  let* index =
    chain_of_exts ~dtype:Dtype.Uint64
      ~fill_value:(Fill_value.of_bytes shard_sentinel)
      index_exts
  in
  let inner_repr dt = { dtype = dt; shape = chunk_shape } in
  let index_size grid =
    match encoded_size index (shard_index_repr grid) with
    | Fixed n -> n
    | Bounded _ | Unbounded ->
        err "sharding_indexed: index_codecs must have a fixed encoded size"
  in
  let read_index src grid =
    let n = index_size grid in
    let range =
      if at_start then Byte_range.From_start { off = 0; len = Some n }
      else Byte_range.Suffix n
    in
    let bytes = src.Byte_source.read range in
    if Base_bigstring.length bytes <> n then
      err "sharding_indexed: the index read returned %d of %d bytes"
        (Base_bigstring.length bytes) n;
    decode_chunk index (shard_index_repr grid) bytes
  in
  let partial src r sub =
    let grid = shard_grid ~chunk_shape r.shape in
    let outer = Ia.of_array r.shape in
    Subset.validate ~outer sub;
    let rank = Array.length r.shape in
    let esize = Dtype.size r.dtype in
    let start = Ia.to_array sub.Subset.start in
    let extent = Ia.to_array sub.Subset.shape in
    let out = Slab.create r.dtype sub.Subset.shape in
    let idx = read_index src grid in
    (* The inner chunks the subset touches, in C order. *)
    let lo = Array.init rank (fun d -> start.(d) / chunk_shape.(d)) in
    let hi =
      Array.init rank (fun d ->
          let e = start.(d) + extent.(d) in
          if e <= start.(d) then lo.(d)
          else ((e + chunk_shape.(d) - 1) / chunk_shape.(d)))
    in
    let touched = ref [] in
    let rec walk d coord =
      if d = rank then begin
        let linear = ref 0 in
        for k = 0 to rank - 1 do
          linear := (!linear * grid.(k)) + coord.(k)
        done;
        touched := (Array.copy coord, !linear) :: !touched
      end
      else
        for i = lo.(d) to hi.(d) - 1 do
          coord.(d) <- i;
          walk (d + 1) coord
        done
    in
    if product extent > 0 then walk 0 (Array.make (max rank 1) 0);
    let touched = List.rev !touched in
    let entries =
      List.map (fun (coord, linear) -> (coord, shard_entry idx linear)) touched
    in
    (* An absent inner chunk reads as fill value. Painting the whole
       output first costs one pass and keeps the assembly loop free of a
       second region walker. *)
    if List.exists (fun (_, e) -> Option.is_none e) entries then
      Slab.fill out (Fill_value.to_bytes fill_value);
    let present =
      List.filter_map
        (fun (coord, e) ->
          match e with Some (off, len) -> Some (coord, off, len) | None -> None)
        entries
    in
    let ranges =
      List.map
        (fun (_, off, len) -> Byte_range.From_start { off; len = Some len })
        present
    in
    (* One batched read for every inner chunk the subset needs. *)
    let bufs =
      match ranges with [] -> [] | _ -> src.Byte_source.read_many ranges
    in
    if List.length bufs <> List.length ranges then
      err "sharding_indexed: %d buffers for %d requested ranges"
        (List.length bufs) (List.length ranges);
    let irepr = inner_repr r.dtype in
    List.iter2
      (fun (coord, _, len) buf ->
        if Base_bigstring.length buf <> len then
          err "sharding_indexed: an inner chunk read returned %d of %d bytes"
            (Base_bigstring.length buf) len;
        let chunk = decode_chunk inner irepr buf in
        let origin = Array.init rank (fun d -> coord.(d) * chunk_shape.(d)) in
        let shape =
          Array.init rank (fun d ->
              let a = max start.(d) origin.(d) in
              let b =
                min (start.(d) + extent.(d)) (origin.(d) + chunk_shape.(d))
              in
              b - a)
        in
        let src_start =
          Array.init rank (fun d -> max start.(d) origin.(d) - origin.(d))
        in
        let dst_start =
          Array.init rank (fun d -> max start.(d) origin.(d) - start.(d))
        in
        copy_region ~esize ~src:(Slab.bigstring chunk) ~src_outer:chunk_shape
          ~src_start ~dst:(Slab.bigstring out) ~dst_outer:extent ~dst_start
          ~shape)
      present bufs;
    out
  in
  let decode buf r =
    let rank = Array.length r.shape in
    partial
      (Byte_source.of_bigstring buf)
      r
      {
        Subset.start = Ia.of_array (Array.make rank 0);
        shape = Ia.of_array r.shape;
      }
  in
  let encode slab =
    let r =
      { dtype = Slab.dtype slab; shape = Ia.to_array (Slab.shape slab) }
    in
    let grid = shard_grid ~chunk_shape r.shape in
    let rank = Array.length r.shape in
    let n = product grid in
    let m = index_size grid in
    let esize = Dtype.size r.dtype in
    let cshape = Ia.of_array chunk_shape in
    let outer = Ia.of_array r.shape in
    (* An inner chunk equal to the fill value everywhere is omitted, so
       the reference image is built once and compared with memcmp. *)
    let blank = Slab.create r.dtype cshape in
    Slab.fill blank (Fill_value.to_bytes fill_value);
    let cbytes = product chunk_shape * esize in
    let payloads = Array.make (max n 1) None in
    let coord = Array.make (max rank 1) 0 in
    let linear = ref 0 in
    let rec walk d =
      if d = rank then begin
        let chunk = Slab.create r.dtype cshape in
        let start =
          Ia.of_array (Array.init rank (fun k -> coord.(k) * chunk_shape.(k)))
        in
        Subset.gather ~elem_size:esize ~src:(Slab.bigstring slab) ~outer
          { Subset.start; shape = cshape } ~dst:(Slab.bigstring chunk);
        let blank_here =
          cbytes = 0
          || Base_bigstring.memcmp (Slab.bigstring chunk) ~pos1:0
               (Slab.bigstring blank) ~pos2:0 ~len:cbytes
             = 0
        in
        if not blank_here then
          payloads.(!linear) <- Some (encode_chunk inner chunk);
        incr linear
      end
      else
        for i = 0 to grid.(d) - 1 do
          coord.(d) <- i;
          walk (d + 1)
        done
    in
    if n > 0 then walk 0;
    let payload_bytes =
      Array.fold_left
        (fun acc p ->
          match p with Some b -> acc + Base_bigstring.length b | None -> acc)
        0 payloads
    in
    let total = m + payload_bytes in
    let buf = Base_bigstring.create total in
    let ishape = Ia.of_array (shard_index_repr grid).shape in
    let idx = Slab.create Dtype.Uint64 ishape in
    (* An inner chunk offset is measured from the start of the whole
       shard, so it starts past the index when the index leads. *)
    let pos = ref (if at_start then m else 0) in
    for i = 0 to n - 1 do
      match payloads.(i) with
      | None ->
          shard_set_entry idx (2 * i) shard_absent;
          shard_set_entry idx ((2 * i) + 1) shard_absent
      | Some b ->
          let len = Base_bigstring.length b in
          Base_bigstring.blit ~src:b ~src_pos:0 ~dst:buf ~dst_pos:!pos ~len;
          shard_set_entry idx (2 * i) (Int64.of_int !pos);
          shard_set_entry idx ((2 * i) + 1) (Int64.of_int len);
          pos := !pos + len
    done;
    let ibytes = encode_chunk index idx in
    if Base_bigstring.length ibytes <> m then
      err "sharding_indexed: the index encoded to %d bytes, %d were reserved"
        (Base_bigstring.length ibytes) m;
    Base_bigstring.blit ~src:ibytes ~src_pos:0 ~dst:buf
      ~dst_pos:(if at_start then 0 else total - m) ~len:m;
    buf
  in
  let shard_size r =
    let grid = shard_grid ~chunk_shape r.shape in
    let m = index_size grid in
    let n = product grid in
    match encoded_size inner (inner_repr r.dtype) with
    | Fixed f | Bounded f -> Bounded (m + (n * f))
    | Unbounded -> Unbounded
  in
  Ok
    (A2b
       {
         name = "sharding_indexed";
         encoded_size = shard_size;
         encode;
         decode;
         partial_decode = Some partial;
       })

and chain_of_exts ?resolver ~dtype ~fill_value exts =
  let resolve ext =
    let user =
      match resolver with
      | Some r -> r ext ~dtype ~fill_value
      | None -> None
    in
    match user with Some _ -> user | None -> builtins ext ~dtype ~fill_value
  in
  let rec go a2a a2b b2b kept = function
    | [] -> (
        match a2b with
        | None -> Error "missing array to bytes codec"
        | Some a2b ->
            Ok
              {
                a2a = List.rev a2a;
                a2b;
                b2b = List.rev b2b;
                exts = List.rev kept;
              })
    | (ext : Ext.t) :: tl -> (
        match resolve ext with
        | None ->
            if ext.must_understand then
              Error (Printf.sprintf "unknown codec %S" ext.name)
            else go a2a a2b b2b kept tl
        | Some (Error e) -> Error (Printf.sprintf "codec %S: %s" ext.name e)
        | Some (Ok (A2a c)) -> go (c :: a2a) a2b b2b (ext :: kept) tl
        | Some (Ok (A2b c)) -> (
            match a2b with
            | Some _ -> Error "multiple array to bytes codecs"
            | None -> go a2a (Some c) b2b (ext :: kept) tl)
        | Some (Ok (B2b c)) -> go a2a a2b (c :: b2b) (ext :: kept) tl)
  in
  go [] None [] [] exts

(* [reprs_through c r] pairs each array to array codec with the decoded
   representation entering it, and gives the representation reaching the
   array to bytes codec. *)
and reprs_through c repr0 =
  let rec go acc r = function
    | [] -> (List.rev acc, r)
    | (a : a2a) :: tl -> go ((a, r) :: acc) (a.encoded_repr r) tl
  in
  go [] repr0 c.a2a

and encoded_size c repr0 =
  let _, r = reprs_through c repr0 in
  List.fold_left
    (fun s (b : b2b) -> b.encoded_size s)
    (c.a2b.encoded_size r) c.b2b

and decode_chunk c repr0 bytes =
  let stages, r_a2b = reprs_through c repr0 in
  (* The size entering each bytes to bytes codec, in encode order, so
     each decode step knows the exact size it must produce. *)
  let sizes_in =
    let rec go acc s = function
      | [] -> List.rev acc
      | (b : b2b) :: tl -> go (s :: acc) (b.encoded_size s) tl
    in
    go [] (c.a2b.encoded_size r_a2b) c.b2b
  in
  let bytes =
    List.fold_left2
      (fun bs (b : b2b) ds -> b.decode bs ~decoded_size:ds)
      bytes (List.rev c.b2b) (List.rev sizes_in)
  in
  let slab = c.a2b.decode bytes r_a2b in
  List.fold_left (fun s ((a : a2a), r) -> a.decode s r) slab (List.rev stages)

and encode_chunk c slab =
  let slab = List.fold_left (fun s (a : a2a) -> a.encode s) slab c.a2a in
  List.fold_left (fun bs (b : b2b) -> b.encode bs) (c.a2b.encode slab) c.b2b

let chain_exts c = c.exts

let supports_partial c =
  c.a2a = [] && c.b2b = [] && c.a2b.partial_decode <> None

let partial_decode c r src sub =
  if supports_partial c then
    match c.a2b.partial_decode with
    | Some f -> Some (f src r sub)
    | None -> None
  else None
