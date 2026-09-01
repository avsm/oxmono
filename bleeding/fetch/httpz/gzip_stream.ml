open Fetch

let malformed message =
  raise (err (Protocol_error ("malformed gzip response: " ^ message)))

(* Decompress 1.6.0 has four RFC 1952 header bugs: it does not check CM or
   reserved FLG bits, reads XLEN as big-endian, and checks FHCRC against a
   reconstructed (and incomplete) header using the wrong half and byte order
   of its CRC-32.  Validate the original header here, then adjust only the
   bytes handed to that decoder.  The adjusted header has exactly the same
   length, so [src_rem] still maps onto the transport input window. *)

let max_header_bytes = 256 * 1024

let crc32_byte crc byte =
  let crc = ref (Int32.logxor crc (Int32.of_int byte)) in
  for _ = 1 to 8 do
    crc :=
      Int32.logxor
        (Int32.shift_right_logical !crc 1)
        (if Int32.logand !crc 1l = 0l then 0l else 0xedb88320l)
  done;
  !crc

let crc32_finish = Int32.lognot

type header_stage =
  | Fixed
  | Extra_length
  | Extra_data of int
  | Filename
  | Comment
  | Header_checksum
  | Ready

type header = {
  mutable stage : header_stage;
  mutable pos : int;
  mutable flags : int;
  (* Header CRC work is unnecessary unless FHCRC is present. Optional names
     and comments can be large, so keep their common path linear copying and
     scanning without eight bitwise rounds per byte. *)
  mutable checksum : bool;
  (* The RFC checksum covers every original header byte. *)
  mutable crc : int32;
  (* This is the subset decompress 1.6.0 reconstructs for its broken FHCRC
     comparison: the fixed header and the two zero-terminated strings, but no
     XLEN or FEXTRA payload. *)
  mutable decompress_crc : int32;
}

let header () =
  {
    stage = Fixed;
    pos = 0;
    flags = 0;
    checksum = false;
    crc = Int32.minus_one;
    decompress_crc = Int32.minus_one;
  }

let byte cs i = Cstruct.get_uint8 cs i

let crc_range get h ~decompress first last =
  if h.checksum then
    for i = first to last - 1 do
      let octet = get i in
      h.crc <- crc32_byte h.crc octet;
      if decompress then
        h.decompress_crc <- crc32_byte h.decompress_crc octet
    done

let after_extra h =
  h.stage <-
    if h.flags land 0x08 <> 0 then Filename
    else if h.flags land 0x10 <> 0 then Comment
    else if h.flags land 0x02 <> 0 then Header_checksum
    else Ready

let after_filename h =
  h.stage <-
    if h.flags land 0x10 <> 0 then Comment
    else if h.flags land 0x02 <> 0 then Header_checksum
    else Ready

let after_comment h =
  h.stage <- if h.flags land 0x02 <> 0 then Header_checksum else Ready

type header_result = [ `Ready | `Partial | `Malformed of string ]

let rec prepare_header h cs len : header_result =
  let get = byte cs in
  match h.stage with
  | Ready -> `Ready
  | Fixed ->
      if len < 10 then `Partial
      else if get 0 <> 0x1f || get 1 <> 0x8b then
        `Malformed "invalid magic bytes"
      else if get 2 <> 8 then
        `Malformed
          (Printf.sprintf "unsupported compression method %d (expected 8)"
             (get 2))
      else
        let flags = get 3 in
        if flags land 0xe0 <> 0 then
          `Malformed
            (Printf.sprintf "reserved flag bits are set (FLG=0x%02x)" flags)
        else begin
          h.flags <- flags;
          h.checksum <- flags land 0x02 <> 0;
          crc_range get h ~decompress:true 0 10;
          h.pos <- 10;
          h.stage <- if flags land 0x04 <> 0 then Extra_length else Extra_data 0;
          prepare_header h cs len
        end
  | Extra_length ->
      if len - h.pos < 2 then `Partial
      else begin
        let at = h.pos in
        let lo = get at and hi = get (at + 1) in
        crc_range get h ~decompress:false at (at + 2);
        h.pos <- at + 2;
        h.stage <- Extra_data (lo lor (hi lsl 8));
        (* The decoder below expects the opposite byte order.  The RFC
           checksum state above has already consumed the original bytes. *)
        Cstruct.set_uint8 cs at hi;
        Cstruct.set_uint8 cs (at + 1) lo;
        prepare_header h cs len
      end
  | Extra_data 0 ->
      after_extra h;
      prepare_header h cs len
  | Extra_data left ->
      let available = len - h.pos in
      if available = 0 then `Partial
      else begin
        let n = min available left in
        crc_range get h ~decompress:false h.pos (h.pos + n);
        h.pos <- h.pos + n;
        h.stage <- Extra_data (left - n);
        prepare_header h cs len
      end
  | Filename | Comment as stage ->
      if h.pos = len then `Partial
      else begin
        let start = h.pos in
        let rec find_zero i =
          if i = len then None
          else if get i = 0 then Some i
          else find_zero (i + 1)
        in
        match find_zero start with
        | None ->
            crc_range get h ~decompress:true start len;
            h.pos <- len;
            `Partial
        | Some zero ->
            crc_range get h ~decompress:true start (zero + 1);
            h.pos <- zero + 1;
            (match stage with Filename -> after_filename h | Comment -> after_comment h | _ -> assert false);
            prepare_header h cs len
      end
  | Header_checksum ->
      if len - h.pos < 2 then `Partial
      else
        let observed = get h.pos lor (get (h.pos + 1) lsl 8) in
        let expected =
          Int32.to_int (Int32.logand (crc32_finish h.crc) 0xffffl)
        in
        if observed <> expected then
          `Malformed
            (Printf.sprintf
               "invalid header checksum (expected 0x%04x, received 0x%04x)"
               expected observed)
        else begin
          (* Satisfy decompress's broken comparison after validating the real
             FHCRC.  It compares the high half of a CRC over its reconstructed
             header as a big-endian integer. *)
          let compat = crc32_finish h.decompress_crc in
          let compat =
            Int32.to_int
              (Int32.logand (Int32.shift_right_logical compat 16) 0xffffl)
          in
          Cstruct.set_uint8 cs h.pos (compat lsr 8);
          Cstruct.set_uint8 cs (h.pos + 1) (compat land 0xff);
          h.pos <- h.pos + 2;
          h.stage <- Ready;
          `Ready
        end

module type INF = sig
  type decoder

  val decode :
    decoder ->
    [ `Await of decoder | `Flush of decoder | `End of decoder
    | `Malformed of string ]

  val src : decoder -> De.bigstring -> int -> int -> decoder
  val dst_rem : decoder -> int
  val src_rem : decoder -> int
  val reset : decoder -> decoder
  val flush : decoder -> decoder
end

module Inflate (Inf : INF) = struct
  type phase =
    | Need_header of header
    | Decoding
    | Member_ended
    | Ended

  type t = {
    src : Eio.Flow.source_ty Eio.Resource.t;
    mutable i : De.bigstring;
    o : De.bigstring;
    o_cs : Cstruct.t;
    mutable i_cs : Cstruct.t;
    mutable d : Inf.decoder;
    mutable ready : (int * int) option;
    mutable phase : phase;
    mutable input_len : int;
    mutable input_eof : bool;
    mutable members : int;
    max_members : int;
  }

  let v ~src ~i ~o ~max_members d =
    {
      src;
      i;
      o;
      o_cs = Cstruct.of_bigarray o;
      i_cs = Cstruct.of_bigarray i;
      d;
      ready = None;
      phase = Need_header (header ());
      input_len = 0;
      input_eof = false;
      members = 0;
      max_members;
    }

  let read_methods = []
  let window t = De.bigstring_length t.o - Inf.dst_rem t.d

  let grow_header_buffer t =
    let old = De.bigstring_length t.i in
    if old >= max_header_bytes then
      malformed
        (Printf.sprintf "member header exceeds %d bytes" max_header_bytes);
    let size = min max_header_bytes (max (old * 2) (old + 1)) in
    let next = De.bigstring_create size in
    let next_cs = Cstruct.of_bigarray next in
    Cstruct.blit t.i_cs 0 next_cs 0 t.input_len;
    t.i <- next;
    t.i_cs <- next_cs

  let read_header_input t =
    if t.input_eof then
      if t.input_len = 0 && t.members > 0 then t.phase <- Ended
      else malformed "unexpected end of input in member header"
    else begin
      if t.input_len = De.bigstring_length t.i then grow_header_buffer t;
      let dst =
        Cstruct.sub_local t.i_cs t.input_len
          (De.bigstring_length t.i - t.input_len)
      in
      match Eio.Flow.single_read t.src dst with
      | n -> t.input_len <- t.input_len + n
      | exception End_of_file ->
          t.input_eof <- true;
          if t.input_len = 0 && t.members > 0 then t.phase <- Ended
          else malformed "unexpected end of input in member header"
    end

  (* A decoder that needs a structure whole, the ten fixed header bytes above
     all, asks for more input without consuming what it already has, so the
     unconsumed suffix has to survive the refill. *)
  let refill_decoder t =
    let rem = Inf.src_rem t.d in
    let base =
      if rem <= 0 then 0
      else begin
        if rem >= De.bigstring_length t.i then
          malformed "decoder stalled on a full input buffer";
        Cstruct.blit t.i_cs (t.input_len - rem) t.i_cs 0 rem;
        rem
      end
    in
    match
      Eio.Flow.single_read t.src
        (Cstruct.sub_local t.i_cs base (De.bigstring_length t.i - base))
    with
    | n ->
        t.input_len <- base + n;
        t.d <- Inf.src t.d t.i 0 (base + n)
    | exception End_of_file ->
        t.input_eof <- true;
        t.input_len <- 0;
        t.d <- Inf.src t.d t.i 0 0

  let start_next_member t =
    let rem = max 0 (Inf.src_rem t.d) in
    if rem > 0 then
      Cstruct.blit t.i_cs (t.input_len - rem) t.i_cs 0 rem;
    t.input_len <- rem;
    t.d <- Inf.reset t.d;
    t.phase <- Need_header (header ())

  let rec single_read t buf =
    match t.ready with
    | Some (pos, len) ->
        let n = min len (Cstruct.length buf) in
        Cstruct.blit t.o_cs pos buf 0 n;
        if n = len then begin
          t.ready <- None;
          if t.phase = Decoding then t.d <- Inf.flush t.d
        end
        else t.ready <- Some (pos + n, len - n);
        n
    | None ->
        match t.phase with
        | Ended -> raise End_of_file
        | Need_header h -> begin
            match prepare_header h t.i_cs t.input_len with
            | `Ready ->
                if t.members >= t.max_members then
                  malformed
                    (Printf.sprintf "representation has more than %d members"
                       t.max_members);
                t.members <- t.members + 1;
                t.d <- Inf.src t.d t.i 0 t.input_len;
                t.phase <- Decoding;
                single_read t buf
            | `Partial ->
                read_header_input t;
                single_read t buf
            | `Malformed message -> malformed message
          end
        | Member_ended ->
            start_next_member t;
            single_read t buf
        | Decoding -> begin
            match Inf.decode t.d with
            | `Await d ->
                t.d <- d;
                refill_decoder t;
                single_read t buf
            | `Flush d ->
                t.d <- d;
                (match window t with
                | 0 -> t.d <- Inf.flush t.d
                | len -> t.ready <- Some (0, len));
                single_read t buf
            | `End d ->
                t.d <- d;
                t.phase <- Member_ended;
                (match window t with
                | 0 -> single_read t buf
                | len ->
                    t.ready <- Some (0, len);
                    single_read t buf)
            | `Malformed "Unexpected end of input" when not t.input_eof ->
                (* Decompress reports this when a transport window ends at a
                   structure boundary.  Until the framed body itself ends,
                   another read can complete the structure. *)
                refill_decoder t;
                single_read t buf
            | `Malformed message -> malformed message
          end
end

module Gunzip = Inflate (Gz.Inf)

let handler = Eio.Flow.Pi.source (module Gunzip)

let gunzip ?(max_members = 1024) src =
  if max_members <= 0 then
    invalid_arg "Gzip_stream.gunzip: max_members must be positive";
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let d = Gz.Inf.decoder `Manual ~o in
  Eio.Resource.T (Gunzip.v ~src ~i ~o ~max_members d, handler)
