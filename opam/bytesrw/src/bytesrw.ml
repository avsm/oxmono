(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Bytes = struct
  include Bytes

  let strf = Printf.sprintf
  let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
  let err_channel_pos kind pos =
    strf "%s: channel position %Ld doesn't fit on int" kind pos

  module Slice = struct
    let err_invalid ~first ~length ~len =
      invalid_argf "invalid slice: first:%d length:%d bytes:%d" first length len

    let err_invalid_sub ~first ~length ~len =
      invalid_argf "invalid subslice: first:%d length:%d slice:%d"
        first length len

    let err_empty_range ~first ~last ~len =
      invalid_argf "invalid slice: first:%d last:%d bytes:%d" first last len

    let err_length l =
      invalid_argf "invalid slice length: %d is not positive" l

    (* Slice lengths *)

    type length = int
    let check_length l = if l <= 0 then err_length l else l

    let io_buffer_size = 65536
    let unix_io_buffer_size = 65536
    let default_length = io_buffer_size

    (* Slices *)

    type t =
      (* Note: some C bindings rely on this layout. *)
      { bytes : Bytes.t; first : int; length : int }

    let eod = { bytes = Bytes.empty; first = 0; length = 0 }
    let[@inline][@zero_alloc] is_eod (s @ local) = s == eod

    let[@inline] make bytes ~first ~length =
      let len = Bytes.length bytes in
      if not (0 <= first && 0 < length && first + length <= len)
      then err_invalid ~first ~length ~len;
      { bytes; first; length }

    let make_or_eod bytes ~first ~length =
      let len = Bytes.length bytes in
      if not (0 <= first && 0 <= length && first + length <= len)
      then err_invalid ~first ~length ~len;
      if length = 0 then eod else { bytes; first; length }

    let[@inline][@zero_alloc] bytes s = s.bytes
    let[@inline][@zero_alloc] first (s @ local) = s.first
    let[@inline][@zero_alloc] last (s @ local) = s.first + s.length - 1
    let[@inline][@zero_alloc] length (s @ local) = s.length

    let copy ~tight (s @ local) =
      if s.length = 0 then eod else
      if not tight then { bytes = Bytes.copy s.bytes; first = s.first; length = s.length } else
      let bytes = Bytes.sub s.bytes s.first s.length in
      { bytes; first = 0; length = s.length}

    let[@zero_alloc] compare (s0 @ local) (s1 @ local) =
      let len0 = s0.length and len1 = s1.length in
      let len_cmp = Int.compare len0 len1 in
      if len_cmp <> 0 then len_cmp else begin
        let first0 = s0.first and first1 = s1.first in
        let b0 = s0.bytes and b1 = s1.bytes in
        let max = len0 - 1 in
        let mutable i = 0 in
        let mutable cmp = 0 in
        while cmp = 0 && i <= max do
          let c0 = Bytes.unsafe_get b0 (first0 + i) in
          let c1 = Bytes.unsafe_get b1 (first1 + i) in
          cmp <- Char.compare c0 c1;
          i <- i + 1
        done;
        cmp
      end

    let[@inline][@zero_alloc] equal (s0 @ local) (s1 @ local) = compare s0 s1 = 0

    (* Breaking slices *)

    let take n s =
      if n <= 0 || is_eod s then None else
      if n >= s.length then Some s else
      Some { s with length = n }

    let drop n s =
      if n >= s.length || is_eod s then None else
      if n <= 0 then Some s else
      Some { s with first = s.first + n; length = s.length - n }

    let break n s = take n s, drop n s

    let sub' ~allow_eod s ~first ~length =
      let len = s.length in
      if not (0 <= first && (0 < length || (length = 0 && allow_eod)) &&
              first + length <= len)
      then err_invalid_sub ~first ~length ~len else
      if length = 0 then eod else
      { bytes = s.bytes; first = s.first + first; length }

    let sub s ~first ~length = sub' ~allow_eod:false s ~first ~length
    let sub_or_eod s ~first ~length = sub' ~allow_eod:true s ~first ~length

    let subrange' ~allow_eod ?(first = 0) ?last s =
      let max = length s - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      if first <= last then
        { bytes = s.bytes; first = s.first + first; length = last - first + 1 }
      else
      if allow_eod then eod else
      err_empty_range ~first ~last ~len:(max + 1)

    let subrange ?first ?last s = subrange' ~allow_eod:false ?first ?last s
    let subrange_or_eod ?first ?last s =
      subrange' ~allow_eod:true ?first ?last s

    (* Converting *)

    let of_bytes' ~allow_eod ?(first = 0) ?last bytes =
      let max = Bytes.length bytes - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      if first <= last then { bytes; first; length = last - first + 1 } else
      if allow_eod then eod else err_empty_range ~first ~last ~len:(max + 1)

    let of_bytes ?first ?last bytes =
      of_bytes' ~allow_eod:false ?first ?last bytes

    let of_bytes_or_eod ?first ?last bytes =
      of_bytes' ~allow_eod:true ?first ?last bytes

    let of_bigbytes' ~allow_eod ?(first = 0) ? last bigbytes =
      let max = Bigarray.Array1.dim bigbytes - 1 in
      let last = match last with
      | None -> max | Some last -> if last > max then max else last
      in
      let first = if first < 0 then 0 else first in
      let length = last - first + 1 in
      let init i = Char.unsafe_chr (Bigarray.Array1.get bigbytes (first + i)) in
      let bytes = Bytes.init length init in
      if first <= last then { bytes; first = 0; length } else
      if allow_eod then eod else err_empty_range ~first ~last ~len:(max + 1)

    let of_bigbytes ?first ?last bytes =
      of_bigbytes' ~allow_eod:false ?first ?last bytes

    let of_bigbytes_or_eod ?first ?last bytes =
      of_bigbytes' ~allow_eod:true ?first ?last bytes

    let of_string ?first ?last s =
      (* We could tighten the string here. *)
      of_bytes ?first ?last (Bytes.of_string s)

    let of_string_or_eod ?first ?last s =
      (* We could tighten the string here. *)
      of_bytes_or_eod ?first ?last (Bytes.of_string s)

    let to_bytes (s @ local) = Bytes.sub s.bytes s.first s.length
    let to_string (s @ local) = Bytes.sub_string s.bytes s.first s.length
    let to_bigbytes (s @ local) =
      (* This should use blits. *)
      let len = s.length and first = s.first and bytes = s.bytes in
      Bigarray.Array1.init Bigarray.Int8_unsigned Bigarray.c_layout
        len (fun i -> Bytes.get_uint8 bytes (first + i))

    let add_to_buffer b (s @ local) = Buffer.add_subbytes b s.bytes s.first s.length
    let output_to_out_channel oc (s @ local) =
      let b = Bytes.unsafe_to_string s.bytes in
      Out_channel.output_substring oc b s.first s.length

    (* Formatting *)

    let pp_meta ppf (s @ local) =
      Format.fprintf ppf "@[[%04d;%04d]@ len:%04d@]"
        s.first (s.first + s.length - 1) s.length

    let pp_full ~hex ppf (s @ local) =
      let first = s.first and len = s.length and bytes = s.bytes in
      let pp_bytes ppf () =
        if not hex
        then Bytesrw_fmt.pp_raw ~first ~len ppf bytes
        else Bytesrw_fmt.pp_hex ~addr:true ~ascii:true ~start:first
            ~len () ppf bytes
      in
      Format.fprintf ppf "@[<v>@[[%04d;%04d]@ len:%04d@]@,%a@]"
        first (first + len - 1) len pp_bytes ()

    let pp_head ~hex c ppf (s @ local) =
      let first = s.first and len = s.length and bytes = s.bytes in
      let pp_head =
        Bytesrw_fmt.(if hex then pp_head_hex else pp_head_raw)
          c ~first ~len
      in
      Format.fprintf ppf "@[@[[%04d;%04d]@ len:%04d@] %a@]"
        first (first + len - 1) len pp_head bytes

    let pp' ?(head = 4) ?(hex = true) () ppf (s @ local) =
      if is_eod s then Format.pp_print_string ppf "<eod>" else
      if head = -1 then pp_full ~hex ppf s else pp_head ~hex head ppf s

    let pp ppf (s @ local) = pp' () ppf s

    let tracer ?(pp = pp) ?(ppf = Format.err_formatter)  ~id s =
      Format.fprintf ppf "@[[%3s]: @[%a@]@]@." id pp s
  end

  module Stream = struct
    type pos = int
    type format = string
    type error = ..
    type error_context =
      { context : [`R|`W] option;
        format : format;
        message : error -> string;
        pos : pos option; }

    exception Error of (error * error_context)

    let error_message (error, m) =
      let context = match m.context with
      | Some `R -> "reader" | Some `W -> "writer" | None -> ""
      in
      let header = match m.format, context with
      | "", v | v, "" -> v | fmt, ctx -> String.concat " " [fmt; ctx]
      in
      let message = m.message error in
      let pos = match m.pos with Some p -> strf "%d:" p |  None -> "" in
      strf "%s:%s %s" header pos message

    let error_to_result e = Result.Error (error_message e)

    type 'e format_error =
      { format : format;
        case : 'e -> error;
        message : error -> string; }

    let make_format_error ~format ~case ~message = { format; case; message }

    let error' ~pos ?context fmt e =
      let format = fmt.format and message = fmt.message in
      let ctx = { context; format; message; pos } in
      raise (Error (fmt.case e, ctx))

    let error fmt ?context e = error' ~pos:None ?context fmt e

    (* Limits *)

    type error += Limit of int
    let limit_error =
      let case l = Limit l in
      let message = function
      | Limit l -> strf "Limit of %d bytes exceeded." l | _ -> assert false
      in
      make_format_error ~format:"" ~case ~message

    let init () =
      let printer = function
      | Error e -> Some (strf "Bytes.Stream.Error <%s>" (error_message e))
      | _ -> None
      in
      Printexc.register_printer printer

    let () = init ()
  end

  module Reader = struct
    type t =
      { mutable pos : int;
        mutable read : unit -> Slice.t;
        slice_length : Slice.length }

    let make ?(pos = 0) ?(slice_length = Slice.default_length) read =
      { pos; read; slice_length = Slice.check_length slice_length }

    let read_eod () = Slice.eod
    let empty ?pos ?slice_length () = make ?pos ?slice_length read_eod
    let[@inline][@zero_alloc] pos (r @ local) = r.pos
    let[@inline][@zero_alloc] read_length (r @ local) = r.pos
    let[@inline][@zero_alloc] slice_length (r @ local) = r.slice_length
    let error fmt r ?pos e =
      let pos = match pos with
      | None -> r.pos | Some p when p < 0 -> r.pos + p | Some p -> p
      in
      Stream.error' ~context:`R ~pos:(Some pos) fmt e

    (* Reading *)

    let read r =
      let slice = r.read () in
      let len = Slice.length slice in
      (if len = 0 then r.read <- read_eod);
      r.pos <- r.pos + len; slice

    let push_back r s =
      if Slice.is_eod s then () else
      let read = r.read in
      let next_read () = r.read <- read; s in
      r.read <- next_read;
      r.pos <- r.pos - Slice.length s

    let sniff n r =
      if n <= 0 then "" else match read r with
      | s when Slice.is_eod s -> ""
      | s when n <= Slice.length s ->
          push_back r s; Bytes.sub_string (Slice.bytes s) (Slice.first s) n
      | s ->
          (* We have to go over multiple slices. Careful about validity ! *)
          let rec loop b i rem r = function
          | s when Slice.is_eod s ->
              let sniff_len = n - rem (* assert (sniff_len > 0) *) in
              let back = Slice.make b ~first:0 ~length:sniff_len in
              push_back r back;
              Bytes.sub_string b 0 sniff_len
          | s ->
              let slen = Slice.length s in
              let n = Int.min slen rem in
              Bytes.blit (Slice.bytes s) (Slice.first s) b i n;
              let rem = rem - n in
              if rem > 0 then loop b (i + n) rem r (read r) else
              let s0 = Slice.make b ~first:0 ~length:(Bytes.length b) in
              (match Slice.drop n s with
              | None -> () | Some s1 -> push_back r s1);
              push_back r s0;
              (* Unsafe is ok: the consumer of s0 is not supposed to mutate the
                 bytes. *)
              Bytes.unsafe_to_string b
          in
          loop (Bytes.create n) 0 n r s

    let skip n r =
      if n <= 0 then () else begin
        let mutable remaining = n in
        while remaining > 0 do
          let s = read r in
          let len = Slice.length s in
          if len = 0 then remaining <- 0  (* EOF *)
          else if len <= remaining then remaining <- remaining - len
          else begin
            match Slice.drop remaining s with
            | None -> remaining <- 0
            | Some back -> push_back r back; remaining <- 0
          end
        done
      end

    let rec discard r = if Slice.is_eod (read r) then () else discard r

    (* Filters *)

    type filter = ?pos:Stream.pos -> ?slice_length:Slice.length -> t -> t

    let sub n ?pos ?slice_length r =
      if n <= 0 then empty ?pos ?slice_length () else
      let slice_length = Option.value ~default:r.slice_length slice_length in
      let slice_length = Slice.check_length slice_length in
      let pos = Option.value ~default:r.pos pos in
      let sr = make ~pos ~slice_length read_eod in
      let count = ref n in
      let read () =
        if !count <= 0 then Slice.eod
        else match read r with
        | slice when Slice.is_eod slice -> sr.read <- read_eod; slice
        | slice ->
            let slen = Slice.length slice in
            if slen <= !count then begin
              count := !count - slen;
              if !count <= 0 then sr.read <- read_eod;
              slice
            end else begin
              let ret, back = Slice.break !count slice in
              (match back with Some b -> push_back r b | None -> ());
              count := 0;
              sr.read <- read_eod;
              match ret with Some s -> s | None -> Slice.eod
            end
      in
      sr.read <- read; sr

    let limit ?action n ?pos ?slice_length r =
      let action = match action with
      | Some act -> act | None -> error Stream.limit_error ?pos:None
      in
      let slice_length = Option.value ~default:r.slice_length slice_length in
      let slice_length = Slice.check_length slice_length in
      let pos = Option.value ~default:r.pos pos in
      let lr = make ~pos ~slice_length read_eod in
      let left = ref n in
      let triggered = ref false in
      let read () =
        if !triggered then Slice.eod
        else match read r with
        | slice when Slice.is_eod slice -> lr.read <- read_eod; slice
        | slice when !left <= 0 ->
            push_back r slice; lr.read <- read_eod;
            triggered := true; action lr n; Slice.eod
        | slice ->
            let slen = Slice.length slice in
            if slen <= !left then begin
              left := !left - slen;
              slice
            end else begin
              let ret, back = Slice.break !left slice in
              (match back with Some b -> push_back r b | None -> ());
              left := 0;
              match ret with Some s -> s | None -> Slice.eod
            end
      in
      lr.read <- read; lr

    let reslice ?pos ?slice_length r =
      let slice_length = Option.value ~default:r.slice_length slice_length in
      let slice_length = Slice.check_length slice_length in
      let pos = Option.value ~default:r.pos pos in
      let reslicer = make ~pos ~slice_length read_eod in
      let buf = Bytes.make slice_length '\x00' in
      let buf_slice = Slice.make ~first:0 ~length:slice_length buf in
      let buf_len = ref 0 in
      let last_slice = ref Slice.eod in
      let last_rem = ref 0 in
      let rec reslice () =
        (* Drain remainder from previous slice *)
        if !last_rem > 0 then begin
          let copy_len = Int.min !last_rem (slice_length - !buf_len) in
          let src = Slice.bytes !last_slice in
          let src_off = Slice.first !last_slice + Slice.length !last_slice - !last_rem in
          Bytes.blit src src_off buf !buf_len copy_len;
          buf_len := !buf_len + copy_len;
          last_rem := !last_rem - copy_len
        end;
        if !buf_len = slice_length then begin
          buf_len := 0; buf_slice
        end else match read r with
        | slice when Slice.is_eod slice ->
            if !buf_len = 0 then Slice.eod
            else begin
              let result = Slice.make ~first:0 ~length:!buf_len buf in
              buf_len := 0; result
            end
        | slice ->
            let slen = Slice.length slice in
            (* Fast path: empty buffer and exact slice size *)
            if !buf_len = 0 && slen = slice_length then slice
            else begin
              last_slice := slice;
              last_rem := slen;
              reslice ()
            end
      in
      reslicer.read <- reslice; reslicer

    (* Appending *)

    let append ?pos ?slice_length r0 r1 =
      let slice_length = match slice_length with
      | None -> Int.max r0.slice_length r1.slice_length | Some l -> l
      in
      let pos = match pos with Some p -> p | None -> 0 in
      let ar = make ~pos ~slice_length read_eod in
      let reading_r0 = ref true in
      let read () =
        if !reading_r0 then begin
          match read r0 with
          | s when Slice.is_eod s -> reading_r0 := false; read r1
          | s -> s
        end else read r1
      in
      ar.read <- read; ar

    (* Tap *)

    let tap f r =
      let read () = let slice = read r in f slice; slice in
      { r with read }

    (* Predicates and comparison *)

    let compare r0 r1 =
      let slice_length = Int.max r0.slice_length r1.slice_length in
      let r0 = reslice ~slice_length r0 in
      let r1 = reslice ~slice_length r1 in
      let mutable result = 0 in
      let mutable done_ = false in
      while not done_ do
        let s0 = read r0 and s1 = read r1 in
        let cmp = Slice.compare s0 s1 in
        if cmp <> 0 then begin result <- cmp; done_ <- true end
        else if Slice.is_eod s0 then done_ <- true
      done;
      result

    let equal r0 r1 = compare r0 r1 = 0

    (* Converting *)

    let of_bytes ?pos ?slice_length b =
      let blen = Bytes.length b in
      if blen = 0 then empty ?pos ?slice_length () else
      let slice_length = match slice_length with
      | None -> blen | Some slen -> Int.min slen blen
      in
      if slice_length = blen then begin
        let returned = ref false in
        let s = Slice.make b ~first:0 ~length:blen in
        let read () =
          if !returned then Slice.eod
          else begin returned := true; s end
        in
        make ?pos ~slice_length read
      end else begin
        let offset = ref 0 in
        let read () =
          if !offset >= blen then Slice.eod
          else begin
            let len = Int.min slice_length (blen - !offset) in
            let s = Slice.make b ~first:!offset ~length:len in
            offset := !offset + len;
            s
          end
        in
        make ?pos ~slice_length read
      end

    let of_string ?pos ?slice_length s =
      (* Unsafe is ok: the consumer is not supposed to mutate the bytes. *)
      of_bytes ?pos ?slice_length (Bytes.unsafe_of_string s)

    let of_in_channel ?pos ?(slice_length = Slice.io_buffer_size) ic =
      let () = In_channel.set_binary_mode ic true in
      let pos = match pos with
      | Some p -> p
      | None ->
          let pos = In_channel.pos ic in
          if pos < 0L then 0 else
          match Int64.unsigned_to_int pos with
          | Some p -> p
          | None -> raise (Sys_error (err_channel_pos "Bytes.Reader" pos))
      in
      let slice_length = Slice.check_length slice_length in
      let b = Bytes.create slice_length in
      let read () =
        let count = In_channel.input ic b 0 (Bytes.length b) in
        if count = 0 then Slice.eod else Slice.make b ~first:0 ~length:count
      in
      make ~pos ~slice_length read

    let of_slice ?pos ?slice_length s =
      if Slice.is_eod s then empty ?pos ?slice_length () else
      let slice_length = match slice_length with
      | None -> Slice.length s | Some l -> l
      in
      let b = Slice.bytes s in
      let start = Slice.first s in
      let len = Slice.length s in
      let offset = ref 0 in
      let read () =
        if !offset >= len then Slice.eod
        else begin
          let chunk = Int.min slice_length (len - !offset) in
          let result = Slice.make b ~first:(start + !offset) ~length:chunk in
          offset := !offset + chunk;
          result
        end
      in
      make ?pos ~slice_length read

    let of_slice_seq ?pos ?slice_length seq =
      let seq = ref seq in
      let read () = match !seq () with
      | Seq.Nil -> Slice.eod | Seq.Cons (slice, next) -> seq := next; slice
      in
      make ?pos ?slice_length read

    let add_to_buffer b r =
      let mutable done_ = false in
      while not done_ do
        let s = read r in
        if Slice.is_eod s then done_ <- true
        else Slice.add_to_buffer b s
      done

    let to_string r =
      let b = Buffer.create r.slice_length in
      add_to_buffer b r; Buffer.contents b

    let to_slice_seq r =
      let dispense () = match read r with
      | slice when Slice.is_eod slice -> None
      | slice -> Some slice
      in
      Seq.of_dispenser dispense

    let output_to_out_channel ?(flush_slices = false) oc r =
      Out_channel.set_binary_mode oc true;
      let mutable done_ = false in
      while not done_ do
        let s = read r in
        if Slice.is_eod s then done_ <- true
        else begin
          Slice.output_to_out_channel oc s;
          if flush_slices then Out_channel.flush oc
        end
      done

    let filter_string (fs : filter list) s =
      let filter r f = f ?pos:None ?slice_length:None r in
      to_string (List.fold_left filter (of_string s) fs)

    (* Formatting *)

    let pp ppf r =
      Format.fprintf ppf "@[<1><reader pos:%d slice:%d>@]" r.pos r.slice_length
  end

  module Writer = struct
    type t =
      { mutable pos : int;
        slice_length : Slice.length;
        mutable write : Slice.t -> unit; }

    let make ?(pos = 0) ?(slice_length = Slice.default_length) write =
      { pos; slice_length = Slice.check_length slice_length; write }

    let[@inline][@zero_alloc] pos (w @ local) = w.pos
    let[@inline][@zero_alloc] slice_length (w @ local) = w.slice_length
    let[@inline][@zero_alloc] written_length (w @ local) = w.pos
    let ignore ?pos ?slice_length () = make ?pos ?slice_length (fun s -> ())
    let error fmt w ?pos e =
      let pos = match pos with
      | None -> w.pos | Some p when p < 0 -> w.pos + p | Some p -> p
      in
      Stream.error' ~context:`W ~pos:(Some pos) fmt e

    (* Writing *)

    let write_only_eod (s @ local) =
      if Slice.is_eod s then () else invalid_arg "slice written after eod"

    let write w (slice @ local) =
      let write = w.write in
      let n = Slice.length slice in
      (if n = 0 then w.write <- write_only_eod);
      w.pos <- w.pos + n; write slice

    let write_eod w = write w Slice.eod
    let write_bytes w (b @ local) =
      let blen = Bytes.length b in
      let slice_length = Int.min w.slice_length blen in
      let mutable offset = 0 in
      while offset < blen do
        let len = Int.min slice_length (blen - offset) in
        write w (Slice.make b ~first:offset ~length:len);
        offset <- offset + len
      done

    let write_string w (s @ local) =
      (* Unsafe is ok: the writer is not supposed to mutate the bytes. *)
      write_bytes w (Bytes.unsafe_of_string s)

    let write_reader ~eod w r =
      let mutable done_ = false in
      while not done_ do
        let slice = Reader.read r in
        if Slice.is_eod slice then begin
          done_ <- true;
          if eod then write_eod w
        end else write w slice
      done

    let write_in_channel ~eod w ic =
      In_channel.set_binary_mode ic true;
      let buf = Bytes.create w.slice_length in
      let mutable done_ = false in
      while not done_ do
        let count = In_channel.input ic buf 0 w.slice_length in
        if count = 0 then begin
          done_ <- true;
          if eod then write_eod w
        end else write w (Slice.make buf ~first:0 ~length:count)
      done

    (* Taps *)

    let tap f w =
      let write slice = f slice; write w slice in
      { w with write }

    (* Converting *)

    let of_out_channel
        ?pos ?(slice_length = Slice.io_buffer_size) ?(flush_slices = false) oc
      =
      let () = Out_channel.set_binary_mode oc true in
      let pos = match pos with
      | Some pos -> pos
      | None ->
          let pos = Out_channel.pos oc in
          if pos < 0L then 0 else
          match Int64.unsigned_to_int pos with
          | Some pos -> pos
          | None -> raise (Sys_error (err_channel_pos "Bytes.Writer" pos))
      in
      let write = function
      | slice when Slice.is_eod slice -> ()
      | slice ->
          Slice.output_to_out_channel oc slice;
          if flush_slices then Out_channel.flush oc
      in
      make ~pos ~slice_length write

    let of_buffer ?pos ?slice_length b =
      let write s = if Slice.is_eod s then () else Slice.add_to_buffer b s in
      make ?pos ?slice_length write

    (* Filters *)

    type filter =
      ?pos:Stream.pos -> ?slice_length:Slice.length -> eod:bool ->  t -> t

    let limit ?action n ?pos ?slice_length ~eod w =
      let action = match action with
      | Some act -> act | None -> error Stream.limit_error ?pos:None
      in
      let slice_length = Option.value ~default:w.slice_length slice_length in
      let pos = Option.value ~default:w.pos pos in
      let lw = make ~pos ~slice_length write_only_eod in
      let left = ref n in
      let triggered = ref false in
      let do_write slice =
        if Slice.is_eod slice then begin
          if eod then write_eod w
        end else if !triggered then ()
        else begin
          let slen = Slice.length slice in
          left := !left - slen;
          if !left >= 0 then write w slice
          else begin
            let to_write = slen + !left in
            if to_write > 0 then
              (match Slice.take to_write slice with
               | Some s -> write w s
               | None -> ());
            if eod then write_eod w;
            lw.write <- write_only_eod;
            triggered := true;
            action w n
          end
        end
      in
      lw.write <- do_write; lw

    let filter_string fs s =
      let b = Buffer.create (String.length s) in
      let filter w f = f ?pos:None ?slice_length:None ~eod:true w in
      let w = List.fold_left filter (of_buffer b) fs in
      write_string w s; write_eod w; Buffer.contents b

    (* Formatting *)

    let pp ppf w =
      Format.fprintf ppf "@[<1><writer pos:%d slice:%d>@]" w.pos w.slice_length
  end

  let pp_hex = Bytesrw_fmt.pp_hex
end
