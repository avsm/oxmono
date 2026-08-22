(*---------------------------------------------------------------------------
   Copyright (c) 2024 The bytesrw programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let pp_hex_char ppf i = Format.fprintf ppf "%02x" i
let pp_raw_char ppf c = match Char.code c with
| 0x0A -> Format.pp_print_string ppf "\n"
| 0x0D -> Format.pp_print_string ppf "\r"
| i when i < 0x20 || i = 0x7f -> Format.fprintf ppf "\\x%a" pp_hex_char i
| _ -> (* XXX We should try to decode utf-8 and if not then escape *)
    Format.pp_print_char ppf c

let pp_head_hex count ~first ~len ppf b =
  let max_idx = first + Int.min count len - 1 in
  if max_idx < first then Format.pp_print_string ppf "<empty>" else begin
    Format.pp_print_char ppf 'x';
    let mutable i = first in
    while i <= max_idx do
      pp_hex_char ppf (Bytes.get_uint8 b i);
      i <- i + 1
    done;
    if first + len - 1 > max_idx then Format.fprintf ppf "@<1>%s" "…"
  end

let pp_head_raw count ~first ~len ppf b =
  let max_idx = first + Int.min count len - 1 in
  if max_idx < first then Format.pp_print_string ppf "<empty>" else begin
    Format.pp_print_char ppf '"';
    let mutable i = first in
    while i <= max_idx do
      Format.pp_print_char ppf (Bytes.get b i);
      i <- i + 1
    done;
    if first + len - 1 > max_idx then Format.fprintf ppf "@<1>%s" "…";
    Format.pp_print_char ppf '"'
  end

let pp_raw ~first ~len ppf b =
  Format.pp_open_vbox ppf 1;
  Format.pp_print_char ppf '"';
  let mutable i = 0 in
  while i < len do
    pp_raw_char ppf (Bytes.get b (first + i));
    if (i + 1) mod 60 = 0 then begin
      Format.pp_print_char ppf '\\';
      Format.pp_print_cut ppf ()
    end;
    i <- i + 1
  done;
  Format.pp_print_char ppf '"';
  Format.pp_close_box ppf ()

(* XXX review this *)

let strf = Printf.sprintf
let err_range ~start ~len ~blen =
  invalid_arg @@
  strf "range start %d len %d: not in bounds [0;%d]" start len (blen - 1)

let[@inline][@zero_alloc] ilog2 v =
  let mutable p = -1 in
  let mutable x = v in
  while x > 0 do
    p <- p + 1;
    x <- x lsr 1
  done;
  p

let pp_address ~addr ~addr_start ~addr_div ~start ~len =
  let pp_32 ppf addr = Format.fprintf ppf "%08x  " addr in
  let pp_64 ppf addr = Format.fprintf ppf "%016x  " addr in
  if not addr then fun ppf _ -> () else
  let astart = match addr_start with Some a -> a | None -> start in
  let amax = astart + len in
  let pp_address =
    if Sys.int_size = 31 then pp_32 else
    if ilog2 amax < 32 then pp_32 else pp_64
  in
  fun ppf off -> pp_address ppf ((astart + off) / addr_div)

let pp_ascii_col ppf get_uint8 b start stop =
  Format.fprintf ppf "  @[<h>@<1>%s" "│";
  let mutable i = start in
  while i <= stop do
    let byte = get_uint8 b i in
    let c = if byte < 0x1F || byte > 0x7E then '.' else Char.chr byte in
    Format.pp_print_char ppf c;
    i <- i + 1
  done;
  Format.fprintf ppf "@<1>%s@]" "│"

let pp_hex
    ?(addr = false) ?addr_start ?(addr_div = 1) ?(count = 16) ?(group = 2)
    ?(ascii = false) ?(start = 0) ?len () ppf b
  =
  let blen = Bytes.length b in
  let len = match len with None -> blen - start | Some len -> len in
  if len = 0 then () else
  let bmax = start + len - 1 in
  if not (0 <= start && start <= bmax && bmax < blen) then
    err_range ~start ~len ~blen
  else begin
    let pp_addr = pp_address ~addr ~addr_start ~addr_div ~start ~len in
    Format.pp_open_vbox ppf 0;
    pp_addr ppf 0;
    Format.fprintf ppf "%02x" (Bytes.get_uint8 b start);
    let mutable i = start + 1 in
    while i <= bmax do
      if i mod count = 0 then begin
        if ascii then pp_ascii_col ppf Bytes.get_uint8 b (i - count) (i - 1);
        Format.pp_print_cut ppf ();
        pp_addr ppf (i - start)
      end
      else if i mod group = 0 then Format.pp_print_char ppf ' ';
      Format.fprintf ppf "%02x" (Bytes.get_uint8 b i);
      i <- i + 1
    done;
    if ascii then begin
      (* Pad incomplete final line *)
      let mutable j = bmax + 1 in
      while j <= bmax + (count - (bmax mod count)) - 1 do
        if j mod group = 0 then Format.pp_print_char ppf ' ';
        Format.fprintf ppf "  ";
        j <- j + 1
      done;
      pp_ascii_col ppf Bytes.get_uint8 b (bmax - bmax mod count) bmax
    end;
    if addr then begin
      Format.pp_print_cut ppf ();
      pp_addr ppf len
    end;
    Format.pp_close_box ppf ()
  end
