(*---------------------------------------------------------------------------
  Copyright (c) 2019 Antonio Nuno Monteiro.
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice,
     this list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
     this list of conditions and the following disclaimer in the documentation
     and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
     may be used to endorse or promote products derived from this software
     without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  SPDX-License-Identifier: BSD-3-Clause
 ---------------------------------------------------------------------------*)

(** HPACK Header Compression per RFC 7541.

    This module implements header compression for HTTP/2, including:
    - Static and dynamic table management
    - Huffman encoding/decoding
    - Integer encoding with variable-length prefix
    - String literal encoding

    Derived from ocaml-h2 with adaptation for Cstruct-based operations. *)

(* ============================================================
   Types
   ============================================================ *)

type header = {
  name : string;
  value : string;
  sensitive : bool;
}

type error = Decoding_error

let pp_error ppf = function
  | Decoding_error -> Format.pp_print_string ppf "HPACK decoding error"

(** Result bind operator for cleaner error handling in decoding. *)
let ( let* ) = Result.bind

(* ============================================================
   Dynamic Table - RFC 7541 Section 2.3.2
   ============================================================ *)

module Dynamic_table = struct
  type t = {
    mutable entries : (string * string * int) array;
    mutable length : int;
    mutable offset : int;
    mutable capacity : int;
    mutable size : int;  (* HPACK size, not array size *)
    mutable max_size : int;
  }

  let default_entry = "", "", 32

  let create max_size =
    let capacity = max 256 max_size in
    { entries = Array.make capacity default_entry
    ; length = 0
    ; offset = 0
    ; capacity
    ; size = 0
    ; max_size
    }

  let[@inline] _get table i =
    table.entries.((table.offset + i) mod table.capacity)

  let[@inline] get table i =
    let name, value, _ = _get table i in
    name, value

  let[@inline] entry_size name value =
    String.length name + String.length value + 32

  let evict_one table =
    table.length <- table.length - 1;
    let i = (table.offset + table.length) mod table.capacity in
    let _, _, entry_size = table.entries.(i) in
    table.entries.(i) <- default_entry;
    table.size <- table.size - entry_size

  let increase_capacity table =
    let new_capacity = 2 * table.capacity in
    let new_entries =
      Array.init new_capacity (fun i ->
        if i < table.length then _get table i else default_entry)
    in
    table.entries <- new_entries;
    table.offset <- 0;
    table.capacity <- new_capacity

  let add table (name, value) =
    let entry_size = entry_size name value in
    while table.size > 0 && table.size + entry_size > table.max_size do
      evict_one table
    done;
    if table.size + entry_size <= table.max_size then begin
      if table.length = table.capacity then increase_capacity table;
      table.length <- table.length + 1;
      table.size <- table.size + entry_size;
      let new_offset = (table.offset + table.capacity - 1) mod table.capacity in
      table.entries.(new_offset) <- name, value, entry_size;
      table.offset <- new_offset
    end

  let[@inline] table_size table = table.length

  let set_capacity table max_size =
    table.max_size <- max_size;
    while table.size > max_size do
      evict_one table
    done
end

(* ============================================================
   Huffman Encoding/Decoding - RFC 7541 Section 5.2
   ============================================================ *)

module Huffman = struct
  let encoded_length s =
    let len = String.length s in
    let rec loop bits i =
      if i < len then
        let input = Char.code s.[i] in
        let _, len_in_bits = H2_huffman_table.encode_table.(input) in
        loop (bits + len_in_bits) (i + 1)
      else
        (bits + 7) / 8
    in
    loop 0 0

  let encode buf off s =
    let bits = ref 0 in
    let bits_left = ref 40 in
    let pos = ref off in
    for i = 0 to String.length s - 1 do
      let code, code_len = H2_huffman_table.encode_table.(Char.code s.[i]) in
      bits_left := !bits_left - code_len;
      bits := !bits lor (code lsl !bits_left);
      while !bits_left <= 32 do
        Cstruct.set_uint8 buf !pos (!bits lsr 32);
        incr pos;
        bits := !bits lsl 8;
        bits_left := !bits_left + 8
      done
    done;
    if !bits_left < 40 then begin
      bits := !bits lor ((1 lsl !bits_left) - 1);
      Cstruct.set_uint8 buf !pos (!bits lsr 32);
      incr pos
    end;
    !pos - off

  let decode s =
    let len = String.length s in
    let buffer = Buffer.create len in
    let[@inline] add_output c =
      if c <> '\000' then Buffer.add_char buffer c
    in
    let[@inline] exists_in_huffman_table token = token <> -1 in
    let rec loop id accept i =
      if i < len then begin
        let input = Char.code s.[i] in
        let index = (id lsl 4) + (input lsr 4) in
        let id, _, output = H2_huffman_table.decode_table.(index) in
        add_output output;
        if exists_in_huffman_table id then begin
          let index = (id lsl 4) + (input land 0x0f) in
          let id, accept, output = H2_huffman_table.decode_table.(index) in
          add_output output;
          if exists_in_huffman_table id
          then loop id accept (i + 1)
          else Error Decoding_error
        end else Error Decoding_error
      end else if not accept then
        Error Decoding_error
      else
        Ok (Buffer.contents buffer)
    in
    loop 0 true 0
end

(* ============================================================
   Integer Encoding - RFC 7541 Section 5.1
   ============================================================ *)

let encode_int buf off prefix n i =
  let max_prefix = (1 lsl n) - 1 in
  if i < max_prefix then begin
    Cstruct.set_uint8 buf off (prefix lor i);
    1
  end else begin
    Cstruct.set_uint8 buf off (prefix lor max_prefix);
    let i = ref (i - max_prefix) in
    let pos = ref (off + 1) in
    while !i >= 128 do
      Cstruct.set_uint8 buf !pos (!i land 127 lor 128);
      incr pos;
      i := !i lsr 7
    done;
    Cstruct.set_uint8 buf !pos !i;
    !pos - off + 1
  end

let decode_int buf off prefix n =
  let max_prefix = (1 lsl n) - 1 in
  let i = prefix land max_prefix in
  if i < max_prefix then
    Ok (i, off)
  else
    let rec loop i m pos =
      if pos >= Cstruct.length buf then
        Error Decoding_error
      else
        let b = Cstruct.get_uint8 buf pos in
        let i = i + ((b land 127) lsl m) in
        if b land 0b1000_0000 = 0b1000_0000
        then loop i (m + 7) (pos + 1)
        else Ok (i, pos + 1)
    in
    loop i 0 off

(* ============================================================
   String Literal Encoding - RFC 7541 Section 5.2
   ============================================================ *)

let encode_string buf off s =
  let string_length = String.length s in
  let huffman_length = Huffman.encoded_length s in
  if huffman_length >= string_length then begin
    (* Raw encoding *)
    let len = encode_int buf off 0 7 string_length in
    Cstruct.blit_from_string s 0 buf (off + len) string_length;
    len + string_length
  end else begin
    (* Huffman encoding *)
    let len = encode_int buf off 128 7 huffman_length in
    let hlen = Huffman.encode buf (off + len) s in
    len + hlen
  end

let decode_string buf off =
  if off >= Cstruct.length buf then
    Error Decoding_error
  else
    let h = Cstruct.get_uint8 buf off in
    let* string_length, pos = decode_int buf (off + 1) h 7 in
    if pos + string_length > Cstruct.length buf then
      Error Decoding_error
    else
      let string_data = Cstruct.to_string ~off:pos ~len:string_length buf in
      let is_huffman = h land 0b1000_0000 <> 0 in
      if is_huffman then
        let* decoded = Huffman.decode string_data in
        Ok (decoded, pos + string_length)
      else
        Ok (string_data, pos + string_length)

(* ============================================================
   Decoder - RFC 7541 Section 6
   ============================================================ *)

module Decoder = struct
  type t = {
    table : Dynamic_table.t;
    max_capacity : int;
  }

  let create max_capacity =
    { table = Dynamic_table.create max_capacity; max_capacity }

  let set_capacity t capacity =
    if capacity > t.max_capacity then
      Error Decoding_error
    else begin
      Dynamic_table.set_capacity t.table capacity;
      Ok ()
    end

  let get_indexed_field table index =
    let static_table_size = H2_hpack_tables.static_table_size in
    let dynamic_table_size = Dynamic_table.table_size table in
    if index = 0 || index > static_table_size + dynamic_table_size then
      Error Decoding_error
    else if index <= static_table_size then
      Ok H2_hpack_tables.static_table.(index - 1)
    else
      Ok (Dynamic_table.get table (index - static_table_size - 1))

  let decode_header_field table buf off prefix prefix_length =
    let* index, pos = decode_int buf (off + 1) prefix prefix_length in
    let* name, pos =
      if index = 0 then
        decode_string buf pos
      else
        let* name, _ = get_indexed_field table index in
        Ok (name, pos)
    in
    let* value, pos = decode_string buf pos in
    Ok ((name, value), pos)

  let decode t buf =
    let table = t.table in
    let len = Cstruct.length buf in

    (* Helper to decode an indexed header field (RFC 7541 Section 6.1) *)
    let decode_indexed b off =
      let* index, pos = decode_int buf (off + 1) b 7 in
      let* name, value = get_indexed_field table index in
      Ok ({ name; value; sensitive = false }, pos)
    in

    (* Helper to decode a literal header field *)
    let decode_literal ~add_to_table ~sensitive b prefix_length off =
      let* (name, value), pos = decode_header_field table buf off b prefix_length in
      if add_to_table then Dynamic_table.add table (name, value);
      Ok ({ name; value; sensitive }, pos)
    in

    let rec loop acc saw_first_header off =
      if off >= len then
        Ok (List.rev acc)
      else
        let b = Cstruct.get_uint8 buf off in
        if b land 0b1000_0000 <> 0 then begin
          (* Indexed Header Field - RFC 7541 Section 6.1 *)
          let* header, pos = decode_indexed b off in
          loop (header :: acc) true pos
        end
        else if b land 0b1100_0000 = 0b0100_0000 then begin
          (* Literal Header Field with Incremental Indexing - RFC 7541 Section 6.2.1 *)
          let* header, pos = decode_literal ~add_to_table:true ~sensitive:false b 6 off in
          loop (header :: acc) true pos
        end
        else if b land 0b1111_0000 = 0 then begin
          (* Literal Header Field without Indexing - RFC 7541 Section 6.2.2 *)
          let* header, pos = decode_literal ~add_to_table:false ~sensitive:false b 4 off in
          loop (header :: acc) true pos
        end
        else if b land 0b1111_0000 = 0b0001_0000 then begin
          (* Literal Header Field Never Indexed - RFC 7541 Section 6.2.3 *)
          let* header, pos = decode_literal ~add_to_table:false ~sensitive:true b 4 off in
          loop (header :: acc) true pos
        end
        else if b land 0b1110_0000 = 0b0010_0000 then begin
          (* Dynamic Table Size Update - RFC 7541 Section 6.3 *)
          if saw_first_header then
            Error Decoding_error
          else
            let* capacity, pos = decode_int buf (off + 1) b 5 in
            let* () = set_capacity t capacity in
            loop acc saw_first_header pos
        end
        else
          Error Decoding_error
    in
    loop [] false 0
end

(* ============================================================
   Encoder - RFC 7541 Section 6
   ============================================================ *)

module Encoder = struct
  module HeaderFieldsTbl = Hashtbl.Make(struct
    type t = string
    let equal = String.equal
    let hash = Hashtbl.hash
  end)

  module ValueMap = Map.Make(String)

  type t = {
    table : Dynamic_table.t;
    lookup_table : int ValueMap.t HeaderFieldsTbl.t;
    mutable next_seq : int;
  }

  let create capacity =
    { table = Dynamic_table.create capacity
    ; lookup_table = HeaderFieldsTbl.create capacity
    ; next_seq = 0
    }

  let set_capacity t new_capacity =
    Dynamic_table.set_capacity t.table new_capacity

  let add encoder (name, value) =
    Dynamic_table.add encoder.table (name, value);
    let map =
      match HeaderFieldsTbl.find_opt encoder.lookup_table name with
      | Some map -> ValueMap.add value encoder.next_seq map
      | None -> ValueMap.singleton value encoder.next_seq
    in
    encoder.next_seq <- encoder.next_seq + 1;
    HeaderFieldsTbl.replace encoder.lookup_table name map

  (* Binary format constants *)
  let never_indexed = 0b0001_0000, 4
  let without_indexing = 0b0000_0000, 4
  let incremental_indexing = 0b0100_0000, 6
  let indexed = 0b1000_0000, 7

  let[@inline] seq_to_index next_seq seq =
    H2_hpack_tables.static_table_size + next_seq - seq

  let is_without_indexing_set =
    let module IntSet = Set.Make(Int) in
    IntSet.of_list H2_hpack_tables.TokenIndices.[
      path; age; content_length; etag; if_modified_since;
      if_none_match; location; set_cookie
    ]

  let[@inline] is_without_indexing token =
    let module IntSet = Set.Make(Int) in
    token <> -1 && IntSet.mem token is_without_indexing_set

  let[@inline] is_sensitive token value =
    token <> -1 &&
    H2_hpack_tables.TokenIndices.(
      token = authorization || (token = cookie && String.length value < 20))

  let find_encoding encoder skip_indexing token name value =
    (* Search static table for matching name/value *)
    let rec loop i =
      if i >= H2_hpack_tables.static_table_size then begin
        (* Name matched but value didn't *)
        let index = token + 1 in
        if skip_indexing then
          without_indexing, index
        else begin
          add encoder (name, value);
          incremental_indexing, index
        end
      end else
        let name', value' = H2_hpack_tables.static_table.(i) in
        if name = name' then
          if value' = value then
            indexed, i + 1
          else
            loop (i + 1)
        else begin
          let index = token + 1 in
          if skip_indexing then
            without_indexing, index
          else begin
            add encoder (name, value);
            incremental_indexing, index
          end
        end
    in
    loop token

  let encode encoder header =
    let { name; value; sensitive } = header in
    let token = H2_hpack_tables.lookup_token_index name in
    let token_found = token <> -1 in
    if sensitive || is_sensitive token value then begin
      let index =
        if token_found then
          token + 1
        else
          match HeaderFieldsTbl.find_opt encoder.lookup_table name with
          | Some map ->
              let _, any_entry = ValueMap.choose map in
              seq_to_index encoder.next_seq any_entry
          | None -> 0
      in
      never_indexed, index
    end
    else if token_found then begin
      match HeaderFieldsTbl.find_opt encoder.lookup_table name with
      | Some map ->
          (match ValueMap.find_opt value map with
           | Some seq -> indexed, seq_to_index encoder.next_seq seq
           | None ->
               let skip_indexing = is_without_indexing token in
               find_encoding encoder skip_indexing token name value)
      | None ->
          let skip_indexing = is_without_indexing token in
          find_encoding encoder skip_indexing token name value
    end
    else begin
      match HeaderFieldsTbl.find_opt encoder.lookup_table name with
      | Some map ->
          (match ValueMap.find_opt value map with
           | Some seq -> indexed, seq_to_index encoder.next_seq seq
           | None ->
               let index = seq_to_index encoder.next_seq (snd (ValueMap.choose map)) in
               if is_without_indexing token then
                 without_indexing, index
               else begin
                 add encoder (name, value);
                 incremental_indexing, index
               end)
      | None ->
          if is_without_indexing token then
            without_indexing, 0
          else begin
            add encoder (name, value);
            incremental_indexing, 0
          end
    end

  let[@inline] is_indexed prefix = prefix = 128

  let encode_header encoder buf off header =
    let { name; value; _ } = header in
    let (prefix, prefix_length), index = encode encoder header in
    let len = encode_int buf off prefix prefix_length index in
    let off = off + len in
    if is_indexed prefix then
      len
    else begin
      let name_len =
        if index = 0 then
          encode_string buf off name
        else
          0
      in
      let off = off + name_len in
      let value_len = encode_string buf off value in
      len + name_len + value_len
    end

  let encode_headers encoder buf headers =
    List.fold_left (fun off header ->
      off + encode_header encoder buf off header
    ) 0 headers
end

(* ============================================================
   Constants
   ============================================================ *)

let default_table_size = 4096
