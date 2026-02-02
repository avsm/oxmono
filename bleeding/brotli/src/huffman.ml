(* Canonical Huffman coding with 2-level lookup tables for Brotli *)

let max_length = 15

(* A Huffman code entry in the lookup table *)
type code = {
  bits : int;   (* Number of bits used for this symbol, or bits in subtable *)
  value : int;  (* Symbol value, or offset to subtable *)
}

(* A Huffman lookup table - flat array with 2-level structure *)
type table = code array

exception Invalid_huffman_tree

(* Returns reverse(reverse(key, len) + 1, len) for canonical code generation *)
let get_next_key key length =
  let rec loop step =
    if key land step = 0 then
      (key land (step - 1)) + step
    else
      loop (step lsr 1)
  in
  loop (1 lsl (length - 1))

(* Store code in table[i], table[i+step], table[i+2*step], ... *)
let replicate_value table base step table_end code =
  let rec loop index =
    if index >= base then begin
      table.(index) <- code;
      loop (index - step)
    end
  in
  loop (base + table_end - step)

(* Calculate the width of the next 2nd level table *)
let next_table_bit_size count length root_bits =
  let left = ref (1 lsl (length - root_bits)) in
  let len = ref length in
  while !len < max_length do
    left := !left - count.(!len);
    if !left <= 0 then
      len := max_length  (* Break *)
    else begin
      incr len;
      left := !left lsl 1
    end
  done;
  !len - root_bits

(* Build a Huffman lookup table from code lengths *)
let build_table ~code_lengths ~alphabet_size ~root_bits =
  let count = Array.make (max_length + 1) 0 in
  let offset = Array.make (max_length + 1) 0 in
  let sorted_symbols = Array.make alphabet_size 0 in

  (* Build histogram of code lengths *)
  for symbol = 0 to alphabet_size - 1 do
    let len = code_lengths.(symbol) in
    count.(len) <- count.(len) + 1
  done;

  (* Generate offsets into sorted symbol table by code length *)
  offset.(1) <- 0;
  for length = 1 to max_length - 1 do
    offset.(length + 1) <- offset.(length) + count.(length)
  done;

  (* Sort symbols by length, by symbol order within each length *)
  for symbol = 0 to alphabet_size - 1 do
    let length = code_lengths.(symbol) in
    if length <> 0 then begin
      sorted_symbols.(offset.(length)) <- symbol;
      offset.(length) <- offset.(length) + 1
    end
  done;

  let table_bits = ref root_bits in
  let table_size = ref (1 lsl !table_bits) in
  let total_size = ref !table_size in

  (* Pre-allocate table with maximum possible size *)
  let max_table_size = !table_size * 4 in  (* Conservative estimate *)
  let root_table = Array.make max_table_size { bits = 0; value = 0 } in

  (* Special case: code with only one value *)
  if offset.(max_length) = 1 then begin
    for key = 0 to !total_size - 1 do
      root_table.(key) <- { bits = 0; value = sorted_symbols.(0) land 0xFFFF }
    done;
    Array.sub root_table 0 !total_size
  end
  else begin
    let table = ref 0 in
    let key = ref 0 in
    let symbol = ref 0 in
    let step = ref 2 in

    (* Fill in root table *)
    for length = 1 to root_bits do
      while count.(length) > 0 do
        let code = { bits = length land 0xFF; value = sorted_symbols.(!symbol) land 0xFFFF } in
        incr symbol;
        replicate_value root_table (!table + !key) !step !table_size code;
        key := get_next_key !key length;
        count.(length) <- count.(length) - 1
      done;
      step := !step lsl 1
    done;

    (* Fill in 2nd level tables and add pointers to root table *)
    let mask = !total_size - 1 in
    let low = ref (-1) in
    step := 2;
    let start_table = 0 in

    for length = root_bits + 1 to max_length do
      while count.(length) > 0 do
        if (!key land mask) <> !low then begin
          table := !table + !table_size;
          table_bits := next_table_bit_size count length root_bits;
          table_size := 1 lsl !table_bits;
          total_size := !total_size + !table_size;
          low := !key land mask;
          root_table.(start_table + !low) <- {
            bits = (!table_bits + root_bits) land 0xFF;
            value = (!table - start_table - !low) land 0xFFFF
          }
        end;
        let code = { bits = (length - root_bits) land 0xFF; value = sorted_symbols.(!symbol) land 0xFFFF } in
        incr symbol;
        replicate_value root_table (!table + (!key lsr root_bits)) !step !table_size code;
        key := get_next_key !key length;
        count.(length) <- count.(length) - 1
      done;
      step := !step lsl 1
    done;

    Array.sub root_table 0 !total_size
  end

(* Read a symbol from the bit stream using the Huffman table *)
let[@inline] read_symbol table root_bits br =
  let bits = Bit_reader.peek_bits br 15 in
  let initial_idx = bits land ((1 lsl root_bits) - 1) in
  let entry = table.(initial_idx) in
  if entry.bits <= root_bits then begin
    (* Symbol found in root table *)
    Bit_reader.skip_bits br entry.bits;
    entry.value
  end
  else begin
    (* Need to look in 2nd level table *)
    Bit_reader.skip_bits br root_bits;
    let extra_bits = entry.bits - root_bits in
    let idx2 = (bits lsr root_bits) land ((1 lsl extra_bits) - 1) in
    let entry2 = table.(initial_idx + entry.value + idx2) in
    Bit_reader.skip_bits br entry2.bits;
    entry2.value
  end

(* Build Huffman table for simple prefix codes (1-4 symbols) *)
let build_simple_table symbols num_symbols =
  let table_size = 1 lsl Constants.huffman_max_table_bits in
  let table = Array.make table_size { bits = 0; value = 0 } in

  match num_symbols with
  | 1 ->
    (* Single symbol - use 0 bits *)
    for i = 0 to table_size - 1 do
      table.(i) <- { bits = 0; value = symbols.(0) }
    done;
    table
  | 2 ->
    (* Two symbols - 1 bit each *)
    let half = table_size / 2 in
    for i = 0 to half - 1 do
      table.(i) <- { bits = 1; value = symbols.(0) }
    done;
    for i = half to table_size - 1 do
      table.(i) <- { bits = 1; value = symbols.(1) }
    done;
    table
  | 3 ->
    (* Three symbols: 1, 2, 2 bits *)
    let quarter = table_size / 4 in
    for i = 0 to quarter - 1 do
      table.(i) <- { bits = 1; value = symbols.(0) }
    done;
    for i = quarter to 2 * quarter - 1 do
      table.(i) <- { bits = 2; value = symbols.(1) }
    done;
    for i = 2 * quarter to table_size - 1 do
      table.(i) <- { bits = 2; value = symbols.(2) }
    done;
    table
  | 4 ->
    (* Four symbols: 2 bits each, with tree-select bit *)
    let quarter = table_size / 4 in
    for i = 0 to quarter - 1 do
      table.(i) <- { bits = 2; value = symbols.(0) }
    done;
    for i = quarter to 2 * quarter - 1 do
      table.(i) <- { bits = 2; value = symbols.(1) }
    done;
    for i = 2 * quarter to 3 * quarter - 1 do
      table.(i) <- { bits = 2; value = symbols.(2) }
    done;
    for i = 3 * quarter to table_size - 1 do
      table.(i) <- { bits = 2; value = symbols.(3) }
    done;
    table
  | _ ->
    raise Invalid_huffman_tree

(* Maximum table sizes for different alphabet sizes *)
let max_table_sizes = [|
  256; 402; 436; 468; 500; 534; 566; 598;
  630; 662; 694; 726; 758; 790; 822; 854;
  886; 918; 950; 982; 1014; 1046; 1078; 1080
|]

(* Get maximum table size for a given alphabet size *)
let max_table_size alphabet_size =
  if alphabet_size <= 256 then 256
  else if alphabet_size <= 704 then 1080
  else 2048  (* Large alphabets *)
