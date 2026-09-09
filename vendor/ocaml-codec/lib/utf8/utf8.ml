let validate s =
  if String.is_valid_utf_8 s then Ok ()
  else
    (* Only reached on malformed input, to say where: walk to the first decode
       that fails. A valid prefix advances by the scalar it decoded; an invalid
       byte reports its own position. *)
    let n = String.length s in
    let rec first i =
      if i >= n then Ok ()
      else
        let d = String.get_utf_8_uchar s i in
        if Uchar.utf_decode_is_valid d then first (i + Uchar.utf_decode_length d)
        else Error i
    in
    first 0

(* Bjoern Hoehrmann's UTF-8 DFA
   (https://bjoern.hoehrmann.de/utf-8/decoder/dfa/). [byte_class] maps a byte to
   its character class and [trans] maps [state + class] to the next state: state
   0 accepts at a character boundary, state 12 rejects. The table refuses
   overlong encodings, surrogates and code points above U+10FFFF, so a byte
   stream is validated without ever materialising a code point, and without the
   whole stream having to be in hand at once. *)
module Stream = struct
  let byte_class =
    let a = Array.make 256 0 in
    for b = 0x80 to 0x8f do
      a.(b) <- 1
    done;
    for b = 0x90 to 0x9f do
      a.(b) <- 9
    done;
    for b = 0xa0 to 0xbf do
      a.(b) <- 7
    done;
    a.(0xc0) <- 8;
    a.(0xc1) <- 8;
    for b = 0xc2 to 0xdf do
      a.(b) <- 2
    done;
    a.(0xe0) <- 10;
    for b = 0xe1 to 0xec do
      a.(b) <- 3
    done;
    a.(0xed) <- 4;
    for b = 0xee to 0xef do
      a.(b) <- 3
    done;
    a.(0xf0) <- 11;
    for b = 0xf1 to 0xf3 do
      a.(b) <- 6
    done;
    a.(0xf4) <- 5;
    for b = 0xf5 to 0xff do
      a.(b) <- 8
    done;
    a

  (* The 108-entry transition table, nine states by twelve classes. *)
  let trans =
    [|
      0;
      12;
      24;
      36;
      60;
      96;
      84;
      12;
      12;
      12;
      48;
      72;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      0;
      12;
      12;
      12;
      12;
      12;
      0;
      12;
      0;
      12;
      12;
      12;
      24;
      12;
      12;
      12;
      12;
      12;
      24;
      12;
      24;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      24;
      12;
      12;
      12;
      12;
      12;
      24;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      24;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      36;
      12;
      36;
      12;
      12;
      12;
      36;
      12;
      12;
      12;
      12;
      12;
      36;
      12;
      36;
      12;
      12;
      12;
      36;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
      12;
    |]

  let accept = 0
  let reject = 12

  (* The two tables in one string: a byte an entry rather than a word, so the
     pair of lookups a byte costs stays in one cache line's worth of table, and
     no bounds check on an index the construction already bounds -- a byte
     indexes the 256 classes, and no state plus class exceeds 107. *)
  let table =
    let b = Bytes.create (256 + Array.length trans) in
    Array.iteri (fun i c -> Bytes.set b i (Char.chr c)) byte_class;
    Array.iteri (fun i c -> Bytes.set b (256 + i) (Char.chr c)) trans;
    Bytes.unsafe_to_string b

  (* [error] is an offset or [-1] rather than an [int option] because it is
     tested once per byte, and [= None] on an option is the polymorphic
     equality primitive: a C call per byte of every document validated. *)
  type t = {
    mutable state : int;
    mutable seq_start : int; (* offset of the sequence being decoded *)
    mutable count : int; (* bytes added so far *)
    mutable error : int;
  }

  let v () = { state = accept; seq_start = 0; count = 0; error = -1 }

  (* The automaton is only reached for bytes that need it. At a character
     boundary every byte below 0x80 is a complete, well-formed character, and
     that is most bytes of most documents, so a run of them is stepped over on
     one comparison each. Without that, validating a byte stream this way costs
     several times what the stdlib's whole-string check does, and a parser that
     reads through a window has no whole string to hand it. *)
  let add t b ~first ~length =
    let stop = first + length in
    let i = ref first in
    while t.error < 0 && !i < stop do
      if t.state = accept then begin
        let j = ref !i in
        while !j < stop && Char.code (Bytes.unsafe_get b !j) < 0x80 do
          incr j
        done;
        t.count <- t.count + (!j - !i);
        i := !j
      end;
      if !i < stop then begin
        let byte = Char.code (Bytes.unsafe_get b !i) in
        if t.state = accept then t.seq_start <- t.count;
        let cls = Char.code (String.unsafe_get table byte) in
        t.state <- Char.code (String.unsafe_get table (256 + t.state + cls));
        t.count <- t.count + 1;
        if t.state = reject then t.error <- t.seq_start;
        incr i
      end
    done;
    (* A rejected stream stops being read, so [count] must still account for
       the bytes it was handed: callers report offsets against it. *)
    if !i < stop then t.count <- t.count + (stop - !i)

  let finish t = if t.error < 0 && t.state <> accept then t.error <- t.seq_start
  let error t = if t.error < 0 then None else Some t.error
end
