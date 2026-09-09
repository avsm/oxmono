(* A bounded cursor. Subreaders and payloads borrow the original bytes. *)
type reader = { global_ data : Slice.t; mutable pos : int; limit : int }
type writer = Buffer.t

let of_slice data = exclave_ { data; pos = 0; limit = Slice.length data }
let[@zero_alloc] remaining (r @ local) = r.limit - r.pos
let[@zero_alloc] require (r @ local) n =
  if n < 0 || n > remaining r then invalid_arg "truncated MQTT field"
let[@zero_alloc] uint8 (r @ local) =
  require r 1;
  let v = Slice.get_uint8 r.data r.pos in
  r.pos <- r.pos + 1;
  v
let[@zero_alloc] uint16_be (r @ local) =
  require r 2;
  let v = Bytes.get_uint16_be r.data.bytes (r.data.off + r.pos) in
  r.pos <- r.pos + 2;
  v
let uint32_be (r @ local) =
  require r 4;
  let v = Bytes.get_int32_be r.data.bytes (r.data.off + r.pos) in
  r.pos <- r.pos + 4;
  v
let take n (r @ local) =
  require r n;
  let s = Bytes.sub_string r.data.bytes (r.data.off + r.pos) n in
  r.pos <- r.pos + n;
  s
let sub n (r @ local) = exclave_
  require r n;
  let s = { data = r.data; pos = r.pos; limit = r.pos + n } in
  r.pos <- r.pos + n;
  s
let mqtt_binary (r @ local) = let n = uint16_be r in take n r
let mqtt_string (r @ local) =
  let s = mqtt_binary r in
  if not (Utf8.valid s) then invalid_arg "invalid MQTT UTF-8 string";
  s
let[@zero_alloc] variable_length (r @ local) =
  let rec loop (r @ local) count shift acc =
    let b = uint8 r in
    let v = acc lor ((b land 127) lsl shift) in
    if b land 128 = 0 then begin
      if count > 0 && b = 0 then invalid_arg "non-minimal variable integer";
      v
    end else if count = 3 then invalid_arg "variable integer exceeds 4 bytes"
    else loop r (count + 1) (shift + 7) v
  in
  loop r 0 0 0
let take_rest (r @ local) =
  let v = Slice.sub r.data r.pos (remaining r) in
  r.pos <- r.limit;
  v
let is_eod (r @ local) = remaining r = 0
let finish (r @ local) = if not (is_eod r) then invalid_arg "trailing MQTT packet bytes"
let bool (r @ local) =
  match uint8 r with 0 -> false | 1 -> true
  | _ -> invalid_arg "MQTT boolean must be 0 or 1"
let write_uint8 w n =
  if n < 0 || n > 255 then invalid_arg "uint8 out of range";
  Buffer.add_char w (Char.chr n)
let write_uint16_be w n =
  if n < 0 || n > 65535 then invalid_arg "uint16 out of range";
  write_uint8 w (n lsr 8); write_uint8 w (n land 255)
let write_uint32_be w n =
  for shift = 3 downto 0 do
    write_uint8 w (Int32.to_int (Int32.shift_right_logical n (shift * 8)) land 255)
  done
let write_string = Buffer.add_string
let write_mqtt_binary w s =
  write_uint16_be w (String.length s); write_string w s
let write_mqtt_string w s =
  if not (Utf8.valid s) then invalid_arg "invalid MQTT UTF-8 string";
  write_mqtt_binary w s
let write_variable_length w n =
  if n < 0 || n > 268435455 then invalid_arg "variable integer out of range";
  let rec loop n =
    if n < 128 then write_uint8 w n
    else (write_uint8 w ((n land 127) lor 128); loop (n lsr 7))
  in loop n
let rec read_many (f : reader @ local -> 'a) (r @ local) acc =
  if is_eod r then List.rev acc
  else let v = f r in read_many f r (v :: acc)
let many f (r @ local) = read_many f r []
let many1 f (r @ local) = let v = f r in v :: many f r
let count n (f : reader @ local -> 'a) (r @ local) =
  if n <= 0 then invalid_arg "empty MQTT code list";
  let rec loop (r @ local) acc n =
    if n = 0 then List.rev acc else let v = f r in loop r (v :: acc) (n - 1)
  in loop r [] n
let write_fixed_header w kind flags size =
  write_uint8 w ((Shared.Packet_type.to_int kind lsl 4) lor flags);
  write_variable_length w size
let to_string f = let w = Buffer.create 128 in f w; Buffer.contents w
let check condition message = if not condition then invalid_arg message
let packet_id id = check (id > 0 && id <= 65535) "invalid packet identifier"
let topic s = check (Shared.Topic.Name.validate s) "invalid topic name"
let filter s = check (Shared.Topic.Filter.validate s) "invalid topic filter"
let connect_flags flags =
  check (flags land 1 = 0) "reserved CONNECT flag";
  check (flags land 4 <> 0 || flags land 0x38 = 0) "Will flags without Will";
  check (flags land 0x18 <> 0x18) "invalid Will QoS"

let decode (read : reader @ local -> 'a) ?(max_size = Frame.default_max_size) data =
  try
    let n = Frame.length ~max_size data in
    check (n > 0 && n = Slice.length data) "incomplete or trailing MQTT frame";
    let local_ reader = of_slice data in
    Ok (read reader)
  with
  | Invalid_argument message | Frame.Malformed message -> Error message
