exception Malformed of string
let default_max_size = 16 * 1024 * 1024

let[@zero_alloc] length ?(max_size = default_max_size) (buf @ local) =
  if max_size < 2 || max_size > 268435460 then
    invalid_arg "MQTT maximum packet size must be 2..268435460";
  let n = Slice.length buf in
  if n = 0 then 0
  else
    let first = Slice.get_uint8 buf 0 in
    let kind = first lsr 4 and flags = first land 15 in
    if kind = 0 then raise (Malformed "reserved packet type");
    if kind = 3 then begin
      let qos = (flags lsr 1) land 3 in
      if qos = 3 || (qos = 0 && flags land 8 <> 0) then
        raise (Malformed "invalid PUBLISH flags")
    end else begin
      let expected = if kind = 6 || kind = 8 || kind = 10 then 2 else 0 in
      if flags <> expected then raise (Malformed "invalid fixed header flags")
    end;
    let rec loop (buf @ local) n max_size pos acc shift =
      if pos >= n then 0
      else
        let b = Slice.get_uint8 buf pos in
        let acc = acc lor ((b land 127) lsl shift) in
        if b land 128 <> 0 then begin
          if pos = 4 then raise (Malformed "remaining length exceeds 4 bytes");
          loop buf n max_size (pos + 1) acc (shift + 7)
        end else begin
          if pos > 1 && b = 0 then
            raise (Malformed "non-minimal remaining length");
          let total = pos + 1 + acc in
          if total > max_size then raise (Malformed "packet exceeds size limit");
          total
        end
    in
    loop buf n max_size 1 0 0
