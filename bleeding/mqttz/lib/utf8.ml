(* MQTT-1.5.3: shortest-form UTF-8, no surrogates and no U+0000.
   Noncharacters and control characters are discouraged, but not forbidden. *)
let[@zero_alloc] valid s =
  let n = String.length s in
  let rec loop s n i =
    if i = n then true
    else
      let a = Char.code s.[i] in
      if a = 0 then false
      else if a < 0x80 then loop s n (i + 1)
      else
        let width =
          if a >= 0xc2 && a <= 0xdf then 2
          else if a >= 0xe0 && a <= 0xef then 3
          else if a >= 0xf0 && a <= 0xf4 then 4
          else 0
        in
        if width = 0 || width > n - i then false
        else
          let b = Char.code s.[i + 1] in
          let rec continuation j =
            j = width ||
            (let c = Char.code s.[i + j] in
             c >= 0x80 && c <= 0xbf && continuation (j + 1))
          in
          b >= 0x80 && b <= 0xbf
          && (a <> 0xe0 || b >= 0xa0)
          && (a <> 0xed || b <= 0x9f)
          && (a <> 0xf0 || b >= 0x90)
          && (a <> 0xf4 || b <= 0x8f)
          && continuation 2 && loop s n (i + width)
  in
  loop s n 0

let[@zero_alloc] valid_payload (s : Slice.t @ local) =
  let n = s.len in
  let rec loop (s : Slice.t @ local) n i =
    if i = n then true
    else
      let a = Bytes.get_uint8 s.bytes (s.off + i) in
      if a < 0x80 then loop s n (i + 1)
      else
        let width =
          if a >= 0xc2 && a <= 0xdf then 2
          else if a >= 0xe0 && a <= 0xef then 3
          else if a >= 0xf0 && a <= 0xf4 then 4
          else 0
        in
        if width = 0 || width > n - i then false
        else
          let b = Bytes.get_uint8 s.bytes (s.off + i + 1) in
          let rec continuation j =
            j = width ||
            (let c = Bytes.get_uint8 s.bytes (s.off + i + j) in
             c >= 0x80 && c <= 0xbf && continuation (j + 1))
          in
          b >= 0x80 && b <= 0xbf
          && (a <> 0xe0 || b >= 0xa0)
          && (a <> 0xed || b <= 0x9f)
          && (a <> 0xf0 || b >= 0x90)
          && (a <> 0xf4 || b <= 0x8f)
          && continuation 2 && loop s n (i + width)
  in
  loop s n 0

let[@zero_alloc] valid_payload_string s =
  (* The temporary alias is read-only and cannot leave this validation call. *)
  let local_ view = Slice.make_local (Bytes.unsafe_of_string s)
      ~off:0 ~len:(String.length s) in
  let valid = valid_payload view in
  valid
