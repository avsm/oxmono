(* SPDX-License-Identifier: ISC *)

type role = Client | Server
type kind = Text | Binary

let range b off len =
  off >= 0 && len >= 0 && off <= Bytes.length b - len

let byte b i = Char.code (Bytes.get b i)
let put b i n = Bytes.set b i (Char.chr (n land 255))

module Handshake = struct
  type headers = (string * string) list

  let token_char = function
      | 'a'..'z' | 'A'..'Z' | '0'..'9'
      | '!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-'
      | '.' | '^' | '_' | '`' | '|' | '~' -> true
      | _ -> false

  let token s =
    String.length s > 0 && String.for_all token_char s

  let fields name headers =
    List.filter_map (fun (k, v) ->
      if String.lowercase_ascii k = name then Some (String.trim v)
      else None) headers

  let one name headers =
    match fields name headers with [v] -> Some v | _ -> None

  let tokens name headers =
    List.concat_map (fun s ->
      List.map String.trim (String.split_on_char ',' s)) (fields name headers)

  let contains name value headers =
    let ts = tokens name headers in
    List.for_all token ts &&
    List.exists (fun s -> String.lowercase_ascii s = value) ts

  let valid_protocols ps =
    List.for_all token ps &&
    List.length (List.sort_uniq String.compare ps) = List.length ps

  (* RFC 6455 requires valid extension syntax even when declining the offer.
     A quoted parameter must unescape to a token, not an arbitrary string. *)
  let valid_extensions value =
    let length = String.length value in
    let local_ pos = ref 0 in
    let local_ whitespace () =
      while !pos < length && (value.[!pos] = ' ' || value.[!pos] = '\t') do
        incr pos
      done in
    let local_ word () =
      let start = !pos in
      while !pos < length && token_char value.[!pos] do
        incr pos
      done;
      !pos > start in
    let local_ parameter () =
      if !pos < length && value.[!pos] = '"' then begin
        incr pos;
        let mutable count = 0 in
        let mutable valid = true in
        while valid && !pos < length && value.[!pos] <> '"' do
          if value.[!pos] = '\\' then incr pos;
          if !pos >= length || not (token_char value.[!pos]) then
            valid <- false
          else begin incr pos; count <- count + 1 end
        done;
        if valid && count > 0 && !pos < length && value.[!pos] = '"' then
          (incr pos; true)
        else false
      end else word () in
    let mutable valid = true in
    let mutable more = true in
    while valid && more do
      whitespace ();
      valid <- word ();
      whitespace ();
      while valid && !pos < length && value.[!pos] = ';' do
        incr pos;
        whitespace ();
        valid <- word ();
        whitespace ();
        if valid && !pos < length && value.[!pos] = '=' then begin
          incr pos;
          whitespace ();
          valid <- parameter ();
          whitespace ()
        end
      done;
      if !pos < length && value.[!pos] = ',' then incr pos
      else more <- false
    done;
    valid && !pos = length

  let key_valid key =
    match Base64.decode key with
    | Ok s -> String.length s = 16 && Base64.encode_string s = key
    | Error _ -> false

  let answer key =
    Base64.encode_string
      (Digestif.SHA1.to_raw_string
         (Digestif.SHA1.digest_string
            (key ^ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))

  let request ?(protocols = []) ~nonce () =
    if String.length nonce <> 16 || not (valid_protocols protocols) then
      invalid_arg "WebSocket nonce or protocols";
    ["Upgrade", "websocket"; "Connection", "Upgrade";
     "Sec-WebSocket-Version", "13";
     "Sec-WebSocket-Key", Base64.encode_string nonce] @
    if protocols = [] then []
    else ["Sec-WebSocket-Protocol", String.concat ", " protocols]

  let accept ?protocol ~meth ~http_1_1 headers =
    let ps = tokens "sec-websocket-protocol" headers in
    let selected = match protocol with
      | None -> true
      | Some p -> token p && List.mem p ps
    in
    if meth <> "GET" || not http_1_1 then Error "expected HTTP/1.1 GET"
    else if not (contains "connection" "upgrade" headers) ||
            not (contains "upgrade" "websocket" headers) then
      Error "missing WebSocket upgrade"
    else if one "host" headers = None || one "host" headers = Some "" then
      Error "missing or duplicate Host"
    else if one "sec-websocket-version" headers <> Some "13" then
      Error "expected WebSocket version 13"
    else if not (valid_protocols ps) || not selected then
      Error "invalid subprotocol selection"
    else if not (List.for_all valid_extensions
                   (fields "sec-websocket-extensions" headers)) then
      Error "invalid extension offer"
    else match one "sec-websocket-key" headers with
      | Some key when key_valid key ->
          Ok (["Upgrade", "websocket"; "Connection", "Upgrade";
               "Sec-WebSocket-Accept", answer key] @
              match protocol with None -> []
              | Some p -> ["Sec-WebSocket-Protocol", p])
      | _ -> Error "invalid or duplicate WebSocket key"

  let verify ?(protocols = []) ~key ~status headers =
    let websocket_upgrade = match one "upgrade" headers with
      | Some value -> String.lowercase_ascii value = "websocket"
      | None -> false in
    if not (key_valid key) || not (valid_protocols protocols) then
      Error "invalid client handshake state"
    else if status <> 101 then Error "expected status 101"
    else if not (contains "connection" "upgrade" headers) ||
            not websocket_upgrade then
      Error "expected a single WebSocket upgrade"
    else if one "sec-websocket-accept" headers <> Some (answer key) then
      Error "invalid or duplicate WebSocket accept"
    else if fields "sec-websocket-extensions" headers <> [] then
      Error "unsolicited WebSocket extension"
    else match fields "sec-websocket-protocol" headers with
      | [] -> Ok None
      | [p] when token p && List.mem p protocols -> Ok (Some p)
      | _ -> Error "unsolicited or duplicate subprotocol"
end

module Frame = struct
  type opcode = Continuation | Text | Binary | Close | Ping | Pong
  type status = Complete | Partial | Malformed | Too_large
  type header =
    #{ fin : bool; opcode : opcode; length : int; masked : bool;
       mask : int; header_length : int }

  let number = function
    | Continuation -> 0 | Text -> 1 | Binary -> 2
    | Close -> 8 | Ping -> 9 | Pong -> 10

  let control = function Close | Ping | Pong -> true | _ -> false

  let[@zero_alloc] parse ~role ~max_payload b ~off ~len =
    let empty = #{ fin = false; opcode = Continuation; length = 0;
                   masked = false; mask = 0; header_length = 0 } in
    if not (range b off len) || max_payload < 0 then #(Malformed, empty)
    else if len < 2 then #(Partial, empty)
    else
      let first = byte b off and second = byte b (off + 1) in
      let fin = first land 128 <> 0 in
      let masked = second land 128 <> 0 in
      let code = first land 15 in
      let valid_opcode = code = 0 || code = 1 || code = 2 ||
                         code = 8 || code = 9 || code = 10 in
      let short = second land 127 in
      if first land 112 <> 0 || not valid_opcode ||
         masked <> (role = Server) || (code >= 8 && (not fin || short > 125))
      then #(Malformed, empty)
      else
        let extra = if short = 126 then 2 else if short = 127 then 8 else 0 in
        let size = 2 + extra + if masked then 4 else 0 in
        if len < size then #(Partial, empty)
        else if extra = 8 && byte b (off + 2) land 128 <> 0 then
          #(Malformed, empty)
        else
          let mutable length = if extra = 0 then short else 0 in
          let mutable too_large = length > max_payload in
          for i = 0 to extra - 1 do
            let n = byte b (off + 2 + i) in
            if n > max_payload || length > (max_payload - n) / 256 then
              too_large <- true
            else if not too_large then length <- length * 256 + n
          done;
          (* Check canonical length independently of the application limit. *)
          let nonminimal =
            (extra = 2 && byte b (off + 2) = 0 && byte b (off + 3) < 126) ||
            (extra = 8 && byte b (off + 2) = 0 && byte b (off + 3) = 0 &&
             byte b (off + 4) = 0 && byte b (off + 5) = 0 &&
             byte b (off + 6) = 0 && byte b (off + 7) = 0)
          in
          if nonminimal then #(Malformed, empty)
          else if too_large then #(Too_large, empty)
          else
            let opcode = match code with
              | 0 -> Continuation | 1 -> Text | 2 -> Binary
              | 8 -> Close | 9 -> Ping | _ -> Pong in
            let mutable mask = 0 in
            if masked then
              for i = 0 to 3 do
                mask <- mask * 256 + byte b (off + 2 + extra + i)
              done;
            #(Complete,
              #{ fin; opcode; length; masked; mask; header_length = size })

  let[@zero_alloc] write b ~off ~fin ~opcode ~length ~masked ~mask =
    if length < 0 || (control opcode && (length > 125 || not fin)) then
      invalid_arg "WebSocket frame length or fragmentation";
    let extra = if length < 126 then 0 else if length <= 65535 then 2 else 8 in
    let size = 2 + extra + if masked then 4 else 0 in
    if not (range b off size) then invalid_arg "WebSocket header buffer";
    put b off ((if fin then 128 else 0) lor number opcode);
    put b (off + 1) ((if masked then 128 else 0) lor
      (if extra = 0 then length else if extra = 2 then 126 else 127));
    for i = 0 to extra - 1 do
      put b (off + 2 + i) (length lsr (8 * (extra - 1 - i)))
    done;
    if masked then
      for i = 0 to 3 do
        put b (off + 2 + extra + i) (mask lsr (8 * (3 - i)))
      done;
    size

  let[@zero_alloc] mask b ~off ~len ~key ~offset =
    if not (range b off len) || offset < 0 then
      invalid_arg "WebSocket mask range";
    for i = 0 to len - 1 do
      let shift = 8 * (3 - ((offset + i) land 3)) in
      put b (off + i) (byte b (off + i) lxor (key lsr shift))
    done
end

(* Validate a slice without manufacturing a string or accepting surrogate,
   overlong or out-of-range encodings. Validation follows message assembly. *)
let valid_utf8 b off len =
  let stop = off + len in
  let mutable i = off in
  let mutable valid = true in
  while valid && i < stop do
    let c = byte b i in
    if c < 128 then i <- i + 1
    else begin
      let width = if c >= 194 && c <= 223 then 2
        else if c >= 224 && c <= 239 then 3
        else if c >= 240 && c <= 244 then 4 else 0 in
      if width = 0 || width > stop - i then valid <- false
      else begin
        let second = byte b (i + 1) in
        if (c = 224 && second < 160) || (c = 237 && second >= 160) ||
           (c = 240 && second < 144) || (c = 244 && second >= 144)
        then valid <- false;
        for j = 1 to width - 1 do
          let n = byte b (i + j) in
          if n < 128 || n > 191 then valid <- false
        done;
        i <- i + width
      end
    end
  done;
  valid

let valid_close_code = function
  | 1000 (* Normal closure *)
  | 1001 (* Going away *)
  | 1002 (* Protocol error *)
  | 1003 (* Unsupported data *)
  | 1007 (* Invalid payload *)
  | 1008 (* Policy violation *)
  | 1009 (* Message too big *)
  | 1010 (* Required extension missing, sent by clients only *)
  | 1011 (* Internal error *)
  | 1012 (* Service restart *)
  | 1013 (* Try again later *)
  | 1014 (* Bad gateway *) -> true
  | n -> n >= 3000 && n <= 4999

exception Protocol_error of int * string

type t = {
  role : role;
  max_message : int;
  max_fragments : int;
  random : (bytes -> off:int -> len:int -> unit);
  read : (bytes -> off:int -> len:int -> int);
  write : (bytes -> off:int -> len:int -> unit);
  with_write_lock : ((unit -> unit) @ local -> unit);
  rx_head : bytes;
  tx_head : bytes;
  control : bytes;
  scratch : bytes;
  mutable payload : bytes;
  mutable sent_close : bool;
  mutable received_close : bool;
  mutable failed : bool;
  mutable receiving : bool;
}

let no_random _ ~off:_ ~len:_ = invalid_arg "WebSocket client needs randomness"

let create ~role ?(max_message = 16 * 1024 * 1024) ?(max_fragments = 1024)
    ?(random : (bytes -> off:int -> len:int -> unit) = no_random)
    ~read ~write ~with_write_lock () =
  if max_message < 1 || max_fragments < 1 ||
     max_message > Sys.max_string_length ||
     (role = Client && random == no_random)
  then invalid_arg "WebSocket connection limits or randomness";
  { role; max_message; max_fragments; random; read; write; with_write_lock;
    rx_head = Bytes.create 14; tx_head = Bytes.create 14;
    control = Bytes.create 125; scratch = Bytes.create 4096;
    payload = Bytes.create (min 4096 max_message);
    sent_close = false; received_close = false;
    failed = false; receiving = false }

let fail t code reason =
  t.failed <- true;
  raise (Protocol_error (code, reason))

let check t = if t.failed then invalid_arg "WebSocket connection has failed"

let protect t (f @ local) =
  match f () with
  | v -> check t; v
  | exception e -> t.failed <- true; raise e

(* A callback can suspend while another fiber fails the connection. Recheck
   on return, including the last I/O, before using its result or doing more. *)
let exact t b off len =
  check t;
  let mutable pos = off in
  let stop = off + len in
  while pos < stop do
    let n = t.read b ~off:pos ~len:(stop - pos) in
    check t;
    if n = 0 then fail t 1006 "EOF without a close frame";
    if n < 0 || n > stop - pos then fail t 1002 "invalid transport read count";
    pos <- pos + n
  done

let write t b off len =
  check t;
  t.write b ~off ~len;
  check t

let write_frame t opcode b off len =
  check t;
  let masked = t.role = Client in
  let mutable key = 0 in
  if masked then begin
    t.random t.tx_head ~off:0 ~len:4;
    check t;
    for i = 0 to 3 do key <- key * 256 + byte t.tx_head i done
  end;
  let size = Frame.write t.tx_head ~off:0 ~fin:true ~opcode ~length:len
      ~masked ~mask:key in
  write t t.tx_head 0 size;
  if not masked then (if len > 0 then write t b off len)
  else begin
    let mutable pos = 0 in
    while pos < len do
      let count = min (Bytes.length t.scratch) (len - pos) in
      Bytes.blit b (off + pos) t.scratch 0 count;
      Frame.mask t.scratch ~off:0 ~len:count ~key ~offset:pos;
      write t t.scratch 0 count;
      pos <- pos + count
    done
  end

let send_frame t opcode b off len =
  check t;
  protect t (fun () -> t.with_write_lock (fun () ->
    check t;
    if t.sent_close || t.received_close then
      invalid_arg "WebSocket connection is closing";
    write_frame t opcode b off len))

let send t kind b ~off ~len =
  if not (range b off len) || len > t.max_message then
    invalid_arg "WebSocket message range or limit";
  if kind = Text && not (valid_utf8 b off len) then
    invalid_arg "WebSocket text must be UTF-8";
  let opcode = match kind with Text -> Frame.Text | Binary -> Frame.Binary in
  send_frame t opcode b off len

let ping t b ~off ~len =
  if not (range b off len) || len > 125 then
    invalid_arg "WebSocket ping length";
  send_frame t Frame.Ping b off len

let close t ?(code = 1000) ?(reason = "") () =
  if not (valid_close_code code) || String.length reason > 123 ||
     not (String.is_valid_utf_8 reason) || (code = 1010 && t.role = Server)
  then invalid_arg "WebSocket close code or reason";
  check t;
  protect t (fun () -> t.with_write_lock (fun () ->
    check t;
    if not t.sent_close && not t.received_close then begin
      let b = Bytes.create (2 + String.length reason) in
      put b 0 (code lsr 8); put b 1 code;
      Bytes.blit_string reason 0 b 2 (String.length reason);
      t.sent_close <- true;
      write_frame t Frame.Close b 0 (Bytes.length b)
    end))

let grow t length used =
  if length > Bytes.length t.payload then begin
    let capacity = max length (min t.max_message
      (Bytes.length t.payload + min (Bytes.length t.payload)
         (t.max_message - Bytes.length t.payload))) in
    let next = Bytes.create capacity in
    Bytes.blit t.payload 0 next 0 used;
    t.payload <- next
  end

let receive_inner t (f @ local) =
  let mutable used = 0 in
  let mutable fragments = 0 in
  let mutable kind = Binary in
  let mutable done_ = false in
  let mutable delivered = false in
  while not done_ do
    exact t t.rx_head 0 2;
    let short = byte t.rx_head 1 land 127 in
    let extra = (if short = 126 then 2 else if short = 127 then 8 else 0) +
      (if byte t.rx_head 1 land 128 <> 0 then 4 else 0) in
    let #(initial, _) = Frame.parse ~role:t.role
        ~max_payload:(max 125 t.max_message) t.rx_head ~off:0 ~len:2 in
    (match initial with
     | Frame.Malformed -> fail t 1002 "invalid frame header"
     | Frame.Too_large -> fail t 1009 "frame exceeds message limit"
     | Frame.Complete | Frame.Partial -> ());
    exact t t.rx_head 2 extra;
    let #(status, h) = Frame.parse ~role:t.role
        ~max_payload:(max 125 t.max_message)
        t.rx_head ~off:0 ~len:(2 + extra) in
    (match status with
     | Frame.Complete -> ()
     | Frame.Too_large -> fail t 1009 "frame exceeds message limit"
     | Frame.Malformed | Frame.Partial -> fail t 1002 "invalid frame header");
    match h.#opcode with
    | Frame.Close | Frame.Ping | Frame.Pong ->
        exact t t.control 0 h.#length;
        if h.#masked then
          Frame.mask t.control ~off:0 ~len:h.#length ~key:h.#mask ~offset:0;
        if h.#opcode = Frame.Close then begin
          if h.#length = 1 then fail t 1002 "close payload has one byte";
          let reply_length = if h.#length = 0 then 0 else begin
            let code = byte t.control 0 * 256 + byte t.control 1 in
            if not (valid_close_code code) || (code = 1010 && t.role = Client)
            then fail t 1002 "invalid close code";
            if not (valid_utf8 t.control 2 (h.#length - 2)) then
              fail t 1007 "invalid close reason";
            (* Reply with normal closure because 1010 and its extension
               list describe a client-side failure. *)
            if code = 1010 then begin
              put t.control 0 (1000 lsr 8); put t.control 1 1000;
              2
            end else h.#length
          end in
          t.with_write_lock (fun () ->
            check t;
            t.received_close <- true;
            if not t.sent_close then begin
              t.sent_close <- true;
              write_frame t Frame.Close t.control 0 reply_length
            end);
          done_ <- true
        end else if h.#opcode = Frame.Ping then
          t.with_write_lock (fun () ->
            check t;
            if not t.received_close then
              write_frame t Frame.Pong t.control 0 h.#length)
    | Frame.Continuation | Frame.Text | Frame.Binary ->
        if h.#opcode = Frame.Continuation then begin
          if fragments = 0 then fail t 1002 "continuation without a message"
        end else begin
          if fragments <> 0 then
            fail t 1002 "data frame inside fragmented message";
          kind <- if h.#opcode = Frame.Text then Text else Binary
        end;
        if fragments >= t.max_fragments || h.#length > t.max_message - used then
          fail t 1009 "message exceeds configured bounds";
        fragments <- fragments + 1;
        grow t (used + h.#length) used;
        exact t t.payload used h.#length;
        if h.#masked then
          Frame.mask t.payload ~off:used ~len:h.#length ~key:h.#mask ~offset:0;
        used <- used + h.#length;
        if h.#fin then begin
          if kind = Text && not (valid_utf8 t.payload 0 used) then
            fail t 1007 "invalid text UTF-8";
          if t.sent_close then begin
            used <- 0;
            fragments <- 0
          end else begin
            check t;
            f kind t.payload ~off:0 ~len:used;
            delivered <- true;
            done_ <- true
          end
        end
  done;
  delivered

let receive t ~f =
  check t;
  if t.receiving then invalid_arg "concurrent WebSocket receive";
  if t.received_close then false
  else begin
    t.receiving <- true;
    match protect t (fun () -> receive_inner t f) with
    | result -> t.receiving <- false; result
    | exception e -> t.receiving <- false; raise e
  end
