
module type S = sig
  val extract : ?salt:string -> string -> string
  val expand : prk:string -> ?info:string -> int -> string
end

module Make (H : Digestif.S) : S = struct
  let extract ?salt ikm =
    let key = match salt with
      | None -> String.make H.digest_size '\x00'
      | Some x -> x
    in
    H.(to_raw_string (hmac_string ~key ikm))

  let expand ~prk ?info len =
    if len < 0 then
      failwith "len must be non-negative"
    else if len > 255 * H.digest_size then
      failwith "len must be at most 255 * digest_size";
    let info = match info with
      | None -> ""
      | Some x -> x
    in
    let t n last =
      let nc = String.make 1 (Char.unsafe_chr n) in
      H.(to_raw_string (hmac_string ~key:prk (String.concat "" [last ; info ; nc])))
    in
    let n = (len + H.digest_size - 1) / H.digest_size in
    let rec compute acc count = match count, acc with
      | c, xs when c > n -> String.concat "" (List.rev xs)
      | c, x::_ -> compute (t c x :: acc) (succ c)
      | _, [] -> invalid_arg "can not happen"
    in
    let buf = compute [""] 1 in
    String.sub buf 0 len
end

let (extract @ portable) ~hash ?salt ikm =
  let key =
    match salt with
    | None -> String.make (Digestif.digest_size hash) '\x00'
    | Some salt -> salt
  in
  Digestif.hmacv_string_raw hash ~key [ ikm ]

let (expand @ portable) ~hash ~prk ?info len =
  let digest_size = Digestif.digest_size hash in
  if len < 0 then failwith "len must be non-negative"
  else if len > 255 * digest_size then
    failwith "len must be at most 255 * digest_size";
  let info = match info with None -> "" | Some info -> info in
  let blocks = (len + digest_size - 1) / digest_size in
  let rec compute acc previous counter =
    if counter > blocks then String.concat "" (List.rev acc)
    else
      let next =
        Digestif.hmacv_string_raw hash ~key:prk
          [ previous; info; String.make 1 (Char.unsafe_chr counter) ]
      in
      compute (next :: acc) next (counter + 1)
  in
  String.sub (compute [] "" 1) 0 len
