module H = Httpz.Header_name
module F64 = Stdlib_upstream_compatible.Float_u

type media = [ `Html | `Markdown | `Json | `Xml | `Other of string ]

let of_media s : media =
  match s with
  | "text/html" -> `Html
  | "text/markdown" -> `Markdown
  | "application/json" -> `Json
  | "application/xml" | "application/atom+xml" -> `Xml
  | other -> `Other other

let to_media (m : media) =
  match m with
  | `Html -> "text/html"
  | `Markdown -> "text/markdown"
  | `Json -> "application/json"
  | `Xml -> "application/xml"
  | `Other other -> other

(* Accept is read in place. A member is [range;params] between commas, its
   range trimmed and compared without case, its quality the last [q]
   parameter. *)

let[@zero_alloc] is_ows c = Char.equal c ' ' || Char.equal c '\t'

let[@zero_alloc] rec skip_ows (v : string @ local) i j =
  if i < j && is_ows (String.unsafe_get v i) then skip_ows v (i + 1) j else i

let[@zero_alloc] rec trim_ows (v : string @ local) i j =
  if j > i && is_ows (String.unsafe_get v (j - 1)) then trim_ows v i (j - 1)
  else j

let[@zero_alloc] rec index_from (v : string @ local) i j c =
  if i >= j then j
  else if Char.equal (String.unsafe_get v i) c then i
  else index_from v (i + 1) j c

(* Separators inside a quoted parameter value are data. Quoted-pair escapes
   keep the next byte opaque as well. Keep the recursive worker top-level so
   it does not allocate a closure over the local input. *)
let[@zero_alloc] rec index_unquoted_loop
    (v : string @ local) i j wanted quoted escaped =
  if i >= j then j
  else
    let c = String.unsafe_get v i in
    if escaped then index_unquoted_loop v (i + 1) j wanted quoted false
    else if quoted && Char.equal c '\\' then
      index_unquoted_loop v (i + 1) j wanted quoted true
    else if Char.equal c '"' then
      index_unquoted_loop v (i + 1) j wanted (not quoted) false
    else if (not quoted) && Char.equal c wanted then i
    else index_unquoted_loop v (i + 1) j wanted quoted false

let[@zero_alloc] index_unquoted_from (v : string @ local) i j wanted =
  index_unquoted_loop v i j wanted false false

let[@zero_alloc] lower c = Char.lowercase_ascii c

let[@zero_alloc] rec all_tchar (s : string @ local) i j =
  Httpz.Header.Syntax.is_token_sub s ~pos:i ~len:(j - i)

(* RFC 9110 section 12.5.1 assigns each representation the quality of its most
   specific matching range. [specificity] is [-1] for no match, [0] for [*/*],
   [1] for [type/*] and [2] otherwise, so a specific q=0 is not erased by an
   acceptable wildcard. Suffix ranges such as [*+json] are as specific as a
   full subtype. *)
let[@zero_alloc] specificity (range : string @ local) a b (media : string) =
  Httpz.Media.Syntax.specificity ~range ~pos:a ~len:(b - a) media

(* A qvalue is at most three decimals of a number between zero and one, with
   no sign, exponent, or other spelling, per RFC 9110 section 12.4.2. A member
   whose q is anything else is unusable rather than merely ill-ranked, since
   accepting it would let [q=2] or [q=inf] outrank everything a client asked
   for. [valid] is [false] for such a member. *)
let[@zero_alloc] qvalue (s : string @ local) a b : #(bool * float#) =
  let thousandths = Httpz.Header.Syntax.qvalue_sub s ~pos:a ~len:(b - a) in
  if thousandths < 0 then #(false, #0.)
  else #(true, F64.div (F64.of_int thousandths) #1000.)

let[@zero_alloc] valid_param_value (s : string @ local) a b =
  let len = b - a in
  Httpz.Header.Syntax.is_token_sub s ~pos:a ~len
  || Httpz.Header.Syntax.is_quoted_string_sub s ~pos:a ~len

(* The last [q] parameter wins, and a malformed one makes the member
   unusable. *)
let[@zero_alloc] rec quality (v : string @ local) i stop ~valid (q : float#) =
  if i > stop then #(valid, q)
  else
    let semi = index_unquoted_from v i stop ';' in
    let eq = index_from v i semi '=' in
    let #(valid, q) =
      if eq = semi then #(false, q)
      else
        let ka = skip_ows v i eq in
        let kb = trim_ows v ka eq in
        if kb <= ka || not (all_tchar v ka kb) then #(false, q)
        else if kb - ka = 1 && Char.equal (lower (String.unsafe_get v ka)) 'q' then
          if not valid then #(false, q)
          else
            let va = skip_ows v (eq + 1) semi in
            let vb = trim_ows v va semi in
            qvalue v va vb
        else
          let va = skip_ows v (eq + 1) semi in
          let vb = trim_ows v va semi in
          #(valid && valid_param_value v va vb, q)
    in
    quality v (semi + 1) stop ~valid q

(* One member of a value, between [start] and [stop]. [order] counts usable
   members across every Accept field so ties break in the client's order.
   The state is the best specificity so far, its quality and its order. *)
let[@zero_alloc] member (v : string @ local) start stop media ~order
    (state : #(int * float# * int)) =
  let semi = index_unquoted_from v start stop ';' in
  let a = skip_ows v start semi in
  let b = trim_ows v a semi in
  if b <= a then #(order, state)
  else
    let #(valid, q) = quality v (semi + 1) stop ~valid:true #1. in
    if not valid then #(order, state)
    else
      let #(best, _, _) = state in
      let current = specificity v a b media in
      if current > best then #(order + 1, #(current, q, order))
      else #(order + 1, state)

let[@zero_alloc] rec members (v : string @ local) start media ~order state =
  let n = String.length v in
  if start > n then #(order, state)
  else
    let stop = index_unquoted_from v start n ',' in
    let #(order, state) = member v start stop media ~order state in
    members v (stop + 1) media ~order state

(* Every Accept field takes part, since repeated fields combine. *)
let[@zero_alloc] rec preference (t : Headers.t @ local) media ~order state =
  match t with
  | [] -> state
  | f :: tl ->
      if Headers.same_name f.Headers.name H.Accept then
        let #(order, state) = members f.Headers.value 0 media ~order state in
        preference tl media ~order state
      else preference tl media ~order state

let[@zero_alloc] better ~found (q : float#) (range_order : int) (best_q : float#)
    (best_range : int) =
  F64.compare q #0. > 0
  && ((not found) || F64.compare q best_q > 0
     || (F64.equal q best_q && range_order < best_range))

(* The two drivers differ only in how a variant names its media type. *)
let[@zero_alloc] rec choose_media (headers : Headers.t @ local) variants ~found
    (best_q : float#) best_range best =
  match variants with
  | [] -> if found then This best else Null
  | (media, h) :: rest ->
      let #(spec, q, range_order) =
        preference headers (to_media media) ~order:0 #(-1, #0., max_int)
      in
      if spec >= 0 && better ~found q range_order best_q best_range then
        choose_media headers rest ~found:true q range_order h
      else choose_media headers rest ~found best_q best_range best

let[@zero_alloc] rec choose_codec (headers : Headers.t @ local) codecs ~found
    (best_q : float#) best_range best =
  match codecs with
  | [] -> if found then This best else Null
  | codec :: rest ->
      let #(spec, q, range_order) =
        preference headers (Httpz.Media.media_type codec) ~order:0
          #(-1, #0., max_int)
      in
      if spec >= 0 && better ~found q range_order best_q best_range then
        choose_codec headers rest ~found:true q range_order codec
      else choose_codec headers rest ~found best_q best_range best

(* [of_accept] builds a list, so it works on a heap copy. *)
let rec split_on (s : string) c i acc =
  let n = String.length s in
  let j = index_unquoted_from s i n c in
  if j = n then List.rev (String.sub s i (n - i) :: acc)
  else split_on s c (j + 1) (String.sub s i (j - i) :: acc)

let parse_one s =
  let n = String.length s in
  let semi = index_from s 0 n ';' in
  let a = skip_ows s 0 semi in
  let b = trim_ows s a semi in
  if not (Httpz.Media.Syntax.valid_range s ~pos:a ~len:(b - a)) then None
  else
    let #(valid, q) = quality s (semi + 1) n ~valid:true #1. in
    if not valid then None
    else Some (String.lowercase_ascii (String.sub s a (b - a)), F64.to_float q)

let of_accept (accept : string option @ local) =
  match accept with
  | None -> []
  | Some accept ->
      split_on (Pct.copy_all accept) ',' 0 []
      |> List.filter_map parse_one
      (* The sort is stable, so two types the client gave the same q keep the
         order it wrote them in, which is the order it prefers them in. *)
      |> List.stable_sort (fun (_, a) (_, b) -> Float.compare b a)
      |> List.filter_map (fun (m, q) -> if q <= 0. then None else Some (of_media m))

(* RFC 9110 section 15.5.7: a client that stated what it accepts and cannot be
   served is told so, rather than handed a representation it did not ask for.
   The body lists what is on offer so the client can pick again. *)
let[@cold] not_acceptable types (respond : Resp.respond @ local) =
  let body =
    String.concat "" (List.map (fun t -> t ^ "\n") ("Not Acceptable" :: types))
  in
  let local_ headers = Headers.vary Headers.empty "Accept" in
  let () = Resp.text respond ~status:Httpz.Res.Not_acceptable ~headers body in
  ()

(* Negotiated responses must vary on Accept for caches to distinguish them. *)
let v variants env (req : Req.t @ local) (respond : Resp.respond @ local) =
    match variants with
    | [] -> Resp.not_found respond ()
    | (_, first) :: _ -> (
        let headers = Req.headers req in
        let chosen =
          if not (Headers.mem headers H.Accept) then This first
          else choose_media headers variants ~found:false #0. max_int first
        in
        match chosen with
        | Null ->
            not_acceptable (List.map (fun (m, _) -> to_media m) variants) respond
        | This h ->
            let local_ varying : Resp.respond =
             fun d ->
              let local_ d =
                { d with Resp.headers = Headers.vary d.Resp.headers "Accept" }
              in
              let () = respond d in
              ()
            in
            let () = h env req varying in
            ())

let select_opt codecs (req : Req.t @ local) =
  match codecs with
  | [] -> invalid_arg "Proffer.Negotiate.select_opt: no codecs"
  | first :: _ -> (
      let headers = Req.headers req in
      if not (Headers.mem headers H.Accept) then Some first
      else
        match choose_codec headers codecs ~found:false #0. max_int first with
        | This c -> Some c
        | Null -> None)

let[@zero_alloc] select codecs (req : Req.t @ local) =
  match codecs with
  | [] -> invalid_arg "Proffer.Negotiate.select: no codecs"
  | first :: _ -> (
      let headers = Req.headers req in
      if not (Headers.mem headers H.Accept) then first
      else
        match choose_codec headers codecs ~found:false #0. max_int first with
        | This c -> c
        | Null -> first)

let encode ?status ?(etag : Etag.t option @ local)
    ?(cache : Cache_control.t option @ local)
    ?(headers : Headers.t @ local = Headers.empty)
    (respond : Resp.respond @ local) (req : Req.t @ local) codecs x =
  let chosen =
    match codecs with
    | [] -> invalid_arg "Proffer.Negotiate.encode: no codecs"
    | first :: _ ->
        let request_headers = Req.headers req in
        if not (Headers.mem request_headers H.Accept) then This first
        else
          choose_codec request_headers codecs ~found:false #0. max_int first
  in
  match chosen with
  | This codec ->
      let local_ headers = Headers.vary headers "Accept" in
      let () = Resp.encode respond ?status ?etag ?cache ~headers codec x in
      ()
  | Null ->
      let () = not_acceptable (List.map Httpz.Media.media_type codecs) respond in
      ()
