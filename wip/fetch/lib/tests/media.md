# Typed bodies

A `Media.t` codec pairs a media type with an encoder and decoder for one
OCaml type. `Fetch.encode` builds a request from a value, `Fetch.decode`
reads a response into one, and `get_as` and `read_as` combine a GET with
decoding.

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch

(* A codec with a string form, standing in for JSON. *)
let pair : (string * string) Media.t =
  Media.of_strings "application/x-pair"
    ~encode:(fun (a, b) -> a ^ "=" ^ b)
    ~decode:(fun s ->
      match String.index_opt s '=' with
      | Some i -> Ok (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))
      | None -> Error "no equals sign")

let pairs = Media.lines "text/x-pairs" pair

let with_type ?(status = 200) ct body req =
  Fetch_mock.respond ~status ~headers:(Http.Header.of_list [ ("Content-Type", ct) ]) body req

let drain = function
  | Empty -> ""
  | String s -> s
  | Stream { flow; _ } -> Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:10_000 flow)

let server (req : Middleware.request) =
  Fmt.pr "> %s %s accept=%s@." (Http.Method.to_string req.meth)
    (Middleware.Url.path_and_query req.url)
    (Option.value (Http.Header.get req.headers "accept") ~default:"-");
  match Middleware.Url.path_segments req.url with
  | [ "one" ] -> with_type "application/x-pair; v=1" "a=1" req
  | [ "missing" ] -> with_type ~status:404 "text/plain" "no such pair" req
  | [ "html" ] -> with_type "text/html" "<p>hi</p>" req
  | [ "broken" ] -> with_type "application/x-pair" "nope" req
  | [ "many" ] -> with_type "text/x-pairs" "a=1\nb=2\n" req
  | [ "echo" ] ->
    Fmt.pr ">   content-type=%s body=%S@."
      (Option.value (Http.Header.get req.headers "content-type") ~default:"-")
      (drain req.body);
    with_type "application/x-pair" "ok=1" req
  | _ -> Fetch_mock.respond ~status:404 "" req

let client = Fetch_mock.client server
let run f = Eio_mock.Backend.run f
```

## A 2xx decodes, and Accept names the codec

```ocaml
# run @@ fun () -> Fetch.read_as client pair "https://x/one";;
> GET /one accept=application/x-pair
- : (string * string, response) result = Ok ("a", "1")
```

## Any other status is the response itself

The body has been buffered, so it can still be read, or decoded with a
codec for the error representation:

```ocaml
# run @@ fun () ->
  match Fetch.read_as client pair "https://x/missing" with
  | Ok _ -> assert false
  | Error r -> (status r, Fetch.decode Media.text r);;
> GET /missing accept=application/x-pair
- : int * string = (404, "no such pair")
```

`expect` turns that into an exception for callers that want one:

```ocaml
# run @@ fun () ->
  try
    ignore (Fetch.expect (Fetch.read_as client pair "https://x/missing"));
    assert false
  with Fetch.Rejected r -> status r;;
> GET /missing accept=application/x-pair
- : int = 404
```

## A body the codec cannot read raises, like any protocol failure

```ocaml
# run @@ fun () -> Fetch.read_as client pair "https://x/html";;
> GET /html accept=application/x-pair
Exception:
Eio.Io Http Decode_failure expected application/x-pair, unsupported media type "text/html",
  reading https://x/html
# run @@ fun () -> Fetch.read_as client pair "https://x/broken";;
> GET /broken accept=application/x-pair
Exception:
Eio.Io Http Decode_failure expected application/x-pair, malformed body: no equals sign,
  reading https://x/broken
```

## Sending a value

```ocaml
# run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let headers, body = Fetch.encode pair ("k", "v") in
  Fetch.decode pair (Fetch.post ~sw ~headers ~body client "https://x/echo");;
> POST /echo accept=-
>   content-type=application/x-pair body="k=v"
- : string * string = ("ok", "1")
```

## Sequences stream in both directions

```ocaml
# run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let r = Fetch.get ~sw client "https://x/many" in
  List.of_seq (Fetch.decode_seq pairs r);;
> GET /many accept=-
- : (string * string) list = [("a", "1"); ("b", "2")]
# run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let headers, body = Fetch.encode_seq pairs (List.to_seq [ ("a", "1"); ("b", "2") ]) in
  status (Fetch.post ~sw ~headers ~body client "https://x/echo");;
> POST /echo accept=-
>   content-type=text/x-pairs body="a=1\nb=2\n"
- : int = 200
```

## Markdown bracket nesting is bounded

The CommonMark parser is exponential in the nesting depth of link brackets,
so a body deeper than `max_bracket_depth` (16 by default) is rejected before
the parser sees it.

```ocaml
let md = Markdown.markdown ()

let nested n = String.make n '[' ^ "x" ^ String.make n ']'

let md_client body =
  Fetch_mock.client (fun req ->
      Fetch_mock.respond
        ~headers:(Http.Header.of_list [ ("Content-Type", "text/markdown") ])
        body req)
```

A shallow document decodes as usual.

```ocaml
# run @@ fun () ->
  Result.is_ok (Fetch.read_as (md_client (nested 8)) md "https://x/doc");;
- : bool = true
```

A deep one is a malformed body, and is refused in microseconds rather than
minutes.

```ocaml
# run @@ fun () ->
  Result.is_ok (Fetch.read_as (md_client (nested 30)) md "https://x/doc");;
Exception:
Eio.Io Http Decode_failure expected text/markdown, malformed body: bracket nesting deeper than 16,
  reading https://x/doc
```

The bound is the caller's to set. Lowering it refuses documents the
default would accept; raising it past the default hands cmarkit inputs it
cannot finish, so no test does that.

```ocaml
# run @@ fun () ->
  let shallow = Markdown.markdown ~max_bracket_depth:4 () in
  Result.is_ok (Fetch.read_as (md_client (nested 8)) shallow "https://x/doc");;
Exception:
Eio.Io Http Decode_failure expected text/markdown, malformed body: bracket nesting deeper than 4,
  reading https://x/doc
```

## Safe Markdown links reject control-obfuscated schemes

The compatibility guard checks the URL emitted by cmarkit after reversing
percent escapes and removing ASCII whitespace and controls. This covers both
entity-decoded and already-percent-encoded spellings until the same fix is
available from the cmarkit dependency itself.

```ocaml
let contains needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec loop i =
    i <= hl - nl
    && (String.sub haystack i nl = needle || loop (i + 1))
  in
  loop 0

let render_md source =
  Media.encode (Markdown.html ()) (Cmarkit.Doc.of_string source)
```

```ocaml
# List.map
    (fun source -> contains {|href=""|} (render_md source))
    [ "[x](java&Tab;script:alert(1))";
      "[x](java&#10;script:alert(1))";
      "[x](&#1;javascript:alert(1))";
      "[x](java%09script:alert(1))" ];;
- : bool list = [true; true; true; true]
# contains {|href="https://example.test/"|}
    (render_md "[x](https://example.test/)");;
- : bool = true
```
