# Form bodies, urlencoded and multipart

`Fetch.Form.urlencoded` builds the body a browser sends for a form
without files, with the serializer of the
[WHATWG URL Standard](https://url.spec.whatwg.org/#application/x-www-form-urlencoded).
`Fetch.Form.multipart` frames a list of parts for a form with files:
`field` and `file` hold their content as a string and the result is a
`String` body; `stream` reads its content from a flow while the request
is sent, and one such part makes the whole body a `Stream` that frames
the parts as the backend reads it. The framing follows
[RFC 7578](https://www.rfc-editor.org/rfc/rfc7578) and the part headers
follow the
[WHATWG multipart/form-data encoding algorithm](https://html.spec.whatwg.org/multipage/form-control-infrastructure.html#multipart/form-data-encoding-algorithm).

```ocaml
# #require "fetch";;
# #require "fetch.mock";;
# #require "eio.mock";;
# #require "httpz";;
```

```ocaml
let () = Printexc.record_backtrace false
open Fetch

let source = Eio.Flow.string_source

let read_body = function
  | Empty -> ""
  | String s -> s
  | Stream { flow; _ } ->
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:1_000_000 flow)

let drain body = Eio_mock.Backend.run @@ fun () -> read_body body

let note = "%PDF-1.4 not really\n"

(* A server that hands a urlencoded body straight back, and a token
   endpoint that answers with one of its own. *)
let form_type = "application/x-www-form-urlencoded; charset=UTF-8"

let server (req : Middleware.request) =
  let headers = Http.Header.of_list [ ("Content-Type", form_type) ] in
  match Middleware.Url.path_segments req.url with
  | [ "echo" ] -> Fetch_mock.respond ~headers (read_body req.body) req
  | [ "token" ] ->
    Fetch_mock.respond ~headers
      "access_token=2YotnFZFEjr1zCsicMWpAA&token_type=bearer&expires_in=3600"
      req
  | _ -> Fetch_mock.respond ~status:404 "" req

let client = Fetch_mock.client server
let run f = Eio_mock.Backend.run f
```

## The urlencoded body

Each name and value is serialized with the urlencoded byte serializer: a
space becomes `+`, an ASCII alphanumeric and `*-._` stay as they are, and
every other byte becomes `%` and two uppercase hexadecimal digits. Pairs
join with `&` and each name to its value with `=`.

```ocaml
# print_string (drain (snd (Form.urlencoded [ ("q", "a b"); ("t", "~*") ])));;
q=a+b&t=%7E*
- : unit = ()
```

The three bytes that would otherwise be framing are encoded too, so a
value can hold them:

```ocaml
# print_string (drain (snd (Form.urlencoded [ ("k&=", "%+") ])));;
k%26%3D=%25%2B
- : unit = ()
```

Bytes above ASCII are passed through the same rule, which for a UTF-8
page is what a browser sends:

```ocaml
# print_string (drain (snd (Form.urlencoded [ ("nom", "Ada Lovelace \xc3\xa9") ])));;
nom=Ada+Lovelace+%C3%A9
- : unit = ()
```

Order and repeats are the caller's, an empty value keeps its `=`, and an
empty list is an empty body:

```ocaml
# print_string (drain (snd (Form.urlencoded
    [ ("b", "2"); ("a", ""); ("b", "1") ])));;
b=2&a=&b=1
- : unit = ()
# drain (snd (Form.urlencoded []));;
- : string = ""
```

## Reading a form-encoded response

`Media.form` is the codec for the same encoding, so a body built by
`urlencoded` round trips through it. The mock server here echoes the
request body back with the form content type:

```ocaml
# run @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let sent = [ ("grant_type", "client_credentials"); ("scope", "read write");
               ("state", "a&b=c") ] in
  let headers, body = Form.urlencoded sent in
  let got = Fetch.decode Media.form (Fetch.post ~sw ~headers ~body client "https://x/echo") in
  (got = sent, got);;
- : bool * (string * string) list =
(true,
 [("grant_type", "client_credentials"); ("scope", "read write");
  ("state", "a&b=c")])
```

An OAuth token endpoint answers in that encoding, so `read_as` reads it
without a line of parsing:

```ocaml
# run @@ fun () -> Fetch.read_as client Media.form "https://x/token";;
- : ((string * string) list, response) result =
Ok
 [("access_token", "2YotnFZFEjr1zCsicMWpAA"); ("token_type", "bearer");
  ("expires_in", "3600")]
```

## The same bytes either way

The following values contain the same parts, once buffered and once streamed:

```ocaml
let buffered =
  snd (Form.multipart ~boundary:"BOUND"
         [ Form.field "consolidate" "1";
           Form.file ~name:"input" ~filename:"a.pdf"
             ~content_type:"application/pdf" note ])

let streamed () =
  snd (Form.multipart ~boundary:"BOUND"
         [ Form.field "consolidate" "1";
           Form.stream ~name:"input" ~filename:"a.pdf"
             ~content_type:"application/pdf" (source note) ])
```

The buffered body frames each part between the boundary lines:

```ocaml
# print_string (drain buffered);;
--BOUND
Content-Disposition: form-data; name="consolidate"

1
--BOUND
Content-Disposition: form-data; name="input"; filename="a.pdf"
Content-Type: application/pdf

%PDF-1.4 not really

--BOUND--
- : unit = ()
```

The streamed one puts the same bytes on the wire:

```ocaml
# print_string (drain (streamed ()));;
--BOUND
Content-Disposition: form-data; name="consolidate"

1
--BOUND
Content-Disposition: form-data; name="input"; filename="a.pdf"
Content-Type: application/pdf

%PDF-1.4 not really

--BOUND--
- : unit = ()
```

```ocaml
# String.equal (drain buffered) (drain (streamed ()));;
- : bool = true
```

## Length and replayability

A streamed part that declares its length lets the request carry a
`Content-Length`, counted over the framing too:

```ocaml
let sized =
  snd (Form.multipart ~boundary:"BOUND"
         [ Form.field "consolidate" "1";
           Form.stream ~name:"input" ~filename:"a.pdf"
             ~content_type:"application/pdf"
             ~length:(Int64.of_int (String.length note)) (source note) ])
```

```ocaml
# Middleware.body_length sized;;
- : int64 option = Some 205L
# Int64.of_int (String.length (drain sized));;
- : int64 = 205L
```

Without a declared length the size is unknown until the flow ends, so
the backend has to send the body chunked:

```ocaml
# Middleware.body_length (streamed ());;
- : int64 option = None
```

A buffered body can be re-sent after a redirect or a retry. A streamed
one is read once and is gone:

```ocaml
# (Middleware.body_replayable buffered, Middleware.body_replayable (streamed ()));;
- : bool * bool = (true, false)
# let once = streamed () in
  let first = String.length (drain once) in
  (first, String.length (drain once));;
- : int * int = (205, 0)
```

## The boundary invariant

A string part holding the boundary is refused when the body is built:

```ocaml
# Form.multipart ~boundary:"BOUND"
    [ Form.file ~name:"input" ~filename:"a.pdf"
        ~content_type:"application/pdf" "before BOUND after" ];;
Exception:
Invalid_argument "Fetch.Form.multipart: boundary occurs in a part".
```

A streamed part cannot be inspected up front, so the boundary is matched
against its content as it passes and the request fails during the send.
The failure is an `Invalid_request` raised from the body flow rather
than the build-time `Invalid_argument` above:

```ocaml
# drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.stream ~name:"input" ~filename:"a.pdf"
                ~content_type:"application/pdf"
                (source "before BOUND after") ]));;
Exception:
Eio.Io Http Invalid_request "multipart boundary occurs in a streamed part"
```

The match is carried across reads, so splitting the boundary between two
of them hides nothing:

```ocaml
# let split = Eio_mock.Flow.make "pdf" in
  Eio_mock.Flow.on_read split [ `Return "before BO"; `Return "UND after" ];
  drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.stream ~name:"input" ~filename:"a.pdf"
                ~content_type:"application/pdf" split ]));;
+pdf: read "before BO"
+pdf: read "UND after"
Exception:
Eio.Io Http Invalid_request "multipart boundary occurs in a streamed part"
```

## A declared length is exact

A source that ends early would leave the framing and the
`Content-Length` disagreeing with the bytes sent, so it fails instead:

```ocaml
# drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.stream ~name:"input" ~filename:"a.pdf"
                ~content_type:"application/pdf" ~length:64L
                (source "short") ]));;
Exception:
Eio.Io Http Invalid_request "streamed part \"input\" declared 64 bytes but produced 5"
```

So does one with more to give than it declared:

```ocaml
# drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.stream ~name:"input" ~filename:"a.pdf"
                ~content_type:"application/pdf" ~length:3L
                (source "short") ]));;
Exception:
Eio.Io Http Invalid_request "streamed part \"input\" declared 3 bytes but produced more"
```

A negative length is a caller's mistake and is refused at once:

```ocaml
# Form.stream ~name:"input" ~filename:"a.pdf" ~content_type:"application/pdf"
    ~length:(-1L) (source "short");;
Exception: Invalid_argument "Fetch.Form.stream: length -1 is negative".
```

A filename is checked as `file` checks it:

```ocaml
# Form.stream ~name:"input" ~filename:"a\\b.pdf"
    ~content_type:"application/pdf" (source "x");;
Exception:
Invalid_argument
 "Fetch.Form.stream: filename may not contain a backslash or a forbidden control byte".
```

## Names and filenames are escaped, not rejected

The WHATWG encoding algorithm escapes the three bytes that would close or
fold the `Content-Disposition` quoted-string rather than refusing them, so
a name or a filename may hold a quote, a CR or an LF and still name the
same control the form did:

```ocaml
# print_string (drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.file ~name:"in\"put" ~filename:"a\r\nb\".pdf"
                ~content_type:"application/pdf" "x" ])));;
--BOUND
Content-Disposition: form-data; name="in%22put"; filename="a%0D%0Ab%22.pdf"
Content-Type: application/pdf

x
--BOUND--
- : unit = ()
```

A backslash has no such escape: inside a quoted-string it is a
quoted-pair to one parser and a literal to the next, so it is refused
along with every other control byte and DEL.

```ocaml
# Form.field "back\\slash" "x";;
Exception:
Invalid_argument
 "Fetch.Form.field: name may not contain a backslash or a forbidden control byte".
# Form.field "nul\000byte" "x";;
Exception:
Invalid_argument
 "Fetch.Form.field: name may not contain a backslash or a forbidden control byte".
```

A content type is a field value, so a CR in one is still a rejection:

```ocaml
# Form.file ~name:"input" ~filename:"a.pdf"
    ~content_type:"text/plain\r\nX-Injected: yes" "x";;
Exception:
Invalid_argument
 "Fetch.Form.file: content_type contains a forbidden control byte".
```

## A field may carry a content type and further headers

A `field` without a content type is `text/plain` to the receiver. Naming
one puts it in the part, and `headers` adds part headers after the ones
the part derives:

```ocaml
# print_string (drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.field ~content_type:"application/json" "meta" {|{"n":1}|};
              Form.field ~headers:[ ("Content-Transfer-Encoding", "8bit") ]
                "note" "hi" ])));;
--BOUND
Content-Disposition: form-data; name="meta"
Content-Type: application/json

{"n":1}
--BOUND
Content-Disposition: form-data; name="note"
Content-Transfer-Encoding: 8bit

hi
--BOUND--
- : unit = ()
```

The two headers the part derives may not be given a second value, since a
receiver would have to choose between them:

```ocaml
# Form.field ~headers:[ ("Content-Disposition", "inline") ] "note" "hi";;
Exception:
Invalid_argument
 "Fetch.Form.field: part header content-disposition is derived from the part".
# Form.field ~headers:[ ("content-type", "text/plain") ] "note" "hi";;
Exception:
Invalid_argument
 "Fetch.Form.field: part header content-type is derived from the part".
```

A header name is a token and a value is a field value:

```ocaml
# Form.field ~headers:[ ("X Note", "hi") ] "note" "hi";;
Exception:
Invalid_argument
 "Fetch.Form.field: part header name \"X Note\" is not a token".
# Form.field ~headers:[ ("X-Note", "hi\r\nX-Injected: yes") ] "note" "hi";;
Exception:
Invalid_argument
 "Fetch.Form.field: part header \"X-Note\" contains a forbidden control byte".
```

## The boundary avoids the headers, not only the content

The boundary must be absent from what a part serializes as well as from
what it carries, so a filename or an extra header value holding it is
refused just as content is:

```ocaml
# Form.multipart ~boundary:"BOUND"
    [ Form.file ~name:"input" ~filename:"BOUND.pdf"
        ~content_type:"application/pdf" "x" ];;
Exception:
Invalid_argument "Fetch.Form.multipart: boundary occurs in a part".
# Form.multipart ~boundary:"BOUND"
    [ Form.field ~headers:[ ("X-Note", "see BOUND") ] "note" "hi" ];;
Exception:
Invalid_argument "Fetch.Form.multipart: boundary occurs in a part".
```

## Mixed parts keep their order

```ocaml
# print_string (drain
    (snd (Form.multipart ~boundary:"BOUND"
            [ Form.field "first" "1";
              Form.stream ~name:"input" ~filename:"a.pdf"
                ~content_type:"application/pdf" (source note);
              Form.field "last" "2" ])));;
--BOUND
Content-Disposition: form-data; name="first"

1
--BOUND
Content-Disposition: form-data; name="input"; filename="a.pdf"
Content-Type: application/pdf

%PDF-1.4 not really

--BOUND
Content-Disposition: form-data; name="last"

2
--BOUND--
- : unit = ()
```

## A derived boundary differs between bodies

Fields alone still give a `String` body, and without `~boundary` a fresh
candidate is checked against every part. Two bodies built from the same parts
therefore agree part for part but are framed differently, without exposing a
digest of their values in `Content-Type`. `Httpz.Multipart` reads them back:

```ocaml
let parts_of (headers, body) =
  let ct = snd (List.hd (Header.to_list headers)) in
  let boundary = Option.get (Httpz.Multipart.boundary_of_content_type ct) in
  let s = drain body in
  match Httpz.Multipart.parse ~boundary s with
  | Ok ps ->
    (boundary,
     List.map
       (fun (p : Httpz.Multipart.part) ->
         (p.name, p.filename, p.content_type, Httpz.Multipart.content s p))
       ps)
  | Error e -> failwith e

let make () =
  Form.multipart [ Form.field "input" "<article/>"; Form.field "segment" "1" ]
```

```ocaml
# let b1, p1 = parts_of (make ()) and b2, p2 = parts_of (make ()) in
  (String.equal b1 b2, p1 = p2, p1);;
- : bool * bool * (string * string option * string option * string) list =
(false, true,
 [("input", None, None, "<article/>"); ("segment", None, None, "1")])
```

The boundary is still a token no longer than the 70 characters RFC 2046
allows, and it does not occur in the body it frames:

```ocaml
# let headers, body = make () in
  let b = Option.get
    (Httpz.Multipart.boundary_of_content_type
       (snd (List.hd (Header.to_list headers)))) in
  (Middleware.is_token b, String.length b <= 70);;
- : bool * bool = (true, true)
```
