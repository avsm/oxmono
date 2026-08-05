# Multipart forms, buffered and streamed

`Fetch.Form.multipart` frames a list of parts. `field` and `file` hold
their content as a string and the result is a `String` body. `stream`
reads its content from a flow while the request is sent, and one such
part makes the whole body a `Stream` that frames the parts as the
backend reads it.

```ocaml
# #require "fetch";;
# #require "eio.mock";;
```

```ocaml
open Fetch

let source = Eio.Flow.string_source

(* Read a body to a string, as a backend sending it would. *)
let drain body =
  Eio_mock.Backend.run @@ fun () ->
  match body with
  | Empty -> ""
  | String s -> s
  | Stream { flow; _ } ->
    Eio.Buf_read.take_all (Eio.Buf_read.of_flow ~max_size:1_000_000 flow)

let note = "%PDF-1.4 not really\n"
```

## The same bytes either way

The same parts, once buffered and once streamed:

```ocaml
let buffered =
  snd (Form.multipart ~boundary:"BOUND"
         [ Form.field "consolidate" "1";
           Form.file ~name:"input" ~filename:"a.pdf"
             ~content_type:"application/pdf" note ])

(* A streamed body is spent once it has been read, so build a fresh one
   for each use below. *)
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
# Form.stream ~name:"input" ~filename:"a\"b.pdf"
    ~content_type:"application/pdf" (source "x");;
Exception:
Invalid_argument
 "Fetch.Form.stream: filename may not hold a quote or a line break".
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

## Nothing changes for string parts

Fields alone still derive the boundary from the parts and still give a
`String` body:

```ocaml
# let body =
    snd (Form.multipart
           [ Form.field "input" "<article/>"; Form.field "segment" "1" ]) in
  print_string (drain body);;
--formaa15bbc16ef62411e55440b3f1503858x0
Content-Disposition: form-data; name="input"

<article/>
--formaa15bbc16ef62411e55440b3f1503858x0
Content-Disposition: form-data; name="segment"

1
--formaa15bbc16ef62411e55440b3f1503858x0--
- : unit = ()
```
