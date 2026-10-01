(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Tool = Ds4.Tool

(* What numptyd answers is the tool's result, refusals included. An [Error] is
   the death of the session, which is a result too, and one the daemon also
   reads through [Client.fault] to stop the run. The traces of a call go to the
   channel the session's own traces go to, so a caller has one place to record
   them. *)
let ask client op =
  match Client.call client op ~on_trace:(Client.trace client) with
  | Ok output -> output
  | Error e -> e

let render_of = function
  | "text" -> Ok Proto.Text
  | "raw" -> Ok Proto.Raw
  | w ->
      Error
        (Printf.sprintf
           "%S is not a render. Write \"text\" for a page reduced to the words \
            in it, or \"raw\" for the bytes as they arrived."
           w)

(* A bound of zero is what a call that named none decodes as, since a parameter
   has one default and there is no number that means "no bound". *)
let max_bytes_of = function n when n <= 0 -> None | n -> Some n

let render_description =
  "\"text\" or \"raw\". \"text\" reduces an HTML page to the words in it: the \
   content of its script and style elements dropped, its tags removed, the \
   common entities decoded and its whitespace collapsed. It is a reduction and \
   not a renderer, so it keeps no tables, no links and no layout, and a body \
   that is not HTML is returned as it stands. Ask for \"raw\" when the markup \
   is what you need, or when the reduction has dropped what you were looking \
   for."

let max_bytes_description =
  "the largest body to return, in bytes. Left out, one mebibyte. A body over \
   the bound is refused with the size it would have been rather than cut down \
   to fit, since a document cut off reads as the whole of it."

let fetch ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "fetch" (fun url render max_bytes -> (url, render, max_bytes))
    |> Invoke.param
         ~enc:(fun (u, _, _) -> u)
         "url" string
         ~description:"the URL to fetch, with its scheme, such as \"https://…\""
    |> Invoke.param
         ~enc:(fun (_, r, _) -> r)
         ~default:"text" "render" string ~description:render_description
    |> Invoke.param
         ~enc:(fun (_, _, m) -> m)
         ~default:0 "max_bytes" int ~description:max_bytes_description
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Fetch a URL and report its status code, the final URL after any \
       redirects, its content type, how many bytes came back, and then the \
       body. A status outside the 2xx range is reported as itself with what \
       the server sent after it, so read the status before you believe the \
       body: a 404 page is not the article. A body over the bound is refused \
       with the size it would have been, and the answer says so, so retry it \
       with head to see what it is, with a larger max_bytes, or on a narrower \
       page." codec (fun (url, render, max_bytes) ->
      match render_of render with
      | Error e -> e
      | Ok render ->
          ask client
            (Proto.Fetch { url; render; max_bytes = max_bytes_of max_bytes }))

let head ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "head" (fun url -> url)
    |> Invoke.param ~enc:Fun.id "url" string
         ~description:"the URL to ask about, with its scheme"
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Ask for a URL's headers alone: the status code, the final URL after any \
       redirects, the content type and the size the server declares, with no \
       body. Reach for this to see whether something has moved, or how large \
       it is before fetching it." codec (fun url ->
      ask client (Proto.Head { url }))

(* The arguments arrive as JSON rather than as one string, because a shell is
   not involved and a string would have to be split by some rule this end
   invented. A caller that writes them wrong is told the form rather than
   guessed at. *)
let args_of = function
  | Jsont.Array (elts, _) ->
      List.fold_right
        (fun elt acc ->
          match (elt, acc) with
          | Jsont.String (v, _), Ok vs -> Ok (v :: vs)
          | Jsont.String _, e -> e
          | _ ->
              Error
                "every element of args must be a string. Write a number or a \
                 flag's value as the text of it, as a command line carries it.")
        elts (Ok [])
  | _ ->
      Error
        "args must be a JSON array of strings, such as [\"log\", \"-n\", \
         \"1\"]. Each argument is one element, since the program is run \
         without a shell to split them."

let run ~client =
  let codec =
    let open Dsml.Codec in
    Invoke.map "run" (fun program args -> (program, args))
    |> Invoke.param ~enc:fst "program" string
         ~description:
           "the program to run, looked for on the PATH, such as \"git\""
    |> Invoke.param ~enc:snd ~default:(Jsont.Json.list []) "args" json
         ~description:
           "the arguments, as a JSON array of strings, such as [\"log\", \
            \"-n\", \"1\"]. Left out, none."
    |> Invoke.seal
  in
  Tool.v
    ~description:
      "Run a program with arguments and report how it ended and what it wrote, \
       its standard output and standard error interleaved. This is how a \
       service that ships a command line is reached. There is no shell, so \
       there is no quoting, no globbing, no pipeline and no redirection: name \
       the program alone and pass each argument as its own element of args. \
       Output past a mebibyte is dropped, and the answer says how much was."
    codec (fun (program, args) ->
      match args_of args with
      | Error e -> e
      | Ok args -> ask client (Proto.Run { program; args }))
