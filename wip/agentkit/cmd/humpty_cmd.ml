(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Expect = struct
  type call = { line : int; tool : string; arguments : string }
  type prompt = { line : int; text : string }
  type item = Call of call | Prompt of prompt

  (* Said whenever a line is refused, since the reader cannot guess what was
     meant and the form is short enough to state in full. *)
  let form =
    "A line is blank, a # comment, a prompt to send to the model, written as ? \
     followed by its text, or a tool call: the tool's name, one space, and its \
     arguments as the JSON object a model would send, as in: build {}"

  let bad n what = Error (Printf.sprintf "line %d: %s. %s" n what form)

  let is_name s =
    s <> ""
    && String.for_all
         (function
           | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true | _ -> false)
         s

  (* A script written on another machine ends its lines with a carriage
     return, which would otherwise become part of the JSON. *)
  let chop s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s

  let parse text =
    let rec go n acc = function
      | [] -> Ok (List.rev acc)
      | l :: rest -> (
          let s = String.trim (chop l) in
          if s = "" || s.[0] = '#' then go (n + 1) acc rest
          else if s.[0] = '?' then
            let text = String.trim (String.sub s 1 (String.length s - 1)) in
            if text = "" then bad n "there is no text after ?"
            else go (n + 1) (Prompt { line = n; text } :: acc) rest
          else
            match String.index_opt s ' ' with
            | None -> bad n "there are no arguments after the tool name"
            | Some i ->
                let tool = String.sub s 0 i in
                let arguments =
                  String.trim (String.sub s (i + 1) (String.length s - i - 1))
                in
                if not (is_name tool) then
                  bad n (Printf.sprintf "%S is not a tool name" tool)
                else if arguments = "" then
                  bad n "there are no arguments after the tool name"
                else if arguments.[0] <> '{' then
                  bad n "the arguments are not a JSON object"
                else go (n + 1) (Call { line = n; tool; arguments } :: acc) rest
          )
    in
    go 1 [] (String.split_on_char '\n' text)

  let trim_slash s =
    let n = String.length s in
    if n > 1 && s.[n - 1] = '/' then String.sub s 0 (n - 1) else s

  let roots dirs =
    dirs |> List.map trim_slash
    (* A relative name, such as the "." a workspace is usually given as, would
       match text all over a transcript. *)
    |> List.filter (fun d -> String.length d > 1 && d.[0] = '/')
    |> List.sort_uniq String.compare
    (* Longest first, so that a workspace inside another workspace is the one
       that matches. *)
    |> List.sort (fun a b -> compare (String.length b) (String.length a))

  let replace ~sub ~by s =
    if sub = "" then s
    else begin
      let b = Buffer.create (String.length s) in
      let n = String.length s and m = String.length sub in
      let i = ref 0 in
      while !i <= n - m do
        if String.sub s !i m = sub then begin
          Buffer.add_string b by;
          i := !i + m
        end
        else begin
          Buffer.add_char b s.[!i];
          incr i
        end
      done;
      Buffer.add_string b (String.sub s !i (n - !i));
      Buffer.contents b
    end

  (* A duration is what varies between two runs that did the same thing, and it
     is always last on the line: "dune: reply 0.4s", "dune: waiting for socket
     5s". *)
  let drop_duration s =
    let digit c = c >= '0' && c <= '9' in
    let n = String.length s in
    if n < 3 || s.[n - 1] <> 's' || not (digit s.[n - 2]) then s
    else
      let rec back i =
        if i >= 0 && (digit s.[i] || s.[i] = '.') then back (i - 1) else i
      in
      let i = back (n - 2) in
      if i >= 0 && s.[i] = ' ' then String.sub s 0 i else s

  let scrub ~roots text =
    String.split_on_char '\n' text
    |> List.map (fun line ->
        drop_duration
          (List.fold_left
             (fun l root -> replace ~sub:root ~by:"$WS" l)
             line roots))
    |> String.concat "\n"

  (* What okit puts before a server's own output, which is the one part of a
     message that is not a sentence to be read on one line. *)
  let tail_marker = "The server's last output was:"

  let one_line text =
    let text =
      let n = String.length text and m = String.length tail_marker in
      let rec at i =
        if i + m > n then text
        else if String.sub text i m = tail_marker then String.sub text 0 i
        else at (i + 1)
      in
      at 0
    in
    String.split_on_char '\n' text
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")
    |> String.concat " "
end
