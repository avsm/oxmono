(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
let render html =
  let out = Buffer.create (String.length html) in
  let stack = ref [] and hidden = ref 0 and pre = ref 0 in
  let last () =
    if Buffer.length out = 0 then '\n'
    else Buffer.nth out (Buffer.length out - 1)
  in
  let newline () = if last () <> '\n' then Buffer.add_char out '\n' in
  let block = function
    | "p" | "div" | "br" | "li" | "ul" | "ol" | "tr" | "table" | "h1" | "h2"
    | "h3" | "h4" | "blockquote" | "pre" | "hr" ->
        true
    | _ -> false
  in
  let invisible = function
    | "script" | "style" | "head" | "template" -> true
    | _ -> false
  in
  let emit s =
    String.iter
      (fun c ->
        if !pre > 0 then Buffer.add_char out c
        else if c = ' ' || c = '\n' || c = '\r' || c = '\t' then (
          if last () <> ' ' && last () <> '\n' then Buffer.add_char out ' ')
        else Buffer.add_char out c)
      s
  in
  Markup.string html |> Markup.parse_html |> Markup.signals
  |> Markup.iter (function
    | `Start_element ((_, tag), attrs) ->
        let attr k = List.assoc_opt ("", k) attrs in
        let hide = invisible tag || List.mem_assoc ("", "hidden") attrs in
        stack := (tag, hide, attr "href") :: !stack;
        if hide then incr hidden;
        if tag = "pre" then incr pre;
        if !hidden = 0 then (
          if block tag then newline ();
          if tag = "li" then emit "* ";
          if tag = "img" then Option.iter emit (attr "alt"))
    | `End_element -> (
        match !stack with
        | [] -> ()
        | (tag, hide, href) :: rest ->
            if !hidden = 0 then (
              if tag = "a" then
                Option.iter
                  (fun url ->
                    if
                      List.exists
                        (fun prefix -> String.starts_with ~prefix url)
                        [ "https://"; "http://"; "mailto:" ]
                    then emit (" <" ^ url ^ ">"))
                  href;
              if block tag then newline ();
              if tag = "td" || tag = "th" then emit " ");
            if hide then decr hidden;
            if tag = "pre" then decr pre;
            stack := rest)
    | `Text strings when !hidden = 0 -> List.iter emit strings
    | _ -> ());
  Termanil_model.safe_text (String.trim (Buffer.contents out))
