(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
module P = Jmap.Proto

let tokens s =
  let b = Buffer.create 32 in
  let rec loop i quoted acc =
    if i = String.length s then (
      if quoted then failwith "Unclosed search quote";
      List.rev (if Buffer.length b = 0 then acc else Buffer.contents b :: acc))
    else
      match s.[i] with
      | '"' -> loop (i + 1) (not quoted) acc
      | '\\' when i + 1 < String.length s ->
          Buffer.add_char b s.[i + 1];
          loop (i + 2) quoted acc
      | (' ' | '\t' | '\n') when not quoted ->
          let acc =
            if Buffer.length b = 0 then acc else Buffer.contents b :: acc
          in
          Buffer.clear b;
          loop (i + 1) quoted acc
      | c ->
          Buffer.add_char b c;
          loop (i + 1) quoted acc
  in
  loop 0 false []

let date value =
  if String.length value <> 10 then failwith "Search dates use YYYY-MM-DD";
  match Ptime.of_rfc3339 (value ^ "T00:00:00Z") with
  | Ok (t, _, _) -> t
  | Error _ -> failwith "Invalid search date"

let term word =
  match String.index_opt word ':' with
  | None -> P.Email.filter ~text:word ()
  | Some i -> (
      let key = String.lowercase_ascii (String.sub word 0 i) in
      let value = String.sub word (i + 1) (String.length word - i - 1) in
      if value = "" then failwith ("Missing search value for " ^ key);
      match (key, value) with
      | "from", _ -> P.Email.filter ~from:value ()
      | "to", _ -> P.Email.filter ~to_:value ()
      | "cc", _ -> P.Email.filter ~cc:value ()
      | "subject", _ -> P.Email.filter ~subject:value ()
      | "body", _ -> P.Email.filter ~body:value ()
      | "before", _ -> P.Email.filter ~before:(date value) ()
      | "after", _ -> P.Email.filter ~after:(date value) ()
      | "is", "unread" -> P.Email.filter ~not_keyword:`Seen ()
      | "is", "read" -> P.Email.filter ~has_keyword:`Seen ()
      | "is", ("starred" | "flagged") -> P.Email.filter ~has_keyword:`Flagged ()
      | "has", "attachment" -> P.Email.filter ~has_attachment:true ()
      | ("is" | "has"), _ -> failwith ("Unknown search filter: " ^ word)
      | _ -> P.Email.filter ~text:word ())

let filter ~mailbox query =
  let terms = List.map term (tokens query) in
  match (mailbox, terms) with
  | None, [ term ] -> term
  | None, [] -> P.Email.filter ()
  | _ -> P.Filter.and_ (P.Email.filter ?in_mailbox:mailbox () :: terms)
