(*---------------------------------------------------------------------------
   Copyright (c) 2025 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let parse subject =
  let length = String.length subject in
  if length < 7 || subject.[0] <> '#' || subject.[length - 1] <> ']' then None
  else if String.exists (function '\r' | '\n' -> true | _ -> false) subject
  then None
  else
    match
      (String.index_from_opt subject 1 '>', String.rindex_opt subject '[')
    with
    | Some separator, Some suffix
      when separator > 1 && suffix > separator + 1 && suffix < length - 2 ->
        let channel = String.sub subject 1 (separator - 1) |> String.trim in
        let topic =
          String.sub subject (separator + 1) (suffix - separator - 1)
          |> String.trim
        in
        let server =
          String.sub subject (suffix + 1) (length - suffix - 2) |> String.trim
        in
        if
          channel = "" || topic = "" || server = ""
          || String.contains server ']'
        then None
        else Some (channel, topic, server)
    | _ -> None
