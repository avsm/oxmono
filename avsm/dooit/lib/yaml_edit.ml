(* Copyright (c) 2026 Anil Madhavapeddy. SPDX-License-Identifier: ISC *)
open Common

type node = { start : int; stop : int; column : int; kind : kind }

and kind =
  | Scalar
  | Alias
  | Seq of Yamlrw.Layout_style.t * node list
  | Map of Yamlrw.Layout_style.t * (node * node) list

(* Patch parser spans rather than re-emitting the document. Unchanged bytes,
   including comments, quotes, directives and line endings, stay in place. *)
let update raw merged =
  let before = yaml raw in
  if equal before merged then raw
  else
    let events = ref (Yamlrw.Parser.to_list (Yamlrw.Parser.of_string raw)) in
    let next () =
      match !events with
      | x :: xs ->
          events := xs;
          x
      | [] -> fail "incomplete YAML events"
    in
    let peek () =
      match !events with
      | x :: _ -> x.Yamlrw.Event.event
      | [] -> Yamlrw.Event.Stream_end
    in
    let rec node () =
      let e = next () in
      let start = e.span.start.index and column = e.span.start.column - 1 in
      let make stop kind = { start; stop; column; kind } in
      match e.event with
      | Yamlrw.Event.Scalar _ ->
          (* Plain scalar spans include the whitespace consumed while checking
           for a continuation, sometimes the next line's indentation. *)
          let rec trim i =
            if i > start && String.contains " \t\r\n" raw.[i - 1] then
              trim (i - 1)
            else i
          in
          make (trim e.span.stop.index) Scalar
      | Alias _ -> make e.span.stop.index Alias
      | Sequence_start { style; _ } ->
          let rec children acc =
            match peek () with
            | Sequence_end ->
                let ending = next () in
                make ending.span.stop.index (Seq (style, List.rev acc))
            | _ -> children (node () :: acc)
          in
          children []
      | Mapping_start { style; _ } ->
          let rec children acc =
            match peek () with
            | Mapping_end ->
                let ending = next () in
                make ending.span.stop.index (Map (style, List.rev acc))
            | _ ->
                let key = node () in
                let value = node () in
                children ((key, value) :: acc)
          in
          children []
      | _ -> fail "unsupported YAML event during update"
    in
    while
      match peek () with
      | Yamlrw.Event.Stream_start _ | Document_start _ -> true
      | _ -> false
    do
      ignore (next ())
    done;
    let root = node () in
    let newline = if String.contains raw '\r' then "\r\n" else "\n" in
    let len = String.length raw in
    let rec line_start i =
      if i > 0 && raw.[i - 1] <> '\n' then line_start (i - 1) else i
    in
    let rec line_end i =
      if i < len && raw.[i] <> '\n' then line_end (i + 1) else min len (i + 1)
    in
    let spans = ref [] in
    let rec scalars n =
      match n.kind with
      | Scalar -> spans := (n.start, n.stop) :: !spans
      | Alias -> ()
      | Seq (_, xs) -> List.iter scalars xs
      | Map (_, xs) ->
          List.iter
            (fun (k, v) ->
              scalars k;
              scalars v)
            xs
    in
    scalars root;
    let comments a b =
      let result = Buffer.create 64 in
      let rec loop i =
        if i < b then
          if
            raw.[i] = '#'
            && not (List.exists (fun (a, b) -> i >= a && i < b) !spans)
          then (
            let e = min b (line_end i) in
            Buffer.add_string result (String.sub raw i (e - i));
            if e = b && b > 0 && raw.[b - 1] <> '\n' then
              Buffer.add_string result newline;
            loop e)
          else loop (i + 1)
      in
      loop a;
      Buffer.contents result
    in
    let edits = ref [] in
    let edit a b text = edits := (a, b, text) :: !edits in
    let fragment n = String.sub raw n.start (n.stop - n.start) in
    let key n =
      match yaml (fragment n) with
      | `String s -> s
      | _ -> fail "non-string YAML key"
    in
    let deletion a b =
      let retained = comments a b in
      edit a b retained
    in
    let rec patch n old value =
      if equal old value then ()
      else
        match (n.kind, old, value) with
        | Map (style, nodes), `O olds, `O values when style <> `Flow ->
            let bindings = List.map (fun (k, v) -> (key k, k, v)) nodes in
            List.iteri
              (fun i (k, kn, vn) ->
                match List.assoc_opt k values with
                | Some value -> patch vn (List.assoc k olds) value
                | None ->
                    let a = line_start kn.start in
                    let b =
                      if i + 1 < List.length bindings then
                        let _, next, _ = List.nth bindings (i + 1) in
                        line_start next.start
                      else min n.stop (line_end vn.stop)
                    in
                    deletion a b)
              bindings;
            let added =
              List.filter (fun (k, _) -> not (List.mem_assoc k olds)) values
            in
            if added <> [] then
              let column =
                match nodes with (k, _) :: _ -> k.column | [] -> n.column
              in
              let pos = if n.stop >= len then len else line_start n.stop in
              let prefix =
                if pos > 0 && raw.[pos - 1] <> '\n' then newline else ""
              in
              let lines =
                List.map
                  (fun (k, v) ->
                    String.make column ' '
                    ^ json_string (str k)
                    ^ ": " ^ json_string v ^ newline)
                  added
              in
              edit pos pos (prefix ^ String.concat "" lines)
        | Seq (style, nodes), `A olds, `A values
          when style <> `Flow && nodes <> [] ->
            let common = min (List.length olds) (List.length values) in
            List.iteri
              (fun i n ->
                if i < common then patch n (List.nth olds i) (List.nth values i))
              nodes;
            (if List.length values < List.length nodes then
               let first = List.nth nodes (List.length values) in
               if values = [] then
                 (* An explicit empty list is needed, with all old comments kept. *)
                 let a = line_start first.start in
                 let retained = comments a n.stop in
                 edit a n.stop
                   (retained
                   ^ String.make (max 0 (first.column - 2)) ' '
                   ^ "[]" ^ newline)
               else deletion (line_start first.start) n.stop);
            let added =
              List.filteri (fun i _ -> i >= List.length nodes) values
            in
            if added <> [] then
              let column = max 0 ((List.hd nodes).column - 2) in
              let pos = if n.stop >= len then len else line_start n.stop in
              let prefix =
                if pos > 0 && raw.[pos - 1] <> '\n' then newline else ""
              in
              edit pos pos
                (prefix
                ^ String.concat ""
                    (List.map
                       (fun v ->
                         String.make column ' ' ^ "- " ^ json_string v ^ newline)
                       added))
        | Scalar, _, _ -> edit n.start n.stop (json_string value)
        | _ ->
            let retained = comments n.start n.stop in
            let prefix =
              if retained = "" then "" else retained ^ String.make n.column ' '
            in
            edit n.start n.stop (prefix ^ json_string value)
    in
    patch root before merged;
    let edits =
      List.sort (fun (a, b, _) (c, d, _) -> compare (a, b) (c, d)) !edits
    in
    let b = Buffer.create len and cursor = ref 0 in
    List.iter
      (fun (a, z, s) ->
        if a < !cursor || z < a || z > len then
          fail "overlapping YAML edits require review";
        Buffer.add_substring b raw !cursor (a - !cursor);
        Buffer.add_string b s;
        cursor := z)
      edits;
    Buffer.add_substring b raw !cursor (len - !cursor);
    let result = Buffer.contents b in
    if not (equal (yaml result) merged) then
      fail "YAML update does not reproduce the planned metadata";
    result
