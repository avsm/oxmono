(* Local replacement for the small matrix.glyph surface used by Width. *)
module String = struct
  let iter_graphemes f s =
    let offset = ref 0 in
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun () segment ->
        let len = Stdlib.String.length segment in
        f ~offset:!offset ~len;
        offset := !offset + len)
      () s

  let cluster_width s =
    let width = ref 0 in
    let regional = ref false in
    let emoji_style = ref false in
    let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
    let rec loop () =
      match Uutf.decode decoder with
      | `Uchar u ->
          let code = Uchar.to_int u in
          if code >= 0x1f1e6 && code <= 0x1f1ff then regional := true;
          if code = 0xfe0f || code = 0x20e3 then emoji_style := true;
          let hint = Uucp.Break.tty_width_hint u in
          width := max !width (max 0 hint);
          loop ()
      | `Malformed _ ->
          width := max !width 1;
          loop ()
      | `End -> max !width (if !regional || !emoji_style then 2 else 0)
      | `Await -> assert false
    in
    loop ()

  let measure ?(width_method = `Unicode) ?(tab_width = 2) s =
    let _ = width_method in
    let total = ref 0 in
    Uuseg_string.fold_utf_8 `Grapheme_cluster
      (fun () segment ->
        total := !total +
          if segment = "\t" then tab_width else cluster_width segment)
      () s;
    !total

  let measure_sub ?width_method ?tab_width s ~pos ~len =
    measure ?width_method ?tab_width (Stdlib.String.sub s pos len)
end
