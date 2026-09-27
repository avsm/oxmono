type t = Mail_flag.Imap_flag.t option array
let fail message = failwith ("Imap_maildir: " ^ message)
let validate_flags flags = List.iter (function
  | Mail_flag.Imap_flag.System _ | Keyword _ -> ()
  | Recent -> fail "Recent is not a durable Maildir flag"
  | Extension _ -> fail "unsupported Maildir system flag") flags
let find mapping flag = Array.find_index (function
  | Some existing -> Mail_flag.Imap_flag.equal existing flag
  | None -> false) mapping
let parse raw =
  let mapping=Array.make 26 None in
  let lines=String.split_on_char '\n' raw in
  let lines=match List.rev lines with
    | ""::rest -> List.rev rest
    | _ -> fail "unterminated dovecot-keywords" in
  List.iter (fun line ->
    match String.index_opt line ' ' with
    | None -> fail "invalid dovecot-keywords line"
    | Some separator ->
        let index=String.sub line 0 separator in
        let name=String.sub line (separator+1) (String.length line-separator-1) in
        let index=match int_of_string_opt index with
          | Some i when i>=0 && i<26 && index=string_of_int i -> i
          | _ -> fail "invalid dovecot-keywords index" in
        let flag=match Mail_flag.Imap_flag.keyword name with
          | Ok flag -> flag | Error _ -> fail "invalid dovecot-keywords name" in
        if mapping.(index)<>None || find mapping flag<>None then
          fail "duplicate dovecot-keywords mapping";
        mapping.(index)<-Some flag) lines;
  mapping
let encode mapping =
  let buffer=Buffer.create 128 in
  Array.iteri (fun i -> function None -> () | Some flag ->
    Buffer.add_string buffer (Printf.sprintf "%d %s\n" i
      (Mail_flag.Imap_flag.to_wire flag))) mapping;
  let raw=Buffer.contents buffer in
  if String.length raw>65536 then fail "dovecot-keywords exceeds 64 KiB";
  raw
let add mapping flags =
  validate_flags flags;
  let mapping=Array.copy mapping in
  List.iter (function
    | Mail_flag.Imap_flag.Keyword _ as flag when find mapping flag=None ->
        (match Array.find_index Option.is_none mapping with
         | None -> fail "Maildir supports at most 26 distinct mapped keywords"
         | Some i -> mapping.(i)<-Some flag)
    | _ -> ()) flags;
  ignore (encode mapping : string);
  mapping
let letters mapping flags = List.filter_map (function
  | Mail_flag.Imap_flag.Keyword _ as flag ->
      (match find mapping flag with
       | Some i -> Some (Char.chr (Char.code 'a'+i))
       | None -> fail "keyword has no filename mapping")
  | _ -> None) flags
let flags mapping letters =
  let result=ref [] in
  String.iter (function
    | 'a'..'z' as c ->
        (match mapping.(Char.code c-Char.code 'a') with
         | Some flag -> result:=flag::!result
         | None -> fail "filename references an unmapped keyword")
    | 'D' | 'F' | 'P' | 'R' | 'S' | 'T' -> ()
    | _ -> fail "invalid Maildir flag letter") letters;
  Mail_flag.Imap_flag.durable !result
