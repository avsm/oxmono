module Flag = Mail_flag.Imap_flag

type t = Flag.t option array
type error = Maildir_error.t

exception Invalid of error

let invalid e = raise (Invalid e)
let catch f = try Ok (f ()) with Invalid e -> Error e

let max_size = 65536
let empty = Array.make 26 None

let check_flags flags = List.iter (function
  | Flag.System _ | Keyword _ -> ()
  | Recent | Extension _ as flag ->
      invalid (Maildir_error.Unsupported_flag flag)) flags

let validate_flags flags = catch (fun () -> check_flags flags)

let find mapping flag = Array.find_index (function
  | Some existing -> Flag.equal existing flag
  | None -> false) mapping

let parse_line mapping line =
  let invalid what = invalid (Maildir_error.Keyword_map
    (Printf.sprintf "%s in dovecot-keywords line %S" what line)) in
  match String.index_opt line ' ' with
  | None -> invalid "missing separator"
  | Some separator ->
      let index=String.sub line 0 separator in
      let name=String.sub line (separator+1)
        (String.length line-separator-1) in
      let index=match int_of_string_opt index with
        | Some i when i>=0 && i<26 && index=string_of_int i -> i
        | _ -> invalid "invalid index" in
      let flag=match Flag.keyword name with
        | Ok flag -> flag | Error _ -> invalid "invalid name" in
      if mapping.(index)<>None || find mapping flag<>None then
        invalid "duplicate mapping";
      mapping.(index)<-Some flag

let parse raw = catch (fun () ->
  let mapping=Array.copy empty in
  String.split_on_char '\n' raw |> List.iter (fun line ->
    if String.trim line<>"" then parse_line mapping line);
  mapping)

let encode_exn mapping =
  let buffer=Buffer.create 128 in
  Array.iteri (fun i -> function None -> () | Some flag ->
    Buffer.add_string buffer (Printf.sprintf "%d %s\n" i
      (Flag.to_wire flag))) mapping;
  let raw=Buffer.contents buffer in
  if String.length raw>max_size then
    invalid (Maildir_error.Keyword_map "dovecot-keywords exceeds 64 KiB");
  raw

let encode mapping = catch (fun () -> encode_exn mapping)

let add mapping flags = catch (fun () ->
  check_flags flags;
  let mapping=Array.copy mapping in
  List.iter (function
    | Flag.Keyword _ as flag when find mapping flag=None ->
        (match Array.find_index Option.is_none mapping with
         | None -> invalid (Maildir_error.Too_many_keywords flag)
         | Some i -> mapping.(i)<-Some flag)
    | _ -> ()) flags;
  ignore (encode_exn mapping : string);
  mapping)

let equal = Array.for_all2 (Option.equal Flag.equal)

let system_letters = [
  'D', Flag.Draft; 'F', Flag.Flagged; 'R', Flag.Answered;
  'S', Flag.Seen; 'T', Flag.Deleted ]

let letters ?(passed=false) mapping flags =
  let letter = function
    | Flag.System system ->
        List.find_map (fun (c,s) -> if s=system then Some c else None)
          system_letters
    | Keyword _ as flag ->
        (match find mapping flag with
         | Some i -> Some (Char.chr (Char.code 'a'+i))
         | None -> invalid_arg ("Maildir.Keywords.letters: keyword " ^
             Flag.to_wire flag ^ " has no filename mapping"))
    | Recent | Extension _ -> None in
  let chars=List.filter_map letter flags in
  let chars=if passed then 'P'::chars else chars in
  String.of_seq (List.to_seq (List.sort_uniq Char.compare chars))

let flags mapping ~file letters = catch (fun () ->
  String.fold_left (fun acc c ->
    match c with
    | 'a'..'z' ->
        (match mapping.(Char.code c-Char.code 'a') with
         | Some flag -> flag::acc
         | None -> invalid (Maildir_error.Unknown_letter {file;letter=c}))
    | 'P' -> acc
    | c ->
        match List.assoc_opt c system_letters with
        | Some system -> Flag.system system::acc
        | None -> invalid (Maildir_error.Unknown_letter {file;letter=c}))
    [] letters
  |> Flag.durable)
