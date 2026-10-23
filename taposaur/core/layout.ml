(* This hash is part of the corpus format, not an identity operation. *)
let sub_shard uid =
  Digestif.MD5.digest_string (Uid.to_string uid)
  |> Digestif.MD5.to_hex |> fun s -> String.sub s 0 2

let index_group = function
  | Uid.Doi doi -> Doi.prefix doi
  | Uid.Pmc n -> string_of_int (n / 10_000)

let suffix d =
  let parts = String.split_on_char '/' (Doi.suffix d) in
  if List.exists (fun s -> s = "" || s = "." || s = "..") parts then
    invalid_arg "DOI contains an unsafe path segment";
  let path = Doi.path d in
  let i = String.index path '/' in
  String.sub path (i + 1) (String.length path - i - 1)

let pmc n =
  let s = string_of_int n in
  "pmc/PMC" ^ String.sub s 0 (min 4 (String.length s)) ^ "/PMC" ^ s

let paper uid = match uid with
  | Uid.Doi d -> String.concat "/"
      [ "doi"; Doi.prefix d; sub_shard uid; suffix d ]
  | Uid.Pmc n -> pmc n

let flat_paper = function
  | Uid.Doi d -> "doi/" ^ Doi.prefix d ^ "/" ^ suffix d
  | Uid.Pmc n -> pmc n

let index uid = String.concat "/"
  [ Uid.Scheme.to_string (Uid.scheme uid); index_group uid;
    sub_shard uid ^ ".jsonl" ]
