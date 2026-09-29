module Flag = Mail_flag.Imap_flag

type t =
  | Legacy_metadata of string
  | Not_a_directory of string
  | Non_native_path of string
  | Malformed_filename of string
  | Duplicate_identity of string
  | Unknown_letter of { file : string; letter : char }
  | Keyword_map of string
  | Unsupported_flag of Flag.t
  | Too_many_keywords of Flag.t
  | Unrepresentable_date of float
  | Target_exists of string
  | Vanished of string

let pp ppf = function
  | Legacy_metadata name ->
      Format.fprintf ppf "legacy %s metadata requires offline migration" name
  | Not_a_directory path -> Format.fprintf ppf "non-directory path %s" path
  | Non_native_path path ->
      Format.fprintf ppf "path %s has no native filename" path
  | Malformed_filename name ->
      Format.fprintf ppf "unrecognized Maildir filename %s" name
  | Duplicate_identity id ->
      Format.fprintf ppf "duplicate occurrence identity %s" id
  | Unknown_letter {file;letter='a'..'z' as letter} ->
      Format.fprintf ppf "%s references unmapped keyword letter %c" file letter
  | Unknown_letter {file;letter} ->
      Format.fprintf ppf "%s has invalid Maildir flag letter %C" file letter
  | Keyword_map message -> Format.pp_print_string ppf message
  | Unsupported_flag Flag.Recent ->
      Format.pp_print_string ppf "\\Recent is not a durable Maildir flag"
  | Unsupported_flag flag ->
      Format.fprintf ppf "unsupported Maildir system flag %s"
        (Flag.to_wire flag)
  | Too_many_keywords flag ->
      Format.fprintf ppf "keyword %s exceeds the 26 Maildir keyword slots"
        (Flag.to_wire flag)
  | Unrepresentable_date mtime ->
      Format.fprintf ppf "modification time %.0f is not representable" mtime
  | Target_exists name -> Format.fprintf ppf "target %s already exists" name
  | Vanished name ->
      Format.fprintf ppf "published Maildir message %s vanished" name
