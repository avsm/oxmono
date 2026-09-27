type t =
  | Uid | Flags | Internal_date | Rfc822_size | Envelope | Bodystructure
  | Modseq | Emailid | Threadid | Objectid
  | Preview of { lazy_ : bool }
  | Binary_size of int list

let to_wire = function
  | Uid -> "UID" | Flags -> "FLAGS" | Internal_date -> "INTERNALDATE"
  | Rfc822_size -> "RFC822.SIZE" | Envelope -> "ENVELOPE"
  | Bodystructure -> "BODYSTRUCTURE" | Modseq -> "MODSEQ"
  | Emailid -> "EMAILID" | Threadid -> "THREADID" | Objectid -> "OBJECTID"
  | Preview {lazy_ = false} -> "PREVIEW"
  | Preview {lazy_ = true} -> "PREVIEW (LAZY)"
  | Binary_size section ->
      "BINARY.SIZE[" ^ String.concat "." (List.map string_of_int section) ^
      "]"

let capabilities = function
  | Uid | Flags | Internal_date | Rfc822_size | Envelope | Bodystructure ->
      []
  | Modseq -> [Capability.Condstore]
  | Emailid | Threadid -> [Capability.Objectid]
  | Objectid -> [Capability.Objectid_plus]
  | Preview _ -> [Capability.Preview]
  | Binary_size _ -> [Capability.Binary]

let equal (a : t) b = a = b
let pp ppf i = Format.pp_print_string ppf (to_wire i)
