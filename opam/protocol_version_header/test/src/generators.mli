open! Core

(** Generator for protocol version headers with configurable version count range.
    Generates a tuple of (protocol, supported_versions, additional_magic_numbers) suitable
    for passing to [Protocol_version_header.create_exn]. *)
val header_generator
  :  min_versions:int
  -> max_versions:int
  -> (Protocol_version_header.Known_protocol.t * int list * int list)
       Quickcheck.Generator.t
