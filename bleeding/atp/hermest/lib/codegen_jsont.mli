(* This Source Code Form is subject to the terms of the Mozilla Public
   License, v. 2.0. If a copy of the MPL was not distributed with this
   file, You can obtain one at http://mozilla.org/MPL/2.0/.

   Portions copyright (c) 2025 Anil Madhavapeddy. *)

(** Jsont codec generation from AT Protocol lexicons.

    This module generates OCaml code with jsont declarative codecs from parsed
    lexicon documents. The generated code follows this pattern:

    {[
      type foo = { bar : string; baz : int option }

      let foo_jsont =
        Jsont.Object.map ~kind:"Foo" (fun bar baz -> { bar; baz })
        |> Jsont.Object.mem "bar" Jsont.string ~enc:(fun r -> r.bar)
        |> Jsont.Object.opt_mem "baz" Jsont.int ~enc:(fun r -> r.baz)
        |> Jsont.Object.finish
    ]}

    {2 Generated Code Structure}

    For each lexicon, the generator produces:

    - Type definitions for all objects, records, and unions
    - Jsont codecs ([*_jsont]) for JSON serialization/deserialization
    - Proper handling of optional fields, nullable fields, and defaults
    - Import statements for cross-file references

    {2 Cyclic Dependencies}

    When lexicons have circular dependencies, the generator uses [Jsont.json] as
    a fallback to break the cycle. Pass the list of cyclic NSIDs to
    {!gen_lexicon_module} to enable this behavior.

    {2 Union Types}

    Discriminated unions (with [$type] field) are generated as OCaml polymorphic
    variants:

    {[
      type embed =
        [ `Images of images | `External of external_ | `Record of record ]
    ]}

    {2 Example Usage}

    {[
      let code = Codegen_jsont.gen_lexicon_module doc in
      let path = Naming.file_path_of_nsid doc.id in
      Out_channel.write_all path ~data:code
    ]} *)

val gen_lexicon_module :
  ?cyclic_nsids:string list -> Lexicon_types.lexicon_doc -> string
(** [gen_lexicon_module ?cyclic_nsids doc] generates OCaml source code (.ml) for
    the lexicon document [doc].

    @param cyclic_nsids
      List of NSIDs that form dependency cycles with this lexicon. References to
      these NSIDs will use [Jsont.json] to break the cycle.

    @return
      Complete OCaml module source code as a string, including:
      - Copyright header
      - Type definitions
      - Jsont codec implementations *)

val gen_lexicon_interface :
  ?cyclic_nsids:string list -> Lexicon_types.lexicon_doc -> string
(** [gen_lexicon_interface ?cyclic_nsids doc] generates OCaml interface code
    (.mli) for the lexicon document [doc], with OCamldoc from lexicon
    descriptions.

    @param cyclic_nsids
      List of NSIDs that form dependency cycles with this lexicon.

    @return
      Complete OCaml interface source code as a string, including:
      - Copyright header
      - Module-level OCamldoc from the lexicon description
      - Type definitions with field documentation
      - Val declarations for jsont codecs *)

val gen_unified_module :
  module_name:string -> Lexicon_types.lexicon_doc list -> string
(** [gen_unified_module ~module_name lexicons] generates a single OCaml module
    containing all lexicons with nested module structure.

    This approach avoids odoc "hidden fields" warnings by defining all types
    inline in the module hierarchy, so cross-references use public module paths
    (e.g., [Notification.Defs.activity_subscription]) instead of internal flat
    module names (e.g., [App_bsky_notification_defs.activity_subscription]).

    @param module_name Name of the wrapper module (e.g., "Bsky_lexicons")
    @param lexicons List of all lexicon documents to include

    @return Complete OCaml module source code with nested structure *)

val gen_unified_interface :
  module_name:string -> Lexicon_types.lexicon_doc list -> string
(** [gen_unified_interface ~module_name lexicons] generates a single OCaml
    interface (.mli) containing all lexicons with nested module signatures.

    This generates the interface file matching {!gen_unified_module}, with
    OCamldoc comments from lexicon descriptions.

    @param module_name Name of the wrapper module (e.g., "Bsky_lexicons")
    @param lexicons List of all lexicon documents to include

    @return Complete OCaml interface source code with nested module signatures
*)
