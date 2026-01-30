(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Parse YAML frontmatter from Markdown files (Jekyll-format).

    This library parses files with YAML frontmatter headers delimited by
    '---' markers:

    {v
    ---
    title: My Post
    date: 2025-01-15
    tags:
      - ocaml
      - programming
    ---

    The body content starts here.
    v}

    {1 Basic Usage}

    {[
      match Frontmatter.of_string content with
      | Ok fm ->
        let title = Frontmatter.find_string "title" fm in
        let body = Frontmatter.body fm in
        ...
      | Error msg -> Printf.eprintf "Parse error: %s\n" msg
    ]}

    {1 Typed Decoding}

    For structured access, use Jsont codecs:

    {[
      type post = { title: string; date: Ptime.t; tags: string list }

      let post_jsont =
        Jsont.Object.map ~kind:"post"
          (fun title date tags -> { title; date; tags })
        |> Jsont.Object.mem "title" Jsont.string ~enc:(fun p -> p.title)
        |> Jsont.Object.mem "date" ptime_jsont ~enc:(fun p -> p.date)
        |> Jsont.Object.mem "tags" Jsont.(list string) ~dec_absent:[]
             ~enc:(fun p -> p.tags)
        |> Jsont.Object.finish

      let post = Frontmatter.decode post_jsont fm
    ]}
*)

(** {1 Types} *)

type t
(** A parsed frontmatter document. *)

type yaml = Yamlrw.value
(** YAML value type from yamlrw. *)

(** {1 Parsing} *)

val of_string : ?fname:string -> string -> (t, string) result
(** Parse a string containing YAML frontmatter.

    The input should have YAML delimited by '---' markers at the start.
    Everything after the closing '---' is the body.

    @param fname Optional filename for error messages.
    @return Parsed frontmatter or an error message. *)

val of_string_exn : ?fname:string -> string -> t
(** Like {!of_string} but raises [Failure] on parse error. *)

(** {1 Accessors} *)

val yaml : t -> yaml
(** Get the raw YAML value from the frontmatter. *)

val body : t -> string
(** Get the body content after the frontmatter. *)

val fname : t -> string option
(** Get the filename if one was provided during parsing. *)

(** {1 Field Access}

    Convenience functions for accessing common field types. *)

val find : string -> t -> yaml option
(** [find key fm] looks up [key] in the frontmatter YAML. *)

val find_string : string -> t -> string option
(** [find_string key fm] gets a string field from frontmatter. *)

val find_strings : string -> t -> string list
(** [find_strings key fm] gets a string list field, returning empty list
    if not found or not a list. *)

val find_bool : string -> t -> bool option
(** [find_bool key fm] gets a boolean field. *)

val find_int : string -> t -> int option
(** [find_int key fm] gets an integer field. *)

val find_float : string -> t -> float option
(** [find_float key fm] gets a float field. *)

(** {1 Typed Decoding}

    Decode frontmatter using Jsont codecs for structured access. *)

val decode : 'a Jsont.t -> t -> ('a, string) result
(** [decode jsont fm] decodes the frontmatter YAML using the given codec.

    Uses {!Yamlt.decode_value} to interpret the YAML value directly through
    the Jsont codec. *)

val decode_exn : 'a Jsont.t -> t -> 'a
(** Like {!decode} but raises [Failure] on decode error. *)

(** {1 Slug Extraction}

    Jekyll-style filename slug extraction. *)

val slug_of_fname : string -> (string * Ptime.t option, string) result
(** Extract slug and optional date from a Jekyll-style filename.

    Handles formats like:
    - [2025-01-15-my-post.md] -> [("my-post", Some date)]
    - [my-post.md] -> [("my-post", None)]

    @return Tuple of (slug, optional date) or error message. *)
