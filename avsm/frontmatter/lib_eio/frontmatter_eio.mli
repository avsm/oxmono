(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Eio file I/O support for frontmatter.

    {1 Reading Files}

    {[
      Eio_main.run @@ fun env ->
      let fs = Eio.Stdenv.fs env in
      match Frontmatter_eio.of_file fs "posts/2025-01-15-my-post.md" with
      | Ok fm ->
        let title = Frontmatter.find_string "title" fm in
        ...
      | Error msg -> Printf.eprintf "Error: %s\n" msg
    ]}
*)

val of_file : _ Eio.Path.t -> string -> (Frontmatter.t, string) result
(** [of_file fs path] reads and parses a frontmatter file.

    @param fs Eio filesystem capability
    @param path Path to the file to read
    @return Parsed frontmatter or error message *)

val of_file_exn : _ Eio.Path.t -> string -> Frontmatter.t
(** Like {!of_file} but raises [Failure] on error. *)

val read_string : _ Eio.Path.t -> string -> string
(** [read_string fs path] reads a file as a string.

    Helper function for reading file contents. *)
