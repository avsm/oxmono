(*---------------------------------------------------------------------------
  Copyright (c) 2026 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

let rec wait pid =
  try snd (Unix.waitpid [] pid) with
  | Unix.Unix_error (Unix.EINTR, _, _) -> wait pid

let capture args =
  match args with
  | [] -> invalid_arg "empty command"
  | program :: _ ->
      let input, output = Unix.pipe ~cloexec:true () in
      let pid = try Unix.create_process program (Array.of_list args)
          Unix.stdin output Unix.stderr with e ->
        Unix.close input; Unix.close output; raise e in
      Unix.close output;
      let ic = Unix.in_channel_of_descr input in
      let text = In_channel.input_all ic in
      close_in ic;
      match wait pid with
      | Unix.WEXITED 0 -> String.trim text
      | _ -> failwith (program ^ " failed")

let run ?(non_interactive = false) args =
  match args with
  | [] -> invalid_arg "empty command"
  | program :: _ ->
      let env = Unix.environment () in
      let args = if non_interactive && program = "git" then
          program :: "-c" :: "credential.interactive=false" :: List.tl args
        else args in
      let env = if not non_interactive then env else begin
        let ssh = Option.value ~default:"ssh"
            (Sys.getenv_opt "GIT_SSH_COMMAND") in
        let retained = Array.to_list env |> List.filter (fun s ->
          not (String.starts_with ~prefix:"GIT_TERMINAL_PROMPT=" s ||
               String.starts_with ~prefix:"GIT_SSH_COMMAND=" s)) in
        Array.of_list (["GIT_TERMINAL_PROMPT=0";
          "GIT_SSH_COMMAND=" ^ ssh ^ " -oBatchMode=yes"] @ retained)
      end in
      let pid = Unix.create_process_env program (Array.of_list args) env
          Unix.stdin Unix.stdout Unix.stderr in
      match wait pid with
      | Unix.WEXITED 0 -> ()
      | _ -> failwith (program ^ " failed")

let git path args =
  capture (["git"; "--no-optional-locks"; "-C"; path] @ args)
