open Zulip_eio

let title = "OCaml API example"
let content = "A temporary saved snippet with **Markdown**."
let edited_content = "The temporary snippet now contains `edited Markdown`."

let run context =
  let client = Zulip_bot.Context.client context in
  let saved_snippet_id =
    Saved_snippets.create client ~title ~content |> Error.or_raise
  in
  let deleted = ref false in
  Fun.protect
    ~finally:(fun () ->
      if not !deleted then
        match Saved_snippets.delete client ~saved_snippet_id with
        | Ok () -> ()
        | Error error ->
            Format.eprintf "Could not remove temporary snippet: %a@." Error.pp
              error)
    (fun () ->
      Saved_snippets.edit client ~saved_snippet_id ~content:edited_content ()
      |> Error.or_raise;
      let page = Saved_snippets.list client |> Error.or_raise in
      let snippet =
        List.find
          (fun snippet ->
            Saved_snippets.Id.equal saved_snippet_id (Saved_snippets.id snippet))
          page.saved_snippets
      in
      Format.printf "Saved snippet %d: %s@.%s@."
        (Saved_snippets.Id.to_int (Saved_snippets.id snippet))
        (Saved_snippets.title snippet)
        (Saved_snippets.content snippet);
      Saved_snippets.delete client ~saved_snippet_id |> Error.or_raise;
      deleted := true)

let () =
  Zulip_bot_cli.Main.run_once ~name:"snippets"
    ~doc:"Create, edit, list, and delete a temporary saved snippet." run ()
