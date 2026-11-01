open Zulip_eio

let ok = function
  | Ok value -> value
  | Error error -> Alcotest.fail (Error.error_to_string error)

let find id snippets =
  List.find_opt
    (fun snippet -> Saved_snippets.Id.equal id (Saved_snippets.id snippet))
    snippets

let listed client id =
  let page = Saved_snippets.list client |> ok in
  match find id page.saved_snippets with
  | Some snippet -> snippet
  | None -> Alcotest.fail "created saved snippet was not listed"

let run ~client =
  let nonce =
    Printf.sprintf "%d-%.0f" (Unix.getpid ()) (Unix.gettimeofday () *. 1000.)
  in
  let initial_title = "OCaml saved snippet " ^ nonce in
  let initial_content = "Original **Markdown** content " ^ nonce in
  let edited_title = "Edited OCaml saved snippet " ^ nonce in
  let edited_content = "Edited `Markdown` content ☕ " ^ nonce in
  let saved_snippet_id = ref None in
  let cleanup () =
    Option.iter
      (fun id -> ignore (Saved_snippets.delete client ~saved_snippet_id:id))
      !saved_snippet_id
  in
  Fun.protect ~finally:cleanup @@ fun () ->
  let id =
    Saved_snippets.create client ~title:initial_title ~content:initial_content
    |> ok
  in
  saved_snippet_id := Some id;
  let created = listed client id in
  Alcotest.(check string)
    "created title" initial_title
    (Saved_snippets.title created);
  Alcotest.(check string)
    "created content" initial_content
    (Saved_snippets.content created);
  Alcotest.(check bool)
    "creation timestamp" true
    (Saved_snippets.date_created created > 0);

  Saved_snippets.edit client ~saved_snippet_id:id ~title:edited_title () |> ok;
  let title_edited = listed client id in
  Alcotest.(check string)
    "edited title" edited_title
    (Saved_snippets.title title_edited);
  Alcotest.(check string)
    "title edit preserves content" initial_content
    (Saved_snippets.content title_edited);

  Saved_snippets.edit client ~saved_snippet_id:id ~content:edited_content ()
  |> ok;
  let content_edited = listed client id in
  Alcotest.(check string)
    "content edit preserves title" edited_title
    (Saved_snippets.title content_edited);
  Alcotest.(check string)
    "edited content" edited_content
    (Saved_snippets.content content_edited);

  Saved_snippets.delete client ~saved_snippet_id:id |> ok;
  saved_snippet_id := None;
  Alcotest.(check bool)
    "deleted snippet is absent" true
    (Option.is_none (find id (Saved_snippets.list client |> ok).saved_snippets))
