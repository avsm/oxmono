let html markdown =
  let block c = function
    | Cmarkit.Block.Heading (heading, _) ->
        let module C = Cmarkit_renderer.Context in
        C.string c "<p><strong>";
        C.inline c (Cmarkit.Block.Heading.inline heading);
        C.string c "</strong></p>\n";
        true
    | _ -> false
  in
  let renderer =
    Cmarkit_renderer.compose
      (Cmarkit_html.renderer ~safe:true ())
      (Cmarkit_renderer.make ~block ())
  in
  Cmarkit.Doc.of_string ~strict:false ~heading_auto_ids:false markdown
  |> Cmarkit_renderer.doc_to_string renderer
  |> Matrix_ui.Presentation.Html.sanitize
