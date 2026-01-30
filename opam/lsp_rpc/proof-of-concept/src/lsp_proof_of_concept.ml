open Core
open Async
open Lsp_rpc

(* This is only a rough proof-of-concept. It is not a suggestion for how the code should
   be organized and is surely missing logic that a robust implementation would include. *)

module Server = struct
  include Server

  let to_character_offset t text offset =
    let encoding = (encoding t :> [ `UTF_8 | `UTF_16 | `UTF_32 ]) in
    Utf_offset_conv.convert_offset
      offset
      ~from:{ encoding = `UTF_8; units = `Bytes }
      ~to_:{ encoding; units = `Code_units }
      ~text_encoding:(module String.Utf8)
      ~text:(String.Utf8.of_string_unchecked text)
  ;;

  let create reader writer =
    let client_capabilities : Lsp.Types.ClientCapabilities.t option ref = ref None in
    let publish_diagnostics t ~uri ~text ~version =
      let diagnostics =
        (* We will call dollar signs illegal characters, and consecutive dollar signs are
           treated with a single diagnostic that spans the range. Although the algorithm
           could be implemented more efficiently by incorporating knowledge of the target
           encoding, the goal here is to demonstrate logic written assuming UTF-8 with
           position conversions pushed to the edge. *)
        text
        |> String.split ~on:'\n'
        |> List.concat_mapi ~f:(fun row line ->
          let illegal_ranges =
            line
            |> String.to_list
            |> List.foldi ~init:[] ~f:(fun col acc char ->
              match char, acc with
              | '$', (start, end_) :: acc when end_ = col -> (start, col + 1) :: acc
              | '$', _ -> (col, col + 1) :: acc
              | _ -> acc)
          in
          List.map illegal_ranges ~f:(fun (start, end_) ->
            let open Lsp.Types in
            let create_position ~col =
              Position.create ~line:row ~character:(to_character_offset t line col)
            in
            let start = create_position ~col:start in
            let end_ = create_position ~col:end_ in
            let range = Range.create ~start ~end_ in
            Diagnostic.create ~range ~message:(`String "Illegal character: '$'") ()))
      in
      let version =
        let open Option.Let_syntax in
        let%bind capabilities = !client_capabilities in
        let%bind text_document = capabilities.textDocument in
        let%bind publish_diagnostics = text_document.publishDiagnostics in
        match%bind publish_diagnostics.versionSupport with
        | false -> None
        | true -> Some version
      in
      Server.notify t (PublishDiagnostics { uri; version; diagnostics })
    in
    let t_ref = Set_once.create () in
    let t =
      create
        reader
        writer
        ~on_notification:(fun notification ->
          let t = Set_once.get_exn t_ref in
          match notification with
          | T (TextDocumentDidOpen { textDocument = { uri; text; version; _ } }) ->
            publish_diagnostics t ~uri ~text ~version |> Deferred.ignore_m
          | T (TextDocumentDidClose { textDocument = { uri } }) ->
            Server.notify t (PublishDiagnostics { uri; version = None; diagnostics = [] })
            |> Deferred.ignore_m
          | T (TextDocumentDidChange { textDocument = { uri; version }; contentChanges })
            ->
            let name = Lsp.Uri.to_string uri in
            (match contentChanges with
             | [ { text; range = None; rangeLength = None } ] ->
               publish_diagnostics t ~uri ~text ~version |> Deferred.ignore_m
             | _ ->
               Server.notify
                 t
                 (ShowMessage
                    { type_ = Error
                    ; message =
                        sprintf "Unable to apply incremental update to document: %s" name
                    })
               |> Deferred.ignore_m)
          | T Exit
          | T Initialized
          | T (SetTrace _)
          | T (DidSaveTextDocument _)
          | T (WillSaveTextDocument _)
          | T (DidChangeWatchedFiles _)
          | T (DidCreateFiles _)
          | T (DidDeleteFiles _)
          | T (DidRenameFiles _)
          | T (ChangeWorkspaceFolders _)
          | T (ChangeConfiguration _)
          | T (CancelRequest _)
          | T (WorkDoneProgress _)
          | T (WorkDoneProgressCancel _)
          | T (UnknownNotification _)
          | T (NotebookDocumentDidOpen _)
          | T (NotebookDocumentDidChange _)
          | T (NotebookDocumentDidSave _)
          | T (NotebookDocumentDidClose _)
          | Invalid_params _ -> return ())
        ~on_request:
          { f =
              (fun (type a) request ~cancelled:_ : (a, _) Deferred.Result.t ->
                match (request : a Lsp.Client_request.t) with
                | Initialize { capabilities; _ } ->
                  client_capabilities := Some capabilities;
                  let open Lsp.Types in
                  let textDocumentSync =
                    `TextDocumentSyncKind TextDocumentSyncKind.Full
                  in
                  let capabilities = ServerCapabilities.create ~textDocumentSync () in
                  InitializeResult.create ~capabilities () |> Deferred.Result.return
                | Shutdown -> Deferred.Result.return ()
                | InlayHint _
                | TextDocumentHover _
                | TextDocumentDefinition _
                | TextDocumentDeclaration _
                | TextDocumentTypeDefinition _
                | TextDocumentImplementation _
                | TextDocumentCompletion _
                | TextDocumentCodeLens _
                | TextDocumentCodeLensResolve _
                | TextDocumentPrepareCallHierarchy _
                | TextDocumentPrepareRename _
                | TextDocumentRangeFormatting _
                | TextDocumentRename _
                | TextDocumentLink _
                | TextDocumentLinkResolve _
                | TextDocumentMoniker _
                | DocumentSymbol _
                | WorkspaceSymbol _
                | DebugEcho _
                | DebugTextDocumentGet _
                | TextDocumentReferences _
                | TextDocumentHighlight _
                | TextDocumentFoldingRange _
                | SignatureHelp _
                | CodeAction _
                | CodeActionResolve _
                | CompletionItemResolve _
                | WillSaveWaitUntilTextDocument _
                | TextDocumentFormatting _
                | TextDocumentOnTypeFormatting _
                | TextDocumentColorPresentation _
                | TextDocumentColor _
                | SelectionRange _
                | ExecuteCommand _
                | SemanticTokensFull _
                | SemanticTokensDelta _
                | SemanticTokensRange _
                | LinkedEditingRange _
                | CallHierarchyIncomingCalls _
                | CallHierarchyOutgoingCalls _
                | WillCreateFiles _
                | WillDeleteFiles _
                | WillRenameFiles _
                | InlayHintResolve _
                | TextDocumentDiagnostic _
                | TextDocumentInlineCompletion _
                | TextDocumentInlineValue _
                | TextDocumentPrepareTypeHierarchy _
                | TextDocumentRangesFormatting _
                | WorkspaceSymbolResolve _
                | WorkspaceDiagnostic _
                | TypeHierarchySubtypes _
                | TypeHierarchySupertypes _
                | UnknownRequest _ -> Deferred.Result.fail `Method_not_implemented)
          }
        ~on_error:Rpc_error.raise
    in
    Set_once.set_exn t_ref t;
    t
  ;;
end

let command =
  Command.async
    ~summary:"Proof-of-concept LSP"
    (let%map_open.Command () = return () in
     fun () ->
       let (_ : Server.t) = Server.create (force Reader.stdin) (force Writer.stdout) in
       Deferred.never ())
;;

module Client = struct
  include Client

  let initialize_server ?positionEncodings t =
    let open Lsp.Types in
    let params =
      let publishDiagnostics =
        PublishDiagnosticsClientCapabilities.create ~versionSupport:true ()
      in
      let textDocument = TextDocumentClientCapabilities.create ~publishDiagnostics () in
      let general = GeneralClientCapabilities.create ?positionEncodings () in
      let capabilities = ClientCapabilities.create ~general ~textDocument () in
      InitializeParams.create ~capabilities ()
    in
    let%bind (_ : InitializeResult.t) = call t (Initialize params) >>| ok_exn in
    notify t Initialized >>| ok_exn
  ;;

  let create =
    create
      ~on_notification:(fun notification ->
        match notification with
        | T (PublishDiagnostics diagnostics) ->
          let json = Lsp.Types.PublishDiagnosticsParams.yojson_of_t diagnostics in
          Yojson.Safe.pretty_to_channel stdout json;
          print_newline ();
          return ()
        | T (ShowMessage { type_; message }) | T (LogMessage { type_; message }) ->
          let type_ =
            match type_ with
            | Error -> "Error"
            | Warning -> "Warning"
            | Info -> "Info"
            | Log -> "Log"
            | Debug -> "Debug"
          in
          Core.printf "[%s]: %s\n" type_ message;
          return ()
        | _ -> return ())
      ~on_request:
        { f =
            (fun (_ : _ Lsp.Server_request.t) ~cancelled:_ ->
              Deferred.Result.fail `Method_not_implemented)
        }
      ~on_error:Rpc_error.raise
  ;;
end

let create_connected_client_and_server () =
  let read_from_client, write_to_client = Pipe.create () in
  let read_from_server, write_to_server = Pipe.create () in
  let%map read_from_client = Reader.of_pipe (Info.of_string "client") read_from_client
  and read_from_server = Reader.of_pipe (Info.of_string "server") read_from_server
  and write_to_client, `Closed_and_flushed_downstream flushed_to_client =
    Writer.of_pipe (Info.of_string "client") write_to_client
  and write_to_server, `Closed_and_flushed_downstream flushed_to_server =
    Writer.of_pipe (Info.of_string "server") write_to_server
  in
  let client = Client.create read_from_client write_to_server in
  let server = Server.create read_from_server write_to_client in
  let shutdown () =
    let%bind () = Client.call client Shutdown >>| ok_exn in
    let%bind () = Client.notify client Exit >>| ok_exn in
    (* Outside of tests, sending the [Exit] notification would trigger an exit system call
       in the server. In the test that doesn't happen, so we close the client writer,
       indicating to the server that there are no further messages and it should close the
       connection. *)
    let%bind () = Writer.close write_to_server in
    [ Writer.close_finished write_to_client
    ; Writer.close_finished write_to_server
    ; Reader.close_finished read_from_client
    ; Reader.close_finished read_from_server
    ; flushed_to_client
    ; flushed_to_server
    ]
    |> Deferred.all_unit
  in
  client, server, shutdown
;;

let%expect_test "Basic test of LSP client/server communication" =
  let%bind client, _server, shutdown = create_connected_client_and_server () in
  let%bind () = Client.initialize_server client ~positionEncodings:[ UTF8 ] in
  let uri = Lsp.Uri.of_path "/path/to/file" in
  let languageId = "test-lang" in
  let text =
    {|
    This i$
    a te$t sentence
    with many $$$'s in it. |}
  in
  let%bind () =
    Client.notify
      client
      (TextDocumentDidOpen { textDocument = { uri; languageId; version = 1; text } })
    >>| ok_exn
  in
  let%bind () =
    Client.notify
      client
      (TextDocumentDidChange
         { textDocument = { uri; version = 2 }
         ; contentChanges =
             [ { range = None; rangeLength = None; text = "The text ha$ changed" } ]
         })
    >>| ok_exn
  in
  let%bind () =
    Client.notify client (TextDocumentDidClose { textDocument = { uri } }) >>| ok_exn
  in
  let%bind () =
    (* Wait for the text document closure to be processed before shutting down. *)
    Scheduler.yield_until_no_jobs_remain ()
  in
  let%bind () =
    match%map with_timeout (Time_float_unix.Span.of_int_sec 5) (shutdown ()) with
    | `Result () -> ()
    | `Timeout -> print_s [%message "Timed out waiting for shutdown"]
  in
  [%expect
    {|
    {
      "diagnostics": [
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 1, "character": 10 },
            "end": { "line": 1, "character": 11 }
          }
        },
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 2, "character": 8 },
            "end": { "line": 2, "character": 9 }
          }
        },
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 3, "character": 14 },
            "end": { "line": 3, "character": 17 }
          }
        }
      ],
      "uri": "file:///path/to/file",
      "version": 1
    }
    {
      "diagnostics": [
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 0, "character": 11 },
            "end": { "line": 0, "character": 12 }
          }
        }
      ],
      "uri": "file:///path/to/file",
      "version": 2
    }
    { "diagnostics": [], "uri": "file:///path/to/file" }
    |}];
  return ()
;;

let%expect_test "Test proper handling of UTF-8 and UTF-16" =
  let test ~encoding =
    let text = "a𐐀$" in
    let byte_length =
      match encoding with
      | `UTF_8 -> Uchar.Utf8.byte_length
      | `UTF_16 -> Uchar.Utf16le.byte_length
    in
    let code_units_of_offset =
      match encoding with
      | `UTF_8 -> Fn.id
      | `UTF_16 ->
        fun offset ->
          assert (offset mod 2 = 0);
          offset / 2
    in
    text
    |> String.Utf8.of_string
    |> String.Utf8.to_list
    |> List.folding_map ~init:0 ~f:(fun offset uchar ->
      let code_units = code_units_of_offset offset in
      offset + byte_length uchar, [%string "(%{code_units#Int} %{uchar#Uchar.Utf8})"])
    |> String.concat ~sep:" "
    |> printf "Character offsets in text: (%s)\n";
    let%bind client, _server, shutdown = create_connected_client_and_server () in
    let%bind () =
      let (positionEncodings : Lsp.Types.PositionEncodingKind.t list option) =
        match encoding with
        | `UTF_8 -> Some [ UTF8; UTF16 ]
        | `UTF_16 -> None
      in
      Client.initialize_server client ?positionEncodings
    in
    let uri = Lsp.Uri.of_path "/path/to/file" in
    let languageId = "test-lang" in
    let%bind () =
      Client.notify
        client
        (TextDocumentDidOpen { textDocument = { uri; languageId; version = 1; text } })
      >>| ok_exn
    in
    let%bind () =
      Client.notify client (TextDocumentDidClose { textDocument = { uri } }) >>| ok_exn
    in
    let%bind () =
      (* Wait for the text document closure to be processed before shutting down. *)
      Scheduler.yield_until_no_jobs_remain ()
    in
    match%map with_timeout (Time_float_unix.Span.of_int_sec 5) (shutdown ()) with
    | `Result () -> ()
    | `Timeout -> print_s [%message "Timed out waiting for shutdown"]
  in
  let%bind () = test ~encoding:`UTF_16 in
  [%expect
    {|
    Character offsets in text: ((0 a) (1 𐐀) (3 $))
    {
      "diagnostics": [
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 0, "character": 3 },
            "end": { "line": 0, "character": 4 }
          }
        }
      ],
      "uri": "file:///path/to/file",
      "version": 1
    }
    { "diagnostics": [], "uri": "file:///path/to/file" }
    |}];
  let%bind () = test ~encoding:`UTF_8 in
  [%expect
    {|
    Character offsets in text: ((0 a) (1 𐐀) (5 $))
    {
      "diagnostics": [
        {
          "message": "Illegal character: '$'",
          "range": {
            "start": { "line": 0, "character": 5 },
            "end": { "line": 0, "character": 6 }
          }
        }
      ],
      "uri": "file:///path/to/file",
      "version": 1
    }
    { "diagnostics": [], "uri": "file:///path/to/file" }
    |}];
  return ()
;;
