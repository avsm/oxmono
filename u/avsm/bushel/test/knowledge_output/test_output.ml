let check message condition = if not condition then failwith message

let contains text part =
  let rec loop i =
    i + String.length part <= String.length text
    && (String.sub text i (String.length part) = part || loop (i + 1))
  in
  loop 0

let capture ppf run =
  Format.pp_print_flush ppf ();
  let original = Format.pp_get_formatter_out_functions ppf () in
  let style = Fmt.style_renderer ppf in
  let buffer = Buffer.create 256 in
  let spaces count = Buffer.add_string buffer (String.make count ' ') in
  Format.pp_set_formatter_out_functions ppf
    {
      original with
      out_string =
        (fun text pos len -> Buffer.add_substring buffer text pos len);
      out_flush = (fun () -> ());
      out_newline = (fun () -> Buffer.add_char buffer '\n');
      out_spaces = spaces;
      out_indent = spaces;
    };
  Fmt.set_style_renderer ppf `None;
  Fun.protect
    ~finally:(fun () ->
      Format.pp_print_flush ppf ();
      Format.pp_set_formatter_out_functions ppf original;
      Fmt.set_style_renderer ppf style)
    (fun () ->
      let result = run () in
      Format.pp_print_flush ppf ();
      (result, Buffer.contents buffer))

(* Presentation fixtures supply explicit neutral metadata that the real typed
   service always includes. Malformed values are left intact for codec checks. *)
let complete command encoded =
  match Jsont_bytesrw.decode_string Jsont.json encoded with
  | Error _ -> encoded
  | Ok (Jsont.Object _ as value) -> (
      let fields = function
        | Jsont.Object (xs, _) ->
            List.map (fun ((name, _), value) -> (name, value)) xs
        | _ -> []
      in
      let obj xs =
        Jsont.Json.object'
          (List.map (fun (k, v) -> Jsont.Json.mem (Jsont.Json.name k) v) xs)
      in
      let add defaults value =
        let values = fields value in
        obj
          (values
          @ List.filter (fun (k, _) -> not (List.mem_assoc k values)) defaults)
      in
      let str = Jsont.Json.string and num = Jsont.Json.int in
      let empty = Jsont.Json.null () in
      let value =
        if List.mem_assoc "error" (fields value) then value
        else
          match command with
          | "index" | "images-index" | "status" | "images-status" ->
              let value =
                add
                  [
                    ("database", str "");
                    ("generation", str "fixture");
                    ("documents", num 0);
                    ("passages", num 0);
                    ("embedding_provider", empty);
                    ("embedding_coverage", empty);
                  ]
                  value
              in
              obj
                (List.map
                   (fun (k, v) ->
                     if k = "embedding_provider" then
                       match v with
                       | Jsont.Object _ ->
                           ( k,
                             add
                               [
                                 ("max_tokens", num 128);
                                 ("query_instruction", str "");
                                 ("model_identity", str "fixture");
                               ]
                               v )
                       | _ -> (k, v)
                     else if
                       k = "documents" && List.mem_assoc "images" (fields value)
                     then (k, List.assoc "images" (fields value))
                     else (k, v))
                   (fields value))
          | "images-get" | "get" ->
              add
                [
                  ("revision", str "fixture");
                  ("url", str "");
                  ("next_offset", empty);
                ]
                value
          | "images-search" | "search" | "contacts" ->
              let value =
                add
                  [ ("generation", str "fixture"); ("next_offset", empty) ]
                  value
              in
              let value =
                if command = "contacts" then value
                else add [ ("bounded_results", Jsont.Json.bool true) ] value
              in
              obj
                (List.map
                   (fun (k, v) ->
                     match (k, v) with
                     | "items", Jsont.Array (items, _) ->
                         ( k,
                           Jsont.Json.list
                             (List.map
                                (add
                                   (if command = "contacts" then
                                      [ ("matched_on", str "") ]
                                    else
                                      [
                                        ("revision", str "fixture");
                                        ("date", str "");
                                        ("passage", num 0);
                                      ]))
                                items) )
                     | _ -> (k, v))
                   (fields value))
          | _ -> value
      in
      match Jsont_bytesrw.encode_string Jsont.json value with
      | Ok s -> s
      | Error e -> failwith e)
  | Ok _ -> encoded

let render command json =
  let ((), stdout), stderr =
    capture Fmt.stderr (fun () ->
        capture Fmt.stdout (fun () ->
            Knowledge_output.print ~elapsed:92. ~command (complete command json)))
  in
  check "terminal controls escaped"
    ((not (String.contains stdout '\027'))
    && not (String.contains stderr '\027'));
  (stdout, stderr)

let quiet_error command json =
  let stdout, stderr = render command json in
  check "error stays off stdout" (stdout = "");
  check "readable error on stderr" (contains stderr "Error: broken")

let () =
  let partial =
    {|{"newly_indexed":400,"embedding_coverage":{"indexed":400,"total":9095}}|}
  in
  let text, error = render "index" partial in
  check "partial text count and elapsed"
    (contains text "Indexed 400 passages in 1m32s."
    && contains text "400 of 9,095 ready (4%)"
    && contains text "8,695 remaining"
    && contains text "Resume: bushel knowledge index"
    && error = "");
  let text, _ = render "images-index" partial in
  check "image summary and resume command"
    (contains text "Indexed 400 images"
    && (not (contains text "passages"))
    && contains text "Resume: bushel knowledge images index");
  let text, _ =
    render "images-index"
      {|{"newly_indexed":0,"embedding_coverage":{"indexed":9,"total":9}}|}
  in
  check "cached images have honest completion"
    (contains text "Already up to date."
    && contains text "All 9 images ready."
    && not (contains text "Resume:"));
  let text, _ =
    render "images-index"
      {|{"newly_indexed":0,"skipped_images":4,"embedding_coverage":{"indexed":9,"total":9}}|}
  in
  check "image index reports skipped outputs separately from coverage"
    (contains text "All 9 images ready."
    && contains text "Skipped 4 images smaller than 32 pixels on either side.");
  let text, _ =
    render "images-index"
      {|{"newly_indexed":0,"skipped_images":1,"embedding_coverage":{"indexed":0,"total":0}}|}
  in
  check "an all-small corpus explains why there is nothing to index"
    (contains text "No images to index."
    && contains text "Skipped 1 image smaller than 32 pixels on either side.");
  let text, _ =
    render "images-status"
      {|{"documents":0,"skipped_images":2,"embedding_provider":null}|}
  in
  check "image status also reports skipped outputs"
    (contains text "Skipped 2 images smaller than 32 pixels on either side.");
  let text, _ =
    render "images-status"
      {|{"documents":12,"passages":12,"generation":"hidden-generation","embedding_provider":{"backend":"mlx-vision","model_dir":"/local/model","dimensions":2048,"model_identity":"hidden-hash","query_instruction":"hidden-instruction"},"embedding_coverage":{"indexed":2,"total":12},"database":"/cache/images.sqlite3"}|}
  in
  check "image status uses friendly metadata"
    (contains text "12 images"
    && contains text "Local MLX model"
    && contains text "2,048 dimensions"
    && contains text "bushel knowledge images index"
    && (not (contains text "hidden-"))
    && not (contains text "passages"));
  let text, _ =
    render "images-status"
      {|{"images":3,"documents":0,"embedding_provider":null}|}
  in
  check "explicit image count and configure hint"
    (contains text "3 images"
    && contains text "bushel knowledge images configure"
    && not (contains text "--backend"));
  let (), text =
    capture Fmt.stdout (fun () ->
        Knowledge_output.print_value ~command:"images-models"
          (Knowledge_output.Models
             {
               default = "qwen3-vl-2b";
               models =
                 [
                   {
                     name = "qwen3-vl-2b";
                     repo = "fixture";
                     revision = "fixture";
                     dimensions = 2048;
                     size_bytes = 1073741824L;
                     cached = true;
                     model_dir = "/fixture";
                     is_default = true;
                   };
                 ];
             }))
  in
  check "image catalogue display"
    (contains text "qwen3-vl-2b"
    && contains text "Ready" && contains text "default"
    && contains text "bushel knowledge images configure");
  let text, _ =
    render "images-search"
      {|{"items":[{"id":"img'one","title":"Coast\u001b[2J","kind":"image","url":"https://example.org/coast","pixel_path":"/images/coast.jpg","source_path":"/notes/coast.md","snippet":"A coastal view."}],"next_offset":20}|}
  in
  check "image hits are readable with copyable image get"
    (contains text "1. Coast" && contains text "[image]"
    && contains text "Image: /images/coast.jpg"
    && contains text "A coastal view."
    && contains text ("bushel knowledge images get " ^ Filename.quote "img'one")
    && contains text "--offset 20");
  let text, _ =
    render "images-get"
      {|{"id":"image:1","title":"Coast","kind":"image","pixel_path":"/images/coast.jpg","source_path":"/notes/coast.md","width":1920,"height":1080,"media_type":"image/jpeg","text":"First line.\nSecond line.","next_offset":24}|}
  in
  check "image source metadata and continuation"
    (contains text "Image: /images/coast.jpg"
    && contains text "Source: /notes/coast.md"
    && contains text "1,920 × 1,080 pixels"
    && contains text "First line.\nSecond line."
    && contains text "bushel knowledge images get 'image:1' --offset 24");
  let text, _ =
    render "contacts"
      {|{"status":"ambiguous","items":[{"id":"person:alice","name":"Alice","handle":"alice","emails":["alice@example.org"]}],"next_offset":null}|}
  in
  check "contact ambiguity and email retained"
    (contains text "Multiple matching contacts"
    && contains text "Alice (alice)"
    && contains text "alice@example.org");
  quiet_error "images-index" {|{"error":"broken\u001b[2J\nmessage"}|};
  check "malformed response raises for nonzero CLI exit"
    (try
       ignore (render "index" "{not-json");
       false
     with Failure _ -> true);
  List.iter
    (fun json ->
      check "non-object response cannot masquerade as a successful empty result"
        (try
           ignore (render "status" json);
           false
         with Failure _ -> true))
    [ "null"; "[]"; "true"; "42" ];
  check "unknown human command fails instead of returning success"
    (try
       ignore (render "unknown-command" "{}");
       false
     with Failure _ -> true);
  List.iter
    (fun encoded ->
      check "missing or wrongly typed status fields are rejected"
        (try
           Knowledge_output.print ~command:"status" encoded;
           false
         with Failure _ | Invalid_argument _ -> true))
    [
      "{}";
      {|{"documents":"many"}|};
      {|{"database":"x","generation":"g","documents":1,"passages":1,"embedding_provider":null,"embedding_coverage":{"indexed":2,"total":1}}|};
    ];
  print_endline "Human text and image knowledge output passed"
