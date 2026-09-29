let fail message = raise (Failure message)

let contains ~needle haystack =
  let rec search offset =
    let remaining = String.length haystack - offset in
    if remaining < String.length needle then false
    else if String.sub haystack offset (String.length needle) = needle then true
    else search (offset + 1)
  in
  search 0

let printed pp value = Format.asprintf "%a" pp value

type point = { x : int; y : int option }
type scalar = Text of string | Count of int
type node = { value : int; next : node option }

let () =
  let open Apple_fm.Schema in
  let path = property ~description:"A path" "path" string in
  let limit = property ~optional:true "limit" integer in
  (match object_ ~name:"SearchArguments" [ path; limit ] with
  | Ok _ -> ()
  | Error message -> fail message);
  (match object_ ~name:"1bad" [ path ] with
  | Error _ -> ()
  | Ok _ -> fail "accepted an invalid schema name");
  (match object_ ~name:"Duplicate" [ path; path ] with
  | Error _ -> ()
  | Ok _ -> fail "accepted duplicate property names");
  (match object_ ~name:"NoArguments" [] with
  | Ok _ -> ()
  | Error message -> fail ("rejected an empty object schema: " ^ message));
  (match object_ ~name:"Bad-name" [ path ] with
  | Error _ -> ()
  | Ok _ -> fail "accepted punctuation in a schema name");
  (match one_of ~name:"Choice" [ "a"; "a" ] with
  | Error _ -> ()
  | Ok _ -> fail "accepted duplicate choices");
  (match array ~minimum:2 ~maximum:1 string with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted inconsistent array bounds");
  let guided = string_guided ~choices:[ "red"; "blue" ] ~pattern:"[a-z]+" () in
  if not (contains ~needle:"pattern" (printed pp guided)) then
    fail "string guide was not printed";
  (match string_guided ~choices:[ "same"; "same" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted duplicate string guide choices");
  (match string_guided ~constant:"red" ~choices:[ "blue" ] () with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted a constant outside its guided choices");
  let ranged = integer_range ~minimum:1 ~maximum:10 () in
  if not (contains ~needle:"minimum" (printed pp ranged)) then
    fail "integer guide was not printed";
  let number_range =
    number_range ~minimum:0.25 ~maximum:0.75 () |> printed pp
  in
  if
    not
      (contains ~needle:{|"minimum":0.25|} number_range
      && contains ~needle:{|"maximum":0.75|} number_range)
  then fail "number guide was not encoded";
  let bounded_array = array ~minimum:1 ~maximum:2 string |> printed pp in
  if
    not
      (contains ~needle:{|"items":{"type":"string"}|} bounded_array
      && contains ~needle:{|"minimum_items":1|} bounded_array
      && contains ~needle:{|"maximum_items":2|} bounded_array)
  then fail "array schema was not encoded";
  let choice = Result.get_ok (any_of ~name:"Scalar" [ string; number; null ]) in
  if not (contains ~needle:"any_of" (printed pp choice)) then
    fail "any-of schema was not printed";
  let recursive =
    Result.get_ok
      (object_ ~name:"Node"
         [ property ~optional:true "next" (reference "Node") ])
  in
  let recursive_document =
    with_dependencies (reference "Node") [ recursive ] |> printed pp
  in
  if
    not
      (contains ~needle:{|"root":{"type":"reference","name":"Node"}|}
         recursive_document
      && contains ~needle:{|"dependencies":[{"type":"object"|}
           recursive_document)
  then fail "recursive schema document was not encoded";
  if
    not
      (contains ~needle:{|"type":"object"|}
         (printed pp (Result.get_ok (object_ ~name:"Search" [ path ]))))
  then fail "schema printer did not produce JSON";
  let codec =
    let open Apple_fm.Codec in
    Invoke.map "search" (fun path limit -> (path, limit))
    |> Invoke.param ~enc:fst "path" string ~description:"path to search"
    |> Invoke.param ~enc:snd ~default:10 "limit" int
    |> Invoke.seal
  in
  let search =
    Apple_fm.Tool.v ~description:"Search a path." codec (fun (path, limit) ->
        Printf.sprintf "%s:%d" path limit)
  in
  let empty_codec =
    Apple_fm.Codec.Invoke.map "now" () |> Apple_fm.Codec.Invoke.seal
  in
  let no_arguments =
    Apple_fm.Tool.v ~description:"Return the time." empty_codec (fun () ->
        "noon")
  in
  if Apple_fm.Tool.invoke no_arguments "{}" <> "noon" then
    fail "did not invoke a zero-argument tool";
  if Apple_fm.Tool.name search <> "search" then fail "lost the codec name";
  if not (contains ~needle:"search" (printed Apple_fm.Tool.pp search)) then
    fail "tool printer lost the tool name";
  if Apple_fm.Tool.invoke search {|{"path":"lib"}|} <> "lib:10" then
    fail "did not decode a defaulted argument";
  if
    not
      (String.starts_with ~prefix:"Error:"
         (Apple_fm.Tool.invoke search {|{"path":3}|}))
  then fail "did not report an invalid argument";
  let json_tool =
    Apple_fm.Tool.v_json ~description:"Return a number." ~output:Jsont.int
      empty_codec (fun () -> 3)
  in
  if Apple_fm.Tool.invoke json_tool "{}" <> "3" then
    fail "did not encode structured tool output";
  if Apple_fm.Tool.invoke_json no_arguments "{}" <> {|"noon"|} then
    fail "did not expose the JSON text observation";
  let quoted_tool =
    Apple_fm.Tool.v ~description:"Quote." empty_codec (fun () -> "a\"b")
  in
  if Apple_fm.Tool.invoke_json quoted_tool "{}" <> {|"a\"b"|} then
    fail "did not JSON-encode a text observation";
  if not (Apple_fm.Tool.includes_schema_in_instructions no_arguments) then
    fail "lost the schema-in-instructions setting";
  let direct_codec =
    let open Apple_fm.Codec in
    Invoke.map "direct" Fun.id
    |> Invoke.param ~enc:Fun.id "value" int
    |> Invoke.seal
  in
  let mapped_codec =
    Apple_fm.Codec.map ~dec:string_of_int ~enc:int_of_string direct_codec
  in
  (match Apple_fm.Codec.decode_arguments mapped_codec {|{"value":7}|} with
  | Ok "7" -> ()
  | Ok value -> fail ("mapped codec returned " ^ value)
  | Error message -> fail message);
  (match Apple_fm.Codec.encode_arguments mapped_codec "7" with
  | Ok {|{"value":7}|} -> ()
  | Ok json -> fail ("mapped codec encoded " ^ json)
  | Error message -> fail message);
  let point_value =
    let open Apple_fm.Codec in
    Object.map "Point" (fun x y -> { x; y })
    |> Object.param ~enc:(fun point -> point.x) "x" int
    |> Object.optional ~enc:(fun point -> point.y) "y" int
    |> Object.seal
  in
  let point_codec =
    let open Apple_fm.Codec in
    Invoke.map "point" Fun.id
    |> Invoke.param ~enc:Fun.id "point" point_value
    |> Invoke.seal
  in
  let point_tool =
    Apple_fm.Tool.v ~description:"Point." point_codec (fun point ->
        Printf.sprintf "%d:%s" point.x
          (Option.fold ~none:"none" ~some:string_of_int point.y))
  in
  if Apple_fm.Tool.invoke point_tool {|{"point":{"x":3}}|} <> "3:none" then
    fail "object codec did not decode an optional member";
  let scalar_value =
    let open Apple_fm.Codec in
    any_of ~name:"Scalar"
      [
        case string
          ~inject:(fun value -> Text value)
          ~project:(function Text value -> Some value | Count _ -> None);
        case int
          ~inject:(fun value -> Count value)
          ~project:(function Count value -> Some value | Text _ -> None);
      ]
  in
  let scalar_codec =
    let open Apple_fm.Codec in
    Invoke.map "scalar" Fun.id
    |> Invoke.param ~enc:Fun.id "value" scalar_value
    |> Invoke.seal
  in
  let scalar_tool =
    Apple_fm.Tool.v ~description:"Scalar." scalar_codec (function
      | Text value -> value
      | Count value -> string_of_int value)
  in
  if Apple_fm.Tool.invoke scalar_tool {|{"value":4}|} <> "4" then
    fail "union codec did not decode its integer alternative";
  let node_value =
    let open Apple_fm.Codec in
    recursive ~name:"Node" @@ fun self ->
    Object.map "Node" (fun value next -> { value; next })
    |> Object.param ~enc:(fun node -> node.value) "value" int
    |> Object.optional ~enc:(fun node -> node.next) "next" self
    |> Object.seal
  in
  let node_codec =
    let open Apple_fm.Codec in
    Invoke.map "node" Fun.id
    |> Invoke.param ~enc:Fun.id "node" node_value
    |> Invoke.seal
  in
  let node_tool =
    Apple_fm.Tool.v ~description:"Node." node_codec (fun node ->
        match node.next with
        | None -> string_of_int node.value
        | Some next -> Printf.sprintf "%d:%d" node.value next.value)
  in
  if
    Apple_fm.Tool.invoke node_tool {|{"node":{"value":1,"next":{"value":2}}}|}
    <> "1:2"
  then fail "recursive codec did not decode a recursive object";
  if not (contains ~needle:"dependencies" (printed Apple_fm.Tool.pp node_tool))
  then fail "recursive codec lost its Apple schema dependency";
  (match
     Apple_fm.Codec.Invoke.map "bad-name" () |> Apple_fm.Codec.Invoke.seal
   with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted an invalid direct tool name");
  let bounded_codec =
    let open Apple_fm.Codec in
    Invoke.map "rate" Fun.id
    |> Invoke.param ~enc:Fun.id "rating" (int_range ~minimum:1 ~maximum:10 ())
    |> Invoke.seal
  in
  let bounded_tool =
    Apple_fm.Tool.v ~description:"Rate." bounded_codec string_of_int
  in
  if Apple_fm.Tool.invoke bounded_tool {|{"rating":5}|} <> "5" then
    fail "bounded integer rejected a valid value";
  if
    not
      (String.starts_with ~prefix:"Error:"
         (Apple_fm.Tool.invoke bounded_tool {|{"rating":11}|}))
  then fail "bounded integer accepted an out-of-range value";
  let choices_codec =
    let open Apple_fm.Codec in
    Invoke.map "colour" Fun.id
    |> Invoke.param ~enc:Fun.id "value"
         (string_guided ~choices:[ "red"; "blue" ] ())
    |> Invoke.seal
  in
  let choices_tool =
    Apple_fm.Tool.v ~description:"Colour." choices_codec Fun.id
  in
  if
    not
      (String.starts_with ~prefix:"Error:"
         (Apple_fm.Tool.invoke choices_tool {|{"value":"green"}|}))
  then fail "guided string accepted a value outside its choices";
  let array_codec =
    let open Apple_fm.Codec in
    Invoke.map "numbers" Fun.id
    |> Invoke.param ~enc:Fun.id "values" (array ~minimum:1 ~maximum:2 int)
    |> Invoke.seal
  in
  let array_tool =
    Apple_fm.Tool.v ~description:"Numbers." array_codec (fun values ->
        string_of_int (List.length values))
  in
  if
    not
      (String.starts_with ~prefix:"Error:"
         (Apple_fm.Tool.invoke array_tool {|{"values":[]}|}))
  then fail "bounded array accepted an invalid length";
  let float_codec =
    let open Apple_fm.Codec in
    Invoke.map "probability" Fun.id
    |> Invoke.param ~enc:Fun.id "value" (float_range ~minimum:0. ~maximum:1. ())
    |> Invoke.seal
  in
  let float_tool =
    Apple_fm.Tool.v ~description:"Probability." float_codec string_of_float
  in
  if
    not
      (String.starts_with ~prefix:"Error:"
         (Apple_fm.Tool.invoke float_tool {|{"value":null}|}))
  then fail "number codec accepted JSON null";
  (match
     let open Apple_fm.Codec in
     Invoke.map "bad_default" Fun.id
     |> Invoke.param ~enc:Fun.id ~default:0 "value" (int_range ~minimum:1 ())
     |> Invoke.seal
   with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted a default outside its codec constraints");
  (match Apple_fm.Transcript.of_json "not json" with
  | Error _ -> ()
  | Ok _ -> fail "accepted an invalid transcript encoding");
  ignore
    (Apple_fm.Prompt.v
       ~images:[ Apple_fm.Prompt.image "/tmp/image.png" ]
       "inspect");
  (match Apple_fm.Prompt.image "relative.png" with
  | exception Invalid_argument _ -> ()
  | _ -> fail "accepted a relative image path");
  let cancelled = Eio.Cancel.Cancelled Exit in
  let cancelled_tool =
    Apple_fm.Tool.v ~description:"Cancel." codec (fun _ -> raise cancelled)
  in
  (match Apple_fm.Tool.invoke cancelled_tool {|{"path":"lib"}|} with
  | exception Eio.Cancel.Cancelled Exit -> ()
  | _ -> fail "tool invocation swallowed Eio cancellation");
  if printed Apple_fm.Availability.pp `Available <> "available" then
    fail "availability printer failed";
  if
    printed Apple_fm.Generation.pp_sampling (`Top_k { k = 8; seed = Some 1L })
    <> "top-k 8, seed 1"
  then fail "sampling printer failed";
  let options =
    Apple_fm.Generation.options ~temperature:0.5 ~maximum_response_tokens:20 ()
  in
  if
    not
      (contains ~needle:"temperature=0.5"
         (printed Apple_fm.Generation.pp_options options))
  then fail "options printer failed";
  let io_error =
    Eio.Exn.add_context
      (Eio.Exn.create (Apple_fm.Error.E `Closed))
      "while testing"
  in
  let message = printed Eio.Exn.pp io_error in
  if
    not
      (contains ~needle:"Foundation Models: the session is closed" message
      && contains ~needle:"while testing" message)
  then fail "Eio error printer lost its error or context";
  let context_error =
    `Context_size_exceeded
      ({ context_size = Some 4096; token_count = Some 5000; message = "large" }
        : Apple_fm.Error.context_size_exceeded)
  in
  if
    not
      (contains ~needle:"limit 4096" (printed Apple_fm.Error.pp context_error))
  then fail "typed error printer lost context counts";
  ignore (printed Apple_fm.Model.pp Apple_fm.Model.default);
  ignore
    (printed Apple_fm.Context.pp
       (Apple_fm.Context.create ~reasoning_level:`Deep ()));
  ignore
    (Apple_fm.Generation.options
       ~sampling:(`Top_k { k = 8; seed = Some 1L })
       ());
  let invalid_options name make =
    match make () with
    | exception Invalid_argument _ -> ()
    | _ -> fail ("accepted invalid " ^ name)
  in
  invalid_options "token limit" (fun () ->
      Apple_fm.Generation.options ~maximum_response_tokens:0 ());
  invalid_options "infinite temperature" (fun () ->
      Apple_fm.Generation.options ~temperature:Float.infinity ());
  invalid_options "temperature above one" (fun () ->
      Apple_fm.Generation.options ~temperature:1.01 ());
  invalid_options "infinite probability threshold" (fun () ->
      Apple_fm.Generation.options
        ~sampling:(`Probability { threshold = Float.infinity; seed = None })
        ());
  if Sys.int_size > 32 then (
    let too_large = Int32.to_int Int32.max_int + 1 in
    invalid_options "token limit outside the C ABI" (fun () ->
        Apple_fm.Generation.options ~maximum_response_tokens:too_large ());
    invalid_options "top-k outside the C ABI" (fun () ->
        Apple_fm.Generation.options
          ~sampling:(`Top_k { k = too_large; seed = None })
          ()))
