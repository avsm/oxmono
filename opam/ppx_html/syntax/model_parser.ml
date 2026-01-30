open Core
open Ppxlib
open Angstrom
open Angstrom.Let_syntax
open Model

module Locations : sig
  type t

  val create : Location.t -> string -> t
  val location : t -> start:int -> end_:int -> Location.t
end = struct
  type t =
    { lines : position Int.Map.t
    ; text : string
    }

  let position ~lines i =
    let i', pos = Map.closest_key lines `Less_or_equal_to i |> Option.value_exn in
    { pos with pos_cnum = pos.pos_cnum - i' + i }
  ;;

  let create { loc_start; _ } text =
    let lines = Int.Map.empty in
    let lines = Map.set lines ~key:0 ~data:loc_start in
    let lines =
      String.foldi text ~init:lines ~f:(fun i lines c ->
        if Char.( <> ) c '\n'
        then lines
        else (
          let pos =
            { loc_start with
              pos_cnum = loc_start.pos_cnum + i + 1
            ; pos_bol = loc_start.pos_cnum + i + 1
            ; pos_lnum = loc_start.pos_lnum + Map.length lines
            }
          in
          Map.set lines ~key:(i + 1) ~data:pos))
    in
    { lines; text }
  ;;

  let position { lines; text } i =
    assert (0 <= i && i <= String.length text);
    position ~lines i
  ;;

  let location t ~start ~end_ =
    let loc_start = position t start
    and loc_end = position t end_ in
    { Location.loc_start; loc_end; loc_ghost = false }
  ;;
end

module Error = struct
  module Position = struct
    type t = Lexing.position =
      { pos_fname : string
      ; pos_lnum : int
      ; pos_bol : int
      ; pos_cnum : int
      }
    [@@deriving sexp]
  end

  module Location = struct
    include Location

    type t = Location.t =
      { loc_start : Position.t
      ; loc_end : Position.t
      ; loc_ghost : bool
      }
    [@@deriving sexp]
  end

  module T = struct
    type t =
      { message : string
      ; location : Location.t option
      }
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let of_string str =
    try of_string str with
    | _ -> { location = None; message = str }
  ;;

  let raise ~loc { location; message } =
    Location.raise_errorf ~loc:(Option.value location ~default:loc) "%s" message
  ;;
end

let ocaml_expression ({ txt; loc } : string Loc.t) =
  (* borrowed from [ppx_string] *)
  let lexbuf = Lexing.from_string txt in
  lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc.loc_start;
  Parse.expression lexbuf
;;

let with_loc' ~locs f =
  let%map start = pos
  and f
  and end_ = pos in
  let loc = Locations.location locs ~start ~end_ in
  f loc
;;

let with_loc ~locs txt =
  with_loc'
    ~locs
    (let%map txt in
     fun loc -> { txt; loc })
;;

let fail_but_we_know_location ~exn ~loc message =
  let message =
    assert (String.is_empty (Format.flush_str_formatter ()));
    Format.fprintf Format.str_formatter "%s\n" message;
    Location.report_exception Format.str_formatter exn;
    Format.flush_str_formatter ()
  in
  fail (Error.to_string { location = Some loc; message })
;;

let fail ?start ?end_ ~locs ?exn message =
  let%bind pos in
  let start = Option.value start ~default:pos in
  let end_ = Option.value end_ ~default:pos in
  let location = Locations.location locs ~start ~end_ in
  let message =
    match exn with
    | None -> message
    | Some exn ->
      assert (String.is_empty (Format.flush_str_formatter ()));
      Format.fprintf Format.str_formatter "%s\n" message;
      Location.report_exception Format.str_formatter exn;
      Format.flush_str_formatter ()
  in
  fail (Error.to_string { location = Some location; message })
;;

let string s =
  let%map (_ : string) = string s in
  ()
;;

let char c =
  let%map (_ : char) = char c in
  ()
;;

let option s = option None (s >>| Option.some)

let scan_through_ocaml_expression_until_unclosed_curly_brace ~locs =
  let%bind code =
    scan_string "" (fun curr_string c ->
      (* NOTE: This code is semi-complex/ugly. What it does is adding support for being
         able to handle tricky edge cases like:

         [%html {|<div>%{Vdom.Node.text "}"}</div>|}]

         Solely reading a single character at a time results in the curly brace inside of
         the interpolated ocaml closing the entire segment despite it being inside of an
         escaped string.

         The way this code works is that it solely calls the OCaml tokenizer until it
         seems a string with an unclosed curly brace, at which point it stops scanning the
         string. *)
      let curr_string = curr_string ^ Char.to_string c in
      let tokens_of_string = Ocaml_parsing.string_tokens curr_string in
      match tokens_of_string with
      | Error _ -> Some curr_string
      | Ok tokens ->
        let has_unclosed_curlys =
          List.fold tokens ~init:0 ~f:(fun parens -> function
            | LBRACE -> parens + 1
            | RBRACE -> parens - 1
            | _ -> parens)
          < 0
        in
        (match has_unclosed_curlys with
         | true -> None
         | false -> Some curr_string))
  in
  match%bind peek_char with
  | Some '}' -> return code
  | None | Some _ -> fail ~locs "Missing curly brace for interpolated OCaml"
;;

let identify_case_if_string_is_peeked ~string ~case =
  match%bind peek_string (String.length string) with
  | peeked when String.equal peeked string -> return case
  | _ -> Angstrom.fail "If you ever see this error message, this is a ppx_html bug"
;;

let identify_case_if_next_char_matches ~test ~case =
  match%bind peek_char with
  | Some peeked when test peeked -> return case
  | _ -> Angstrom.fail "If you ever see this error message, this is a ppx_html bug"
;;

let ( => ) string case = identify_case_if_string_is_peeked ~string ~case
let ( >=> ) test case = identify_case_if_next_char_matches ~test ~case

let parse_intepolation_kind : Model.Interpolation_kind.t t =
  choice
    [ string "%{" *> return Model.Interpolation_kind.Normal
    ; string "?{" *> return Model.Interpolation_kind.Option
    ; string "*{" *> return Model.Interpolation_kind.List
    ; string "#{" *> return Model.Interpolation_kind.String
    ]
;;

type interpolation =
  { expr : Model.Expr.t
  ; interpolation_kind : Model.Interpolation_kind.t
  }

let parse_expr_common ~locs : interpolation Angstrom.t =
  with_loc'
    ~locs
    (let%bind interpolation_kind = parse_intepolation_kind
     and start = pos
     and code = scan_through_ocaml_expression_until_unclosed_curly_brace ~locs
     and end_ = pos
     and () = string "}" in
     let interpolation_content_loc = Locations.location locs ~start ~end_ in
     let code, to_t =
       match Ocaml_parsing.rsplit_on_hash code with
       | None -> code, None
       | Some (code, to_t) ->
         let to_t =
           { txt = to_t
           ; loc = Locations.location locs ~start:(end_ - String.length to_t - 1) ~end_
           }
           |> ocaml_expression
           |> function
           | { pexp_desc = Pexp_construct (t, None); _ } ->
             { t with txt = Longident.name t.txt }
           | { pexp_loc; _ } ->
             Location.raise_errorf ~loc:pexp_loc "Expected a module identifier (e.g. Foo)"
         in
         code, Some to_t
     in
     let code =
       { txt = "(" ^ code ^ ")"; loc = Locations.location locs ~start:(start - 1) ~end_ }
     in
     let%map (expr : Ppxlib.expression) =
       (* borrowed from [ppx_string] *)
       match ocaml_expression code with
       | expr -> return expr
       | exception (Syntaxerr.Error error as exn) ->
         fail_but_we_know_location
           ~exn
           ~loc:(Syntaxerr.location_of_error error)
           "Failed to parse OCaml expression inside of HTML."
       | exception exn -> fail ~start ~end_ ~locs ~exn "Failed to Parse Expression"
     in
     let expr = { expr with pexp_loc = interpolation_content_loc } in
     let string_relative_location = { String_relative_location.start; end_ = end_ - 1 } in
     fun loc ->
       let expr =
         { Expr.expr; code; to_t; loc; string_relative_location; escape_kind = Escaped }
       in
       { expr; interpolation_kind })
;;

let skip_while1 f = skip f *> skip_while f
let take_span_while1 ~locs f = with_loc ~locs (take_while1 f)
let is_capitalized s = (not (String.is_empty s)) && Char.is_uppercase s.[0]

let parse_name ~allow_hyphens ~allow_dots ~expected ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' -> true
    | c ->
      Char.is_alphanum c
      || Char.equal c '\''
      || (allow_hyphens && Char.equal c '-')
      (* NOTE: We want to allow dots for the "component" syntax:

         e.g. <Foo.f></> *)
      || (allow_dots && Char.equal c '.'))
  <|> (peek_char
       >>= fun c ->
       let but =
         match c with
         | None -> ""
         | Some c ->
           let found =
             match Char.is_whitespace c with
             | true -> "whitespace. No whitespace is allowed here."
             | false -> [%string "'%{c#Char}'"]
           in
           [%string {|, but instead found %{found}|}]
       in
       fail ~locs [%string {|Expected a valid %{expected}%{but}.  |}])
;;

let parse_ocaml_name = parse_name ~allow_hyphens:false
let parse_attr_name = parse_name ~allow_hyphens:true ~allow_dots:false

let parse_html_token ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' | ':' | '-' | '.' -> true
    | c -> Char.is_alphanum c)
;;

let only_normal_interpolation_allowed ~locs ~interpolation_kind expr =
  match interpolation_kind with
  | Interpolation_kind.Normal -> return expr
  | (Option | List | String) as kind ->
    let normal_interpolation = "%{}" in
    fail
      ~locs
      [%string
        {|%{kind#Interpolation_kind} interpolation is not allowed here, only %{normal_interpolation} interpolation is allowed in this context.|}]
;;

let parse_quote_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Quote.Elt.Expr expr)
;;

let interpolation_case =
  choice
    [ "%{" => `Expression; "?{" => `Expression; "*{" => `Expression; "#{" => `Expression ]
;;

let argument_case = choice [ "~" => `Argument `Tilde; "?" => `Argument `Question_mark ]

let parse_quote ~locs : Model.Quote.t Angstrom.t =
  let quoted =
    match%bind
      choice
        [ interpolation_case
        ; Char.is_whitespace >=> `Whitespace_literal
        ; return `Literal
        ]
    with
    | `Expression -> parse_quote_expr ~locs
    | `Whitespace_literal ->
      let%map str = with_loc ~locs (take_while1 Char.is_whitespace) in
      Model.Quote.Elt.Literal str
    | `Literal ->
      let%map str =
        with_loc
          ~locs
          (consumed
             (skip_many1
                (match%bind any_char with
                 | '"' -> fail ~locs "End of string"
                 | '\\' ->
                   (* escape... *)
                   let%map _ = any_char in
                   ()
                 | '%' ->
                   let%bind c = peek_char_fail in
                   if Char.( = ) c '{'
                   then fail ~locs "Expected code block"
                   else return ()
                 | c ->
                   if Char.is_whitespace c then fail ~locs "Whitespace" else return ()))
           >>| Scanf.unescaped)
      in
      Model.Quote.Elt.Literal str
  in
  with_loc
    ~locs
    (match%bind choice [ "\"" => `Quoted; return `Html_token ] with
     | `Quoted ->
       let%map () = string "\""
       and t = many quoted
       and () = string "\"" in
       t
     | `Html_token ->
       let%map t = parse_html_token ~locs in
       [ Model.Quote.Elt.Literal t ])
;;

let parse_attr_value_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Attr.Value.Expr expr)
;;

let parse_value ~locs : Model.Attr.Value.t Angstrom.t =
  match%bind choice [ interpolation_case; return `Literal ] with
  | `Expression -> parse_attr_value_expr ~locs
  | `Literal ->
    let%map str = parse_quote ~locs in
    Model.Attr.Value.Literal str
;;

let parse_tag_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Tag.Expr expr)
;;

let parse_tag ~locs : Model.Tag.t Angstrom.t =
  match%bind choice [ interpolation_case; ">" => `Fragment; return `Literal ] with
  | `Expression -> parse_tag_expr ~locs
  | `Fragment ->
    let%map { txt = (); loc } = with_loc ~locs (return ()) in
    Model.Tag.Fragment loc
  | `Literal ->
    let%map start = pos
    and atom = parse_ocaml_name ~expected:"HTML tag" ~locs ~allow_dots:true
    and end_ = pos in
    (match String.mem atom.txt '.' || is_capitalized atom.txt with
     | false -> Model.Tag.Literal (Literal atom)
     | true ->
       let name = { atom with txt = Ppxlib.Longident.parse atom.txt }
       and string_relative_location = { String_relative_location.start; end_ } in
       Model.Tag.Literal (Component { name; string_relative_location; code = atom }))
;;

let skip_opt_ws = skip_while Char.is_whitespace
let skip_req_ws = skip_while1 Char.is_whitespace

let skip_comment =
  string "<!--"
  *> scan_string
       Reversed_list.[]
       (fun prev c ->
         let prev = Reversed_list.(c :: prev) in
         match prev with
         | '>' :: '-' :: '-' :: _ -> None
         | _ -> Some prev)
  *> string ">"
  *> return ()
;;

let many_nodes ~parse_node =
  fix (fun (many_nodes : Node.t list t) ->
    match%bind
      choice
        [ interpolation_case
        ; "<!--" => `Comment
        ; "</" => `Closing
        ; "<" => `Element
        ; (match%bind peek_char with
           | None -> return `End
           | _ -> Angstrom.fail "If you see this error, it is a ppx_html bug.")
        ; return `Text
        ]
    with
    | `Closing | `End -> return []
    | `Expression | `Element | `Text ->
      let%bind current_node = parse_node
      and remaining = many_nodes in
      return (current_node :: remaining)
    | `Comment ->
      let%bind () = skip_comment in
      many_nodes)
  >>| List.filter ~f:(function
    | Node.Text { txt = ""; loc = _ } -> false
    | _ -> true)
;;

let fail_with_closing_tag ~(tag : Tag.t) ~locs =
  let element =
    match tag with
    | Literal (Literal { txt; _ } | Component { name = _; code = { txt; _ }; _ }) ->
      [%string {|element "%{txt}"|}]
    | Expr _ -> "element"
    | Fragment _ -> "fragment"
  in
  peek_char
  >>= function
  | None ->
    fail
      ~locs
      [%string
        {|Expected closing '>' to terminate %{element}, but found end of ppx_html expression|}]
  | Some c ->
    fail
      ~locs
      [%string {|Expected closing '>' to terminate %{element}, but found '%{c#Char}'|}]
;;

let parse_attr_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Model.Attr.Expr { expr; interpolation_kind }
;;

let rec parse_attr ~parse_node ~locs : Model.Attr.t Angstrom.t =
  with_loc'
    ~locs
    (match%bind choice [ interpolation_case; argument_case; return `Attr_name ] with
     | `Attr_name ->
       let%bind name = parse_attr_name ~expected:"name of attribute" ~locs in
       let%bind value =
         match%bind choice [ "=" => `Equal; return `No_equal ] with
         | `Equal ->
           let%map () = char '='
           and value = parse_value ~locs in
           Some value
         | `No_equal -> return None
       in
       let ret loc = Model.Attr.Attr { name; value; loc } in
       return ret
     | `Argument sigil ->
       let error_message =
         "Error. Expected an OCaml interpolation (e.g. %{}) or HTML element here (e.g. \
          (<></>))"
       in
       let%map sigil =
         match sigil with
         | `Tilde -> string "~" *> return Model.Attr.Sigil.Tilde
         | `Question_mark -> string "?" *> return Model.Attr.Sigil.Question_mark
       and name = parse_attr_name ~expected:"name of argument" ~locs
       and argument =
         match%bind choice [ ":" => `With_payload; return `Punned ] with
         | `Punned -> return None
         | `With_payload ->
           let%bind () = string ":" *> return () in
           (match%bind
              choice
                [ "%{" => `Interpolation
                ; "(<" => `Element
                ; "<" => `Fail_with_hint
                ; return `Unknown
                ]
            with
            | `Unknown -> fail ~locs error_message
            | `Interpolation ->
              let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
              only_normal_interpolation_allowed
                ~locs
                ~interpolation_kind
                (Some (Attr.Argument.Expr expr))
            | `Element ->
              let%bind () = char '('
              and element = parse_element ~parse_node ~locs
              and () = char ')' in
              return (Some (Attr.Argument.Element element))
            | `Fail_with_hint ->
              fail
                ~locs
                [%string
                  "%{error_message} (HINT: Did you forget to wrap the element in \
                   parentheses?)"])
       in
       let ret loc = Model.Attr.Argument { name; argument; loc; sigil } in
       ret
     | `Expression ->
       let%map expr = parse_attr_expr ~locs in
       fun _ -> expr)

and parse_element ~(parse_node : Node.t t) ~locs : Element.t Angstrom.t =
  with_loc'
    ~locs
    (let%bind { txt = tag, attrs, closed, open_string_relative_location; loc = open_loc } =
       with_loc
         ~locs
         (let%bind start = pos in
          let%bind () = char '<'
          and tag = parse_tag ~locs in
          let%map attrs = many_attrs ~parse_node ~locs ~tag
          and () = skip_opt_ws
          and closed = option (char '/' *> skip_opt_ws) >>| Option.is_some
          and end_ = pos
          and () = char '>' in
          tag, attrs, closed, { String_relative_location.start; end_ })
     in
     let error_message =
       lazy
         (let tag =
            match tag with
            | Model.Tag.Literal
                ( Literal { txt; _ }
                | Component { name = _; code = { txt; _ }; string_relative_location = _ }
                  ) -> txt
            | Model.Tag.Expr { expr; _ } -> Pprintast.string_of_expression expr
            | Model.Tag.Fragment _ -> "fragment (<></>)"
          in
          [%string {|No closing tag, but expected one for '%{tag}'|}])
     in
     let%map inner, closing_tag =
       if closed
       then return (None, None)
       else (
         let%bind inner = many_nodes ~parse_node
         and close_start = pos in
         let%bind `Has_closing =
           match%bind peek_string 2 <|> fail ~locs (force error_message) with
           | "</" -> return `Has_closing
           | _ -> fail ~locs (force error_message)
         in
         let%bind () =
           let%map () = char '<'
           and () = char '/' in
           ()
         and () = skip_opt_ws
         and close_tag =
           option (parse_ocaml_name ~expected:"HTML tag" ~locs ~allow_dots:true)
         and () = skip_opt_ws
         and close_end = pos
         and () = char '>' <|> fail_with_closing_tag ~tag ~locs in
         let open struct
           (* NOTE: This is a bit complex. There are roughly three scenarios:

              - "Normal" tags, (e.g. <div></div>): The closing tag _must_ match the
                opening tag.

              - "Interpolated" tags/Fragments (e.g. <></> or <%[{F}]></>): The closing tag
                _must_ be empty.

              - "Component" tags (e.g. <Foo.f></> or <Foo.f></Foo.f>): The closing tag may
                be empty or must match the opening tag.
           *)
           type expected_closing_tag =
             | Empty
             | Empty_or_string of string
             | String of string
         end in
         let expected_closing_tag =
           match tag with
           | Tag.Literal (Literal tag) -> String tag.txt
           | Literal (Component { name; string_relative_location = _; code = _ }) ->
             Empty_or_string (Ppxlib.Longident.name name.txt)
           | Expr _ -> Empty
           | Fragment _ -> Empty
         in
         let ok =
           lazy
             (let loc =
                { String_relative_location.start = close_start; end_ = close_end }
              in
              let is_fragment_like =
                match close_tag with
                | Some { txt = ""; loc = _ } | None -> true
                | _ -> false
              in
              return (Some inner, Some { Closing_tag.loc; is_fragment_like }))
         in
         let fail message = fail ~start:close_start ~end_:close_end ~locs message in
         match expected_closing_tag with
         | Empty ->
           (match close_tag with
            | None -> force ok
            | Some wrong ->
              fail
                [%string "Expected an empty (</>) closing tag, but got </%{wrong.txt}>."])
         | String expected ->
           (match close_tag with
            | None ->
              fail
                [%string "Expected </%{expected}>, but got an empty closing tag (</>)."]
            | Some { txt = got; loc = _ } ->
              if String.equal got expected
              then force ok
              else
                fail [%string "Expected closing tag </%{expected}>, but got </%{got}>."])
         | Empty_or_string expected ->
           (match close_tag with
            | None -> force ok
            | Some { txt = got; loc = _ } ->
              if String.equal got expected
              then force ok
              else
                fail [%string "Expected closing tag </%{expected}>, but got </%{got}>."]))
     in
     fun loc ->
       { Element.tag
       ; attrs
       ; inner
       ; loc
       ; open_loc
       ; open_string_relative_location
       ; closing_tag
       })

and many_attrs ~parse_node ~tag ~locs =
  fix (fun many_attrs ->
    match%bind choice [ ">" => `End; "/>" => `End; return `Must_have_whitespace ] with
    | `End -> return []
    | `Must_have_whitespace ->
      let%bind () = skip_req_ws <|> fail_with_closing_tag ~tag ~locs in
      (match%bind choice [ ">" => `End; "/>" => `End; return `Maybe_attr ] with
       | `End -> return []
       | `Maybe_attr ->
         let%map curr = parse_attr ~parse_node ~locs
         and rem = many_attrs in
         curr :: rem))
;;

let collapse_prefix_and_trailing_ws s =
  (* NOTE: This "collapses" whitespace so that it remains in-sync with the spec defined
     in:

     https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace

     this does not "collapse" across adjacent whitespace between elements sadly.

     The "collapsing" whitespace part is optional. Another valid way of addressing this
     would be to solely leave the whitespace in, but this would have some runtime cost in
     addition to making the string harder to read during tests... This instead moves the
     cost to build time (at ppx expansion time).
  *)
  match String.for_all s ~f:Char.is_whitespace with
  | true -> " "
  | false ->
    let ws_prefix = String.take_while s ~f:Char.is_whitespace in
    let ws_suffix =
      String.take_while (String.rev s) ~f:Char.is_whitespace |> String.rev
    in
    let s =
      match ws_prefix with
      | "" -> s
      | _ -> " " ^ String.chop_prefix_exn ~prefix:ws_prefix s
    in
    let s =
      match ws_suffix with
      | "" -> s
      | _ -> String.chop_suffix_exn ~suffix:ws_suffix s ^ " "
    in
    s
;;

let string_until_interpolation_or_segment =
  let maybe_take s = s => `Take s in
  fix (fun string_until_interpolation_or_segment ->
    match%bind
      choice
        [ maybe_take "%%{"
        ; "%{" => `Finish
        ; "?{" => `Finish
        ; "*{" => `Finish
        ; "#{" => `Finish
        ; "<" => `Finish
        ; return `Consume
        ]
    with
    | `Finish -> return ()
    | `Take s -> string s *> string_until_interpolation_or_segment
    | `Consume ->
      (match%bind peek_char with
       | None -> return ()
       | Some c -> char c *> string_until_interpolation_or_segment))
;;

let parse_text ~locs : Node.t t =
  let%map str =
    with_loc
      ~locs
      (consumed string_until_interpolation_or_segment
       >>| (fun s ->
             if String.for_all s ~f:Char.is_whitespace && String.mem s '\n'
             then ""
             else collapse_prefix_and_trailing_ws s)
       >>| String.substr_replace_all ~pattern:"%%" ~with_:"%")
  in
  Node.Text str
;;

let parse_node_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Node.Expr { expr; interpolation_kind }
;;

let parse_node ~locs : Model.Node.t Angstrom.t =
  fix (fun parse_node ->
    match%bind choice [ interpolation_case; "<" => `Element; return `Text ] with
    | `Expression -> parse_node_expr ~locs
    | `Element ->
      let%bind.Angstrom element = parse_element ~parse_node ~locs in
      return (Node.Element element)
    | `Text -> parse_text ~locs)
;;

let parse ~locs =
  let%bind nodes = many_nodes ~parse_node:(parse_node ~locs) in
  match%bind peek_char with
  | None -> return nodes
  | Some _ ->
    (* NOTE: Here is unparsed input. Parsing will fail, but we still do things here to
       provide better error messages. I _think_ that this only happens with
       <div></div></div> though I am unsure it's __ONLY__ in that case hence the `Unknown
       case. *)
    (match%bind choice [ "</" => `Unopened_tag; return `Unknown ] with
     | `Unopened_tag -> fail ~locs "This closing tag was never opened."
     | `Unknown ->
       fail ~locs "Unparsed input. Please report this bug to ppx_html maintainers.")
;;

let of_string ~loc str =
  let locs = Locations.create loc str in
  let parse = parse ~locs in
  match
    Angstrom.parse_string
      ~consume:All
      (let%bind result = parse in
       let%bind () = end_of_input in
       return result)
      str
  with
  | Ok model -> model
  | Error message ->
    let error =
      let last =
        String.lsplit2 ~on:':' message |> Option.value_map ~f:snd ~default:message
      in
      try Sexp.of_string last |> Error.t_of_sexp with
      | _ -> Error.of_string last
    in
    Error.raise ~loc error
;;
