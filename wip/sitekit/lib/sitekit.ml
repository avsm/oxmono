(* SPDX-License-Identifier: ISC *)
open Htmlit

type html = El.html

(* Attribute names below are all literals, so no caller can add an event
   handler or a style. *)
let el ?(at = []) name children =
  El.v name ~at:(List.map (fun (k, v) -> At.v k v) at) children

let text = El.txt

let word s =
  s <> ""
  && String.for_all
       (function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' -> true | _ -> false)
       s

module Url = struct
  type t = { url : string }

  let encode = Uriz.pct_encode ~component:`Unreserved

  let path ?(query = []) segments =
    let p = "/" ^ String.concat "/" (List.map encode segments) in
    let q =
      match query with
      | [] -> ""
      | q ->
          "?"
          ^ String.concat "&"
              (List.map (fun (k, v) -> encode k ^ "=" ^ encode v) q)
    in
    { url = p ^ q }

  (* Printable ASCII only. A space, a control character or a backslash is
     read differently by different parsers, which is how an open redirect or
     a scheme slips past a check. *)
  let printable s =
    String.for_all (fun c -> c > ' ' && c < '\127' && c <> '\\') s

  let of_path s =
    if
      String.length s >= 1
      && s.[0] = '/'
      && (String.length s < 2 || (s.[1] <> '/' && s.[1] <> '\\'))
      && printable s
    then Some { url = s }
    else None

  let external_ s =
    let lower = String.lowercase_ascii s in
    let after prefix =
      if String.starts_with ~prefix lower then
        Some
          (String.sub s (String.length prefix)
             (String.length s - String.length prefix))
      else None
    in
    match
      match after "https://" with Some r -> Some r | None -> after "http://"
    with
    | Some rest
      when rest <> ""
           && rest.[0] <> '/'
           && printable s
           && String.for_all
                (fun c -> c <> '"' && c <> '\'' && c <> '<' && c <> '>')
                s ->
        Some { url = s }
    | _ -> None

  let to_string t = t.url

  let path_part t =
    let s = t.url in
    let stop =
      match (String.index_opt s '?', String.index_opt s '#') with
      | Some a, Some b -> min a b
      | Some a, None | None, Some a -> a
      | None, None -> String.length s
    in
    String.sub s 0 stop
end

module Asset = struct
  type t = { name : string; media_type : string; content : string; url : Url.t }

  let make ~ext ~media_type ~name content =
    if
      not
        (word name
        && String.lowercase_ascii name = name
        && not (String.contains name '_'))
    then invalid_arg ("Sitekit.Asset: bad name " ^ name);
    let hash =
      String.sub Digestif.SHA256.(to_hex (digest_string content)) 0 16
    in
    {
      name;
      media_type;
      content;
      url = { Url.url = Printf.sprintf "/_sitekit/%s.%s.%s" name hash ext };
    }

  let css ~name s =
    make ~ext:"css" ~media_type:"text/css; charset=utf-8" ~name s

  let js ~name s =
    make ~ext:"js" ~media_type:"text/javascript; charset=utf-8" ~name s

  let url t = t.url
  let media_type t = t.media_type
  let content t = t.content
  let stylesheet = css ~name:"sitekit" Stylesheet.source
  let script = js ~name:"sitekit" Script.source

  let file t =
    let u = Url.to_string t.url in
    String.sub u 10 (String.length u - 10)

  let routes assets =
    List.map
      (fun t ->
        let open Proffer.Route in
        get
          (s "_sitekit" / s (file t))
          (fun _ _ respond ->
            Proffer.Resp.media respond
              ~headers:
                [
                  Proffer.Resp.other "X-Content-Type-Options" "nosniff";
                  Proffer.Resp.other "Cache-Control"
                    "public, max-age=31536000, immutable";
                ]
              t.media_type t.content))
      (stylesheet :: script :: assets)
end

module Headers = struct
  type cache = [ `No_store | `Revalidate ]

  let source s =
    if
      s = ""
      || String.exists
           (fun c ->
             c = ';' || c = ',' || c = '"' || c = ' ' || c < ' ' || c = '\127')
           s
      || List.mem (String.lowercase_ascii s)
           [ "'unsafe-inline'"; "'unsafe-eval'"; "'unsafe-hashes'" ]
    then invalid_arg ("Sitekit.Headers.csp: bad source " ^ s);
    s

  let csp ?(img_src = []) ?(connect_src = []) ?frame_src ?frame_ancestors () =
    let d name base extra =
      String.concat " " ((name :: base) @ List.map source extra)
    in
    String.concat "; "
      ([
         "default-src 'none'";
         "script-src 'self'";
         "style-src 'self'";
         d "img-src" [ "'self'" ] img_src;
         d "connect-src" [ "'self'" ] connect_src;
       ]
      @ (match frame_src with Some l -> [ d "frame-src" [] l ] | None -> [])
      @ [
          "form-action 'self'";
          "base-uri 'none'";
          (match frame_ancestors with
          | Some (_ :: _ as l) -> d "frame-ancestors" [] l
          | _ -> "frame-ancestors 'none'");
        ])

  let html ?(cache = `No_store) ?img_src ?connect_src ?frame_src
      ?frame_ancestors () =
    [
      ( "Content-Security-Policy",
        csp ?img_src ?connect_src ?frame_src ?frame_ancestors () );
      ("X-Content-Type-Options", "nosniff");
      ("Referrer-Policy", "same-origin");
      ("Cross-Origin-Opener-Policy", "same-origin");
      ( "Cache-Control",
        match cache with `No_store -> "no-store" | `Revalidate -> "no-cache" );
    ]

  let asset =
    [
      ("X-Content-Type-Options", "nosniff");
      ("Cache-Control", "public, max-age=31536000, immutable");
    ]

  let fields l = List.map (fun (k, v) -> Proffer.Resp.other k v) l
end

let respond_html respond ?status ?(headers = Headers.html ()) body =
  Proffer.Resp.html respond ?status ~headers:(Headers.fields headers) body

type nav = { label : string; target : Url.t }

let nav label target = { label; target }

type theme = [ `Auto | `Light ]

type site = {
  name : string;
  brand : string;
  home : Url.t;
  nav : nav list;
  stylesheets : Asset.t list;
  scripts : Asset.t list;
  footer : html list;
  theme : theme;
}

let site ~name ~brand ?(home = Url.path []) ?(nav = []) ?(stylesheets = [])
    ?(scripts = []) ?(footer = []) ?(theme = `Auto) () =
  { name; brand; home; nav; stylesheets; scripts; footer; theme }

let href u = ("href", Url.to_string u)
let link u label = el "a" ~at:[ href u ] [ text label ]
let link_to u children = el "a" ~at:[ href u ] children

let follow u label =
  el "a" ~at:[ href u; ("class", "sk-button"); ("data-sk-follow", "") ] [ text label ]

let page site ~current ?(account = []) ?kind ~title body =
  let kind =
    match kind with
    | Some k when word k -> [ ("class", "sk-main " ^ k) ]
    | Some k -> invalid_arg ("Sitekit.page: bad kind " ^ k)
    | None -> [ ("class", "sk-main") ]
  in
  let stylesheet a =
    el "link" ~at:[ ("rel", "stylesheet"); href (Asset.url a) ] []
  in
  let script a =
    el "script" ~at:[ ("src", Url.to_string (Asset.url a)); ("defer", "") ] []
  in
  let nav_link n =
    el "a"
      ~at:
        (href n.target
        ::
        (if Url.path_part n.target = current then [ ("aria-current", "page") ]
         else []))
      [ text n.label ]
  in
  El.to_string ~doctype:true
    (el "html"
       ~at:
         (("lang", "en")
         ::
         (match site.theme with
         | `Light -> [ ("data-theme", "light") ]
         | `Auto -> []))
       [
         el "head"
           ([
              el "meta" ~at:[ ("charset", "utf-8") ] [];
              el "meta"
                ~at:
                  [
                    ("name", "viewport");
                    ("content", "width=device-width, initial-scale=1");
                  ]
                [];
              el "title" [ text (title ^ " · " ^ site.name) ];
            ]
           @ List.map stylesheet (Asset.stylesheet :: site.stylesheets)
           @ List.map script (Asset.script :: site.scripts));
         el "body"
           [
             el "a"
               ~at:[ ("class", "sk-skip"); ("href", "#main") ]
               [ text "Skip to content" ];
             el "header"
               ~at:[ ("class", "sk-header") ]
               [
                 el "a"
                   ~at:[ ("class", "sk-brand"); href site.home ]
                   [
                     el "span"
                       ~at:[ ("class", "sk-mark"); ("aria-hidden", "true") ]
                       [ text site.brand ];
                     text site.name;
                   ];
                 (if site.nav = [] then El.void
                  else
                    el "nav"
                      ~at:
                        [
                          ("class", "sk-nav"); ("aria-label", "Main navigation");
                        ]
                      (List.map nav_link site.nav));
                 (if account = [] then El.void
                  else el "div" ~at:[ ("class", "sk-account") ] account);
               ];
             el "main" ~at:(("id", "main") :: kind)
               (el "h1" [ text title ] :: body);
             (if site.footer = [] then El.void
              else el "footer" ~at:[ ("class", "sk-footer") ] site.footer);
           ];
       ])

let post_button ~action ~hidden ~label =
  el "form"
    ~at:
      [
        ("class", "sk-inline");
        ("method", "post");
        ("action", Url.to_string action);
      ]
    (List.map
       (fun (n, v) ->
         el "input" ~at:[ ("type", "hidden"); ("name", n); ("value", v) ] [])
       hidden
    @ [ el "button" ~at:[ ("type", "submit") ] [ text label ] ])

let account ~name ~logout ~csrf =
  [
    el "span"
      ~at:[ ("class", "sk-who") ]
      [ text "Signed in as "; el "strong" [ text name ] ];
    post_button ~action:logout ~hidden:[ csrf ] ~label:"Log out";
  ]

let badge ?(tone = `Neutral) label =
  let tone =
    match tone with
    | `Neutral -> "neutral"
    | `Good -> "good"
    | `Warning -> "warning"
    | `Danger -> "danger"
  in
  el "span" ~at:[ ("class", "sk-badge " ^ tone) ] [ text label ]

let metric ~label value =
  el "div"
    ~at:[ ("class", "sk-metric") ]
    [ el "strong" [ value ]; el "span" [ text label ] ]

let metrics l = el "div" ~at:[ ("class", "sk-metrics") ] l

let cell ?sort ?ratio children =
  el "td"
    ~at:
      ((match sort with Some v -> [ ("data-sort-value", v) ] | None -> [])
      @
      match ratio with
      | Some r -> [ ("data-sort-ratio", Printf.sprintf "%.12g" r) ]
      | None -> [])
    children

let table ?caption columns rows =
  el "div"
    ~at:[ ("class", "sk-table") ]
    [
      el "div"
        ~at:[ ("class", "sk-scroll") ]
        [
          el "table"
            ~at:[ ("data-sortable", "") ]
            ((match caption with
               | Some c -> [ el "caption" [ text c ] ]
               | None -> [])
            @ [
                el "thead"
                  [
                    el "tr"
                      (List.mapi
                         (fun i (label, kind) ->
                           el "th"
                             ~at:[ ("scope", "col"); ("aria-sort", "none") ]
                             [
                               el "button"
                                 ~at:
                                   [
                                     ("type", "button");
                                     ("data-sort-column", string_of_int i);
                                     ( "data-sort-type",
                                       match kind with
                                       | `Number -> "number"
                                       | `Text -> "text" );
                                   ]
                                 [
                                   text label;
                                   el "span"
                                     ~at:
                                       [
                                         ("class", "sk-arrow");
                                         ("aria-hidden", "true");
                                       ]
                                     [ text "↕" ];
                                 ];
                             ])
                         columns);
                  ];
                el "tbody" (List.map (fun cells -> el "tr" cells) rows);
              ]);
        ];
      el "p"
        ~at:
          [
            ("class", "sk-sort-status sk-sr");
            ("role", "status");
            ("aria-live", "polite");
          ]
        [];
    ]

(* An image of two rectangles, sized by attributes rather than an inline
   style, which the policy would refuse. *)
let coverage ~present ~total =
  let pct =
    if total <= 0 then 0.
    else 100. *. float (max 0 (min present total)) /. float total
  in
  let label =
    if total <= 0 then "None"
    else
      Printf.sprintf "%d present (%.1f%%), %d missing (%.1f%%)" present pct
        (total - present) (100. -. pct)
  in
  el "svg"
    ~at:
      [
        ("class", "sk-coverage" ^ if total <= 0 then " empty" else "");
        ("viewBox", "0 0 100 6");
        ("preserveAspectRatio", "none");
        ("role", "img");
        ("aria-label", label);
      ]
    [
      el "title" [ text label ];
      el "rect"
        ~at:
          [
            ("class", "sk-missing");
            ("width", "100");
            ("height", "6");
            ("rx", "3");
          ]
        [];
      el "rect"
        ~at:
          [
            ("class", "sk-present");
            ("width", Printf.sprintf "%.3f" pct);
            ("height", "6");
            ("rx", "3");
          ]
        [];
    ]

let symbol id children =
  el "symbol"
    ~at:[ ("id", "sk-state-" ^ id); ("viewBox", "0 0 24 24") ]
    children

let circle = el "circle" ~at:[ ("cx", "12"); ("cy", "12"); ("r", "9") ] []
let path d = el "path" ~at:[ ("d", d) ] []

let state_sprite =
  el "svg"
    ~at:
      [
        ("class", "sk-sprite"); ("aria-hidden", "true"); ("focusable", "false");
      ]
    [
      symbol "queued" [ circle; path "M12 7.5v4.5l3 2" ];
      symbol "running"
        [
          el "path"
            ~at:[ ("d", "M12 3a9 9 0 1 1-6.4 2.6"); ("class", "sk-arc") ]
            [];
          path "M5.6 2.5v3.1h3.1";
        ];
      symbol "done" [ circle; path "M8 12.5l2.6 2.6L16 9.5" ];
      symbol "failed" [ circle; path "M9 9l6 6M15 9l-6 6" ];
      symbol "cancelled" [ circle; path "M7.5 16.5l9-9" ];
      symbol "blocked" [ circle; path "M9.5 8.5v7M14.5 8.5v7" ];
      symbol "notice" [ circle; path "M12 11v5.5M12 7.8v.1" ];
      symbol "unknown"
        [
          el "circle"
            ~at:
              [
                ("cx", "12");
                ("cy", "12");
                ("r", "9");
                ("stroke-dasharray", "3 3");
              ]
            [];
          path "M12 16.5v.1M10 9.8a2 2 0 1 1 3 1.8c-.7.4-1 .9-1 1.7";
        ];
    ]

let known =
  [ "queued"; "running"; "done"; "failed"; "cancelled"; "blocked"; "notice" ]

let state_icon ?label state =
  let id = if List.mem state known then state else "unknown" in
  let label = Option.value label ~default:state in
  el "svg"
    ~at:[ ("class", "sk-state " ^ id); ("role", "img"); ("aria-label", label) ]
    [
      el "title" [ text label ]; el "use" ~at:[ ("href", "#sk-state-" ^ id) ] [];
    ]

let state ?label s =
  let label = Option.value label ~default:s in
  el "span"
    ~at:[ ("class", "sk-state-label") ]
    [ state_icon ~label s; text label ]

let facts l =
  el "dl"
    ~at:[ ("class", "sk-facts") ]
    (List.concat_map
       (fun (k, v) ->
         match v with
         | Some v -> [ el "dt" [ text k ]; el "dd" [ v ] ]
         | None -> [])
       l)

let next_page u label = el "p" ~at:[ ("class", "sk-next") ] [ link u label ]

let input ~name ~label ?(kind = "text") ~value () =
  if not (List.mem kind [ "text"; "search"; "number"; "date"; "email" ]) then
    invalid_arg ("Sitekit.input: bad kind " ^ kind);
  el "label"
    [
      text label;
      el "input" ~at:[ ("name", name); ("type", kind); ("value", value) ] [];
    ]

let select ~name ~label ~value options =
  el "label"
    [
      text label;
      el "select"
        ~at:[ ("name", name) ]
        (List.map
           (fun (v, l) ->
             el "option"
               ~at:
                 (("value", v)
                 :: (if v = value then [ ("selected", "") ] else []))
               [ text l ])
           options);
    ]

let checkbox ~id ~label ~checked =
  el "label"
    ~at:[ ("class", "sk-check"); ("for", id) ]
    [
      el "input"
        ~at:
          (("type", "checkbox") :: ("id", id)
          :: (if checked then [ ("checked", "") ] else []))
        [];
      text label;
    ]

let get_form ~action ?(submit = "Show") fields =
  el "form"
    ~at:
      [
        ("class", "sk-form"); ("method", "get"); ("action", Url.to_string action);
      ]
    (fields @ [ el "button" ~at:[ ("type", "submit") ] [ text submit ] ])

let post_form ~action ~hidden ?(submit = "Submit") fields =
  el "form"
    ~at:
      [
        ("class", "sk-form"); ("method", "post"); ("action", Url.to_string action);
      ]
    (List.map
       (fun (n, v) ->
         el "input" ~at:[ ("type", "hidden"); ("name", n); ("value", v) ] [])
       hidden
    @ fields
    @ [ el "button" ~at:[ ("type", "submit") ] [ text submit ] ])

let log s = el "pre" ~at:[ ("class", "sk-log") ] [ text s ]
let sr_only s = el "span" ~at:[ ("class", "sk-sr") ] [ text s ]

let count n =
  let s = string_of_int (abs n) in
  let len = String.length s in
  let b = Buffer.create (len + (len / 3) + 1) in
  if n < 0 then Buffer.add_char b '-';
  String.iteri
    (fun i c ->
      if i > 0 && (len - i) mod 3 = 0 then Buffer.add_char b ',';
      Buffer.add_char b c)
    s;
  Buffer.contents b

let size n =
  let f = Int64.to_float n in
  if f >= 1048576. then Printf.sprintf "%.1f MiB" (f /. 1048576.)
  else if f >= 1024. then Printf.sprintf "%.1f KiB" (f /. 1024.)
  else Int64.to_string n ^ " B"

let time t =
  let (y, m, d), ((hh, mm, _), _) = Ptime.to_date_time t in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d UTC" y m d hh mm

let duration s =
  let s = int_of_float (Float.max 0. s) in
  if s < 60 then Printf.sprintf "%ds" s
  else if s < 3600 then Printf.sprintf "%dm%02ds" (s / 60) (s mod 60)
  else Printf.sprintf "%dh%02dm" (s / 3600) (s / 60 mod 60)
