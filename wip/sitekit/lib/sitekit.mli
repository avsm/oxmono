(** Server-rendered web pages that are safe by construction.

    Sitekit renders HTML with {!Htmlit}, so text and attribute values are
    always escaped. Its components take no raw HTML and no attribute names, and
    every link takes a {!Url.t}, which admits only same-site paths and
    [http]/[https] addresses. Its stylesheet and script are served as files at
    paths named by their content's hash, so pages need no inline style or
    script, and {!Headers.html} gives a Content Security Policy without
    ['unsafe-inline'].

    A page is a {!site} shell around a list of components:

    {[
      let site =
        Sitekit.site ~name:"Example" ~brand:"ex"
          ~nav:[ Sitekit.nav "Jobs" (Sitekit.Url.path [ "jobs" ]) ]
          ()

      let jobs respond =
        Sitekit.respond_html respond
          (Sitekit.page site ~current:"/jobs" ~title:"Jobs"
             [ Sitekit.badge ~tone:`Good "ok" ])
    ]}

    and {!Asset.routes} serves the files its pages link to. *)

type html = Htmlit.El.html

(** {1 URLs} *)

module Url : sig
  type t : immutable_data
  (** A URL safe to use as a link or form target: a same-site path, or an
      [http] or [https] address. *)

  val path : ?query:(string * string) list -> string list -> t
  (** [path ~query segments] is [/segment/segment?key=value], with every
      segment, key and value percent-encoded. [path []] is [/]. *)

  val of_path : string -> t option
  (** [of_path s] is [s] if it is a same-site path: it starts with [/] but not
      [//] or [/\\], and holds no backslash, space, control character or
      character outside ASCII. Use it for paths already encoded, such as one a
      request named. *)

  val external_ : string -> t option
  (** [external_ s] is [s] if it is an [http:] or [https:] address with a host,
      holding no space, control character, backslash, quote or angle bracket.
      Every other scheme, such as [javascript:] or [data:], is refused. *)

  val to_string : t -> string @@ portable

  val path_part : t -> string @@ portable
  (** [path_part t] is [t] without its query or fragment. *)
end

(** {1 Assets} *)

module Asset : sig
  type t : immutable_data
  (** A file pages link to, served at a path named by its content's hash, so
      that it can be cached for good. *)

  val css : name:string -> string -> t
  (** [css ~name source] is a stylesheet. [name], such as [site], must be
      lowercase letters, digits and dashes. *)

  val js : name:string -> string -> t
  (** [js ~name source] is a script. [name] is as for {!css}. *)

  val url : t -> Url.t @@ portable
  (** [url t] is [/_sitekit/NAME.HASH.EXT], where [HASH] is the first sixteen
      hexadecimal digits of the SHA-256 of the content. *)

  val media_type : t -> string @@ portable
  val content : t -> string @@ portable

  val stylesheet : t
  (** [stylesheet] is Sitekit's own stylesheet, which every {!page} links. *)

  val script : t
  (** [script] is Sitekit's own script, which sorts {!table}s. Pages link it
      deferred. It listens for a [sitekit:content] event on the document to
      enhance content added later. *)

  val routes : t list -> 'env Proffer.Route.t list
  (** [routes assets] serves {!stylesheet}, {!script} and [assets] at their
      {!url}s, with {!Headers.asset}. *)
end

(** {1 Security headers} *)

module Headers : sig
  type cache =
    [ `No_store  (** Never kept, for pages showing private data. *)
    | `Revalidate  (** Kept, but checked with the server each time. *) ]

  val csp :
    ?img_src:string list ->
    ?connect_src:string list ->
    ?frame_src:string list ->
    ?frame_ancestors:string list ->
    unit ->
    string
    @@ portable
  (** [csp ()] is a Content Security Policy allowing scripts, styles and
      images only from the site itself, forms posting only to it, and no
      framing:
      [default-src 'none'; script-src 'self'; style-src 'self'; img-src 'self';
       connect-src 'self'; form-action 'self'; base-uri 'none';
       frame-ancestors 'none'].
      Each optional list adds sources to its directive. A [frame_src] list
      adds a [frame-src] directive, and [frame_ancestors] replaces ['none'].
      No source may hold [;], [,], a quote or whitespace, and
      ['unsafe-inline'] and ['unsafe-eval'] are refused, raising
      [Invalid_argument]. *)

  val html :
    ?cache:cache ->
    ?img_src:string list ->
    ?connect_src:string list ->
    ?frame_src:string list ->
    ?frame_ancestors:string list ->
    unit ->
    (string * string) list
    @@ portable
  (** [html ()] is the fields for an HTML response: the {!csp},
      [X-Content-Type-Options: nosniff], [Referrer-Policy: same-origin],
      [Cross-Origin-Opener-Policy: same-origin] and a [Cache-Control] from
      [cache], [`No_store] by default. *)

  val asset : (string * string) list
  (** [asset] is the fields for a content-addressed {!Asset.t}:
      [nosniff] and [Cache-Control: public, max-age=31536000, immutable]. *)

  val fields : (string * string) list -> Proffer.Headers.t @@ portable
  (** [fields l] is [l] as a Proffer field block. *)
end

val respond_html :
  Proffer.Resp.respond @ local ->
  ?status:Proffer.Status.t ->
  ?headers:(string * string) list ->
  string ->
  unit
  @@ portable
(** [respond_html respond s] responds with the page [s] and [headers],
    {!Headers.html} by default. *)

(** {1 Pages} *)

type nav

val nav : string -> Url.t -> nav
(** [nav label url] is a navigation link. *)

type theme =
  [ `Auto  (** Light or dark, as the reader's system prefers. *)
  | `Light ]

type site

val site :
  name:string ->
  brand:string ->
  ?home:Url.t ->
  ?nav:nav list ->
  ?stylesheets:Asset.t list ->
  ?scripts:Asset.t list ->
  ?footer:html list ->
  ?theme:theme ->
  unit ->
  site
(** [site ~name ~brand ()] is a site called [name], whose header shows the
    short [brand] mark and links to [home], [/] by default. [stylesheets] and
    [scripts] are linked after Sitekit's own, scripts deferred. [theme] is
    [`Auto] by default. *)

val page :
  site ->
  current:string ->
  ?account:html list ->
  ?crumbs:(string * Url.t) list ->
  ?kind:string ->
  title:string ->
  html list ->
  string
(** [page site ~current ~title body] is a whole document titled
    ["TITLE · NAME"], with a skip link, the site's header and navigation, an
    [account] area at the end of the header, and [<main id="main">] holding an
    [<h1>] of [title] and [body]. The navigation link whose path is [current]
    is marked [aria-current="page"]. A non-empty [crumbs] trail is shown
    above the [<h1>] by {!breadcrumbs}, ending at [title]. [kind], a class
    name of letters, digits and dashes, is set on [<main>]. *)

val breadcrumbs : (string * Url.t) list -> current:string -> html
(** [breadcrumbs trail ~current] is a breadcrumb trail of the [(label, url)]
    links of [trail], from the top of the site down, ending at the page
    [current], which is marked [aria-current="page"] and is not a link. The
    first link shows a house. Separators are drawn by the stylesheet. *)

val avatar : ?size:int -> Url.t -> alt:string -> html
(** [avatar ~size url ~alt] is a round picture of an account from the site's
    own [url], [size] pixels square, 20 by default, loaded lazily. *)

val person : ?avatar:Url.t -> string -> html
(** [person ~avatar name] is [name] after its {!avatar}, if it has one. *)

val account :
  ?avatar:Url.t ->
  ?links:(string * Url.t) list ->
  name:string ->
  logout:Url.t ->
  csrf:string * string ->
  unit ->
  html list
(** [account ~name ~logout ~csrf ()] is an account menu for the end of the
    header. Its trigger is the account's [avatar], 28 pixels square, or the
    first letter of [name] without it. It opens a panel saying ["Signed in as
    NAME"], the [links], as [(label, url)] pairs, none by default, and a
    button that posts a form to [logout] holding the hidden field [csrf] as
    [(name, value)]. The menu opens without the script, which closes it on a
    click elsewhere or on Escape. *)

(** {1 Components} *)

val text : string -> html
val link : Url.t -> string -> html
val link_to : Url.t -> html list -> html

val follow : Url.t -> string -> html
(** [follow url label] is a link that Sitekit's script follows as soon as the
    page loads, and that the reader can follow without it. A site answers a
    form whose result is another site, such as a login sent to an OAuth
    authorization server, with a page holding it, since the form-action
    policy forbids redirecting a form elsewhere. *)

val badge : ?tone:[ `Neutral | `Good | `Warning | `Danger ] -> string -> html
(** [badge ~tone label] is a small label, [`Neutral] by default. *)

val metric : label:string -> html -> html
(** [metric ~label value] is a large [value] above a small [label]. *)

val metrics : html list -> html
(** [metrics l] lays out {!metric}s in a row. *)

val cell : ?sort:string -> ?ratio:float -> html list -> html
(** [cell ~sort ~ratio content] is a table cell that sorts by [sort], or by
    its text without it. [ratio] is an alternative numeric sort value. *)

val table :
  ?caption:string ->
  (string * [ `Text | `Number ]) list ->
  html list list ->
  html
(** [table columns rows] is a table whose header buttons sort its rows in the
    browser, by text or numerically, announcing each sort to screen readers.
    Each row is a list of {!cell}s. Without the script it is a plain table. *)

val coverage : present:int -> total:int -> html
(** [coverage ~present ~total] is a bar showing what part of [total] is
    [present], drawn as an image with a text alternative. *)

val state_sprite : html
(** [state_sprite] holds the symbols {!state_icon} uses, once per page. *)

val state_icon : ?label:string -> string -> html
(** [state_icon ~label state] is the icon for [state]: [queued], [running],
    [done], [failed], [cancelled], [blocked], [notice], or else [unknown].
    [label] defaults to [state]. *)

val state : ?label:string -> string -> html
(** [state s] is {!state_icon} followed by the label. *)

val facts : (string * html option) list -> html
(** [facts l] is a description list of the entries of [l] with a value. *)

val next_page : Url.t -> string -> html
(** [next_page url label] is a link to the next page of a listing, such as
    ["Later jobs →"]. *)

val input :
  name:string -> label:string -> ?kind:string -> value:string -> unit -> html
(** [input ~name ~label ~value ()] is a labelled input. [kind] is its [type],
    [text] by default, one of [text], [search], [number], [date] or [email];
    others raise [Invalid_argument]. *)

val select :
  name:string -> label:string -> value:string -> (string * string) list -> html
(** [select ~name ~label ~value options] is a labelled choice of
    [(value, label)] [options], with [value] selected. *)

val checkbox : id:string -> label:string -> checked:bool -> html

val get_form : action:Url.t -> ?submit:string -> html list -> html
(** [get_form ~action fields] is a form of [fields] that requests [action]
    with them as its query, with a [submit] button, ["Show"] by default. *)

val post_form :
  action:Url.t ->
  hidden:(string * string) list ->
  ?submit:string ->
  html list ->
  html
(** [post_form ~action ~hidden fields] is a form posting [fields] and the
    [hidden] fields, such as a CSRF token, to [action], with a [submit]
    button, ["Submit"] by default. *)

val post_button :
  action:Url.t -> hidden:(string * string) list -> label:string -> html
(** [post_button ~action ~hidden ~label] is a form posting the [hidden] fields
    to [action] with one button labelled [label]. *)

val log : string -> html

val terminal : string -> html
(** [terminal s] is the output [s] of a program that wrote to a terminal, as
    preformatted text with its colours and text attributes: bold, dim,
    italic, underline, strike-through and inverse, the sixteen colours, which
    follow the site's theme, the 256-colour palette, and truecolour drawn as
    the nearest of those. Every other escape code, every control character
    but tabs and line ends, and every character a terminal draws as nothing
    is removed first, by [Console_text.strip]. Styles are classes, never
    inline styles, so pages keep a policy without ['unsafe-inline']. *)
(** [log s] is [s] as preformatted text. *)

val sr_only : string -> html
(** [sr_only s] is text for screen readers only. *)

(** {1 Formatting} *)

val count : int -> string
(** [count n] is [n] with thousands separated by commas. *)

val size : int64 -> string
(** [size n] is [n] bytes as [B], [KiB] or [MiB]. *)

val time : Ptime.t -> string
(** [time t] is [t] as [YYYY-MM-DD HH:MM UTC]. *)

val duration : float -> string
(** [duration s] is [s] seconds as [42s], [3m05s] or [2h07m]. *)
