(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Each atom's text is non-empty, valid UTF-8 and holds no control character
   but the line feed: the constructors make it so, and every operation below
   keeps it. An atom with [draw] is drawn when printed, in [style] and the
   formatter's look; its [text] is its drawing in {!Look.default}, which has
   the same cells in every look, and measures it. A line feed alone in an
   atom with [block] joins two lines of one widget, which a region too short
   for it drops together. *)
type align = [ `Left | `Center | `Right ]

type atom = {
  style : Style.t;
  link : string option;
  text : string;
  draw : (Look.t -> atom list) option;
  block : bool;
}

module F = struct
  type t = atom list

  let clean = Console_text.sanitize ~keep_newlines:true

  (* The characters of a URI (RFC 3986, sections 2.2 and 2.3) and the percent
     sign of an escape: none of them can end the OSC 8 sequence a target is
     written in. *)
  let uri_char = function
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!'
    | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '%' ->
        true
    | _ -> false

  let hex = "0123456789ABCDEF"

  let target uri =
    if String.for_all uri_char uri then uri
    else begin
      let buf = Buffer.create (3 * String.length uri) in
      String.iter
        (fun c ->
          if uri_char c then Buffer.add_char buf c
          else begin
            Buffer.add_char buf '%';
            Buffer.add_char buf hex.[Char.code c lsr 4];
            Buffer.add_char buf hex.[Char.code c land 15]
          end)
        uri;
      Buffer.contents buf
    end

  let atom ?link style text =
    match clean text with
    | "" -> []
    | text -> [ { style; link; text; draw = None; block = false } ]

  let empty = []
  let is_empty = function [] -> true | _ :: _ -> false
  let text s = atom Style.none s
  let ink style s = atom style s

  let link ~uri t =
    if uri = "" then t
    else
      let link = Some (target uri) in
      List.map (fun a -> { a with link }) t

  let ( ++ ) = ( @ )

  let concat ?(sep = empty) = function
    | [] -> empty
    | x :: xs -> List.concat (x :: List.concat_map (fun y -> [ sep; y ]) xs)

  let styled base t =
    List.map (fun a -> { a with style = Style.(base + a.style) }) t

  let is_drawn a = Option.is_some a.draw

  let rec resolve look t =
    if not (List.exists is_drawn t) then t
    else
      List.concat_map
        (fun a ->
          match a.draw with
          | None -> [ a ]
          | Some draw -> styled a.style (resolve look (draw look)))
        t

  let drawn draw =
    match String.concat "" (List.map (fun a -> a.text) (draw Look.default)) with
    | "" -> empty
    | text ->
        [
          {
            style = Style.none;
            link = None;
            text;
            draw = Some draw;
            block = false;
          };
        ]

  (* A stop itself where it stands, so that a palette colour stays one. *)
  let at stops p =
    let n = Array.length stops in
    let x = Float.max 0. (Float.min 1. p) *. float_of_int (n - 1) in
    let i = int_of_float x in
    if i >= n - 1 then stops.(n - 1)
    else if Float.of_int i = x then stops.(i)
    else Color.blend stops.(i) stops.(i + 1) (x -. float_of_int i)

  let color_at stops p = at (Array.of_list stops) p

  let seamed t =
    let rec go acc joined line = function
      | [] -> List.rev ((joined, List.rev line) :: acc)
      | a :: rest -> (
          match String.index_opt a.text '\n' with
          | None -> go acc joined (a :: line) rest
          | Some i ->
              let before = String.sub a.text 0 i in
              let after =
                String.sub a.text (i + 1) (String.length a.text - i - 1)
              in
              let line =
                if before = "" then line else { a with text = before } :: line
              in
              let rest =
                if after = "" then rest else { a with text = after } :: rest
              in
              let next = a.block && before = "" && after = "" in
              go ((joined, List.rev line) :: acc) next [] rest)
    in
    go [] false [] t

  let lines t =
    if not (List.exists (fun a -> String.contains a.text '\n') t) then [ t ]
    else List.map snd (seamed t)

  let unlines = function
    | [] -> empty
    | l :: ls -> List.concat (l :: List.map (fun l -> text "\n" @ l) ls)

  let joint =
    [
      {
        style = Style.none;
        link = None;
        text = "\n";
        draw = None;
        block = true;
      };
    ]

  let block lines = concat ~sep:joint lines

  let unseam = function
    | [] -> empty
    | (_, l) :: ls ->
        List.concat
          (l :: List.map (fun (j, l) -> (if j then joint else text "\n") @ l) ls)

  let role_style theme = function
    | `Accent -> Style.fg (Theme.accent theme)
    | `Succeeded -> Style.fg (Theme.succeeded theme)
    | `Warning -> Style.fg (Theme.warning theme)
    | `Failed -> Style.fg (Theme.failed theme)
    | `Muted -> (
        match Theme.muted theme with
        | Some c -> Style.fg c
        | None -> Style.faint)
    | `Header -> Theme.header theme

  (* Each line is a part of its own: {!lines} splits text, not a drawing. *)
  let role r t =
    unlines
      (List.map
         (fun line ->
           drawn (fun look -> styled (role_style look.Look.theme r) line))
         (lines t))

  (* A line is laid out as the grapheme clusters a terminal draws, each with its
     width. Clusters are found over the line's whole text, so a mark that
     combines with the letter before it joins that letter even across a change
     of style; a cluster takes the style and link of the atom it starts in. A
     layout keeps offsets into the joined text rather than a value per
     cluster. *)

  type layout = {
    joined : string;
    parts : atom array;
    part_starts : int array;  (** Where each atom starts in [joined]. *)
    n : int;  (** The number of clusters. *)
    starts : int array;  (** Where each cluster starts; [[||]] when ASCII. *)
    widths : int array;  (** Each cluster's width; [[||]] when ASCII. *)
    drawn : bool;  (** Whether a part is drawn when printed. *)
  }

  let ascii line = List.for_all (fun a -> Width.is_ascii a.text) line

  let layout line =
    let parts = Array.of_list line in
    let part_starts = Array.make (Array.length parts) 0 in
    let total =
      Array.fold_left
        (fun (i, off) a ->
          part_starts.(i) <- off;
          (i + 1, off + String.length a.text))
        (0, 0) parts
      |> snd
    in
    let text = String.concat "" (List.map (fun a -> a.text) line) in
    let drawn = List.exists is_drawn line in
    if ascii line then
      {
        joined = text;
        parts;
        part_starts;
        n = total;
        starts = [||];
        widths = [||];
        drawn;
      }
    else begin
      let starts = ref [] and widths = ref [] and n = ref 0 in
      Width.iter_clusters
        (fun ~offset ~len:_ ~cells ->
          starts := offset :: !starts;
          widths := cells :: !widths;
          incr n)
        text;
      let starts = Array.of_list (List.rev !starts) in
      let widths = Array.of_list (List.rev !widths) in
      { joined = text; parts; part_starts; n = !n; starts; widths; drawn }
    end

  let start l i =
    if l.starts = [||] then i
    else if i >= l.n then String.length l.joined
    else l.starts.(i)

  let cell_width l i = if l.widths = [||] then 1 else l.widths.(i)

  (* The atom cluster [i] starts in. *)
  let owner l i =
    let off = start l i in
    let k = ref (Array.length l.parts - 1) in
    while !k > 0 && l.part_starts.(!k) > off do
      decr k
    done;
    l.parts.(!k)

  (* A space of a part drawn when printed may be text in another look, as
     the dots of an ASCII ellipsis are: it is not one a break stands on. *)
  let is_space l i =
    l.joined.[start l i] = ' '
    && start l (i + 1) = start l i + 1
    && not (l.drawn && is_drawn (owner l i))

  (* The number of clusters of a line from [first] that fit in [room]
     cells. *)
  let fitting_from l first room =
    let rec take i room =
      if i < l.n && cell_width l i <= room then
        take (i + 1) (room - cell_width l i)
      else i
    in
    take first room - first

  let fitting l room = fitting_from l 0 room

  (* The cluster that starts [cells] cells into a line, or [l.n]. *)
  let at_cell l cells =
    let rec go i c =
      if i >= l.n || c >= cells then i else go (i + 1) (c + cell_width l i)
    in
    go 0 0

  (* The atoms of clusters [i] to [j - 1]. The part of an atom drawn when
     printed is the same cells of its drawing. *)
  let rec sub l i j =
    if i >= j then []
    else
      let a = start l i and b = start l j in
      let acc = ref [] in
      Array.iteri
        (fun k p ->
          let ps = l.part_starts.(k) in
          let pe = ps + String.length p.text in
          let s = max a ps and e = min b pe in
          if s < e then
            let text = String.sub l.joined s (e - s) in
            let draw =
              match p.draw with
              | Some draw when s > ps || e < pe ->
                  let first =
                    Width.string_width (String.sub l.joined ps (s - ps))
                  in
                  let last = first + Width.string_width text in
                  Some (fun look -> cells first last (draw look))
              | d -> d
            in
            acc := { p with text; draw } :: !acc)
        l.parts;
      List.rev !acc

  (* Cells [first] to [last - 1] of a line. *)
  and cells first last line =
    let l = layout line in
    sub l (at_cell l first) (at_cell l last)

  (* A cluster is replaced only by text as wide that holds no control: the
     line keeps its cells. A cluster drawn when printed is kept. *)
  let recell f line =
    let l = layout line in
    let out = ref [] and run = ref 0 and cell = ref 0 in
    for i = 0 to l.n - 1 do
      let w = cell_width l i in
      (match f ~cell:!cell with
      | None -> ()
      | Some g ->
          let a = owner l i in
          if Option.is_none a.draw then begin
            let first = start l i in
            let text = String.sub l.joined first (start l (i + 1) - first) in
            let style, glyph = g a.style text in
            let glyph =
              if clean glyph = glyph && Width.string_width glyph = w then glyph
              else text
            in
            out := [ { a with style; text = glyph } ] :: sub l !run i :: !out;
            run := i + 1
          end);
      cell := !cell + w
    done;
    if !out = [] then line else List.concat (List.rev (sub l !run l.n :: !out))

  (* Each line is graded from its first cluster to its last, each cluster in
     the colour of its place over its own style. *)
  let graded stops line =
    let stops = Array.of_list stops and l = layout line in
    List.concat
      (List.init l.n (fun i ->
           let p =
             if l.n = 1 then 0. else float_of_int i /. float_of_int (l.n - 1)
           in
           let c = at stops p in
           List.map
             (fun a -> { a with style = Style.(a.style + fg c) })
             (sub l i (i + 1))))

  let gradient ?(colors = []) t =
    let line l =
      match colors with
      | [] -> drawn (fun look -> graded (Theme.palette look.Look.theme) l)
      | colors -> graded colors l
    in
    unlines (List.map line (lines t))

  (* A line whose atoms no cluster crosses is as wide as their widths. *)
  let rec apart prev = function
    | [] -> true
    | a :: rest -> (not (Width.joins prev a.text)) && apart a.text rest

  let line_width = function
    | [ a ] -> Width.string_width a.text
    | [] -> 0
    | a :: rest as line ->
        if apart a.text rest then
          List.fold_left (fun n a -> n + Width.string_width a.text) 0 line
        else
          Width.string_width
            (String.concat "" (List.map (fun a -> a.text) line))

  let width t = List.fold_left (fun n l -> max n (line_width l)) 0 (lines t)
  let spaces n = if n <= 0 then empty else text (String.make n ' ')

  (* A line ends at its last span with text on it, a mark that takes no cell
     included: the spans before it are padded to their widths and followed by
     the gap, and nothing follows. *)
  let hcat ?(gap = 2) spans =
    if gap < 0 then invalid_arg "Console.View.hcat: negative gap";
    let blocks =
      List.filter (fun s -> not (is_empty s)) spans
      |> List.map (fun s -> (Array.of_list (seamed s), width s))
      |> Array.of_list
    in
    let height =
      Array.fold_left (fun h (b, _) -> max h (Array.length b)) 0 blocks
    in
    let row i =
      let cell (b, _) = if i < Array.length b then snd b.(i) else empty in
      let last = ref (-1) in
      Array.iteri (fun j b -> if not (is_empty (cell b)) then last := j) blocks;
      List.concat
        (List.init (!last + 1) (fun j ->
             let ((_, w) as b) = blocks.(j) in
             let c = cell b in
             if j = !last then c else c @ spaces (w - line_width c + gap)))
    in
    (* A row goes on the block above it if one of its spans' lines does. *)
    let joined i =
      Array.exists (fun (b, _) -> i < Array.length b && fst b.(i)) blocks
    in
    List.concat
      (List.init height (fun i ->
           if i = 0 then row i
           else (if joined i then joint else text "\n") @ row i))

  let vcat ~gap spans =
    concat
      ~sep:(text (String.make (gap + 1) '\n'))
      (List.filter (fun s -> not (is_empty s)) spans)

  let height t = if is_empty t then 0 else List.length (lines t)
  let ellipsis = "\xe2\x80\xa6"

  (* An ellipsis wider than [w], such as ASCII dots, keeps its first clusters
     that fit in [w]. It takes the style of the cluster it replaces, as that
     cluster is drawn. *)
  let truncate_line ~ellipsis w line =
    if line_width line <= w then line
    else
      let ellipsis =
        if Width.string_width ellipsis <= w then ellipsis
        else
          let e = layout (text ellipsis) in
          String.sub e.joined 0 (start e (fitting e w))
      in
      let l = layout line in
      let k = fitting l (w - Width.string_width ellipsis) in
      let style = if k < l.n then (owner l k).style else Style.none in
      let draw =
        if k < l.n && is_drawn (owner l k) then
          let cut =
            List.map (fun a -> { a with style = Style.none }) (sub l k (k + 1))
          in
          Some
            (fun look ->
              let style =
                match resolve look cut with
                | a :: _ -> a.style
                | [] -> Style.none
              in
              [
                {
                  style;
                  link = None;
                  text = ellipsis;
                  draw = None;
                  block = false;
                };
              ])
        else None
      in
      sub l 0 k
      @ [ { style; link = None; text = ellipsis; draw; block = false } ]

  let truncate ?(ellipsis = ellipsis) w t =
    if w <= 0 then empty
    else unlines (List.map (truncate_line ~ellipsis w) (lines t))

  (* A wide cluster cut by the left edge leaves its cells in the window
     blank; one cut by the right edge is left out. *)
  let crop_line ~left ~width line =
    let l = layout line in
    let rec skip i c =
      if i < l.n && c < left then skip (i + 1) (c + cell_width l i) else (i, c)
    in
    let first, c = skip 0 0 in
    let lead = min width (c - left) in
    let last = first + fitting_from l first (width - max 0 lead) in
    spaces lead @ sub l first last

  let crop ~left ~width t =
    if width <= 0 then empty
    else unlines (List.map (crop_line ~left ~width) (lines t))

  (* The clusters that fit go; a wide one cut by the edge leaves its cell
     blank. *)
  let fit w line =
    let line =
      if line_width line <= w then line
      else
        let l = layout line in
        sub l 0 (fitting l w)
    in
    match w - line_width line with
    | n when n > 0 -> line @ text (String.make n ' ')
    | _ -> line

  (* A line being broken into lines of at most [w] cells, those after the
     first [hang] cells in: the lines done, newest first, and the ranges of
     clusters of the line under way, newest first. *)
  type filler = {
    l : layout;
    w : int;
    hang : int;
    mutable out : t list;
    mutable cur : (int * int) list;
    mutable cur_w : int;
    mutable first : bool;
  }

  let room f = if f.first then f.w else f.w - f.hang

  let range_width l i j =
    let n = ref 0 in
    for k = i to j - 1 do
      n := !n + cell_width l k
    done;
    !n

  let add f i j =
    (match f.cur with
    | (a, b) :: rest when b = i -> f.cur <- (a, j) :: rest
    | _ -> f.cur <- (i, j) :: f.cur);
    f.cur_w <- f.cur_w + range_width f.l i j

  (* The spaces a break stands on are not drawn. *)
  let rec trim l = function
    | (a, b) :: rest when b > a && is_space l (b - 1) ->
        trim l (if b - 1 > a then (a, b - 1) :: rest else rest)
    | r -> r

  let finish f =
    let body =
      List.concat_map (fun (a, b) -> sub f.l a b) (List.rev (trim f.l f.cur))
    in
    let body =
      if f.first || f.hang = 0 then body
      else
        {
          style = Style.none;
          link = None;
          text = String.make f.hang ' ';
          draw = None;
          block = false;
        }
        :: body
    in
    f.out <- body :: f.out;
    f.cur <- [];
    f.cur_w <- 0;
    f.first <- false

  let has_text f =
    List.exists
      (fun (a, b) ->
        let r = ref false in
        for k = a to b - 1 do
          if not (is_space f.l k) then r := true
        done;
        !r)
      f.cur

  (* A word wider than the room goes on where the room ends; a cluster wider
     than a whole line cannot be drawn and is left out, also when it is wider
     than the room a hanging line leaves after the first. *)
  let split f i j =
    for k = i to j - 1 do
      let cw = cell_width f.l k in
      if cw <= room f then begin
        if f.cur_w + cw > room f then finish f;
        if cw <= room f then add f k (k + 1)
      end
    done

  let rec word f i j =
    if f.cur_w + range_width f.l i j <= room f then add f i j
    else if has_text f then begin
      finish f;
      word f i j
    end
    else split f i j

  let spaces f i j =
    if f.cur = [] && not f.first then ()
    else if f.cur_w + range_width f.l i j <= room f then add f i j
    else if f.cur <> [] then finish f

  (* Words end at a run of spaces, or after a slash. *)
  let rec fill f i =
    let l = f.l in
    if i < l.n then begin
      let j = ref i in
      if is_space l i then begin
        while !j < l.n && is_space l !j do
          incr j
        done;
        spaces f i !j
      end
      else begin
        let stop = ref false in
        while (not !stop) && !j < l.n && not (is_space l !j) do
          let slash = l.joined.[start l !j] = '/' in
          incr j;
          if slash then stop := true
        done;
        word f i !j
      end;
      fill f !j
    end

  let wrap_line ~hang w line =
    let f =
      { l = layout line; w; hang; out = []; cur = []; cur_w = 0; first = true }
    in
    fill f 0;
    if f.cur <> [] || f.out = [] then finish f;
    List.rev f.out

  let widest_word t =
    List.fold_left
      (fun acc line ->
        let l = layout line in
        let best = ref acc and run = ref 0 in
        for i = 0 to l.n - 1 do
          if is_space l i then run := 0
          else begin
            run := !run + cell_width l i;
            best := max !best !run
          end
        done;
        !best)
      0 (lines t)

  (* The cells a line's continuation hangs under: its leading spaces, its first
     word and the spaces after it, when more text follows. *)
  let hanging line =
    let l = layout line in
    let i = ref 0 and w = ref 0 in
    let skip p =
      while !i < l.n && p !i do
        w := !w + cell_width l !i;
        incr i
      done
    in
    skip (is_space l);
    skip (fun k -> not (is_space l k));
    skip (is_space l);
    if !i >= l.n then 0 else !w

  let wrap_with ~hanging:h w t =
    if w <= 0 then invalid_arg "Console.View.wrap: non-positive width";
    List.concat_map
      (fun line ->
        let hang = if h then max 0 (min (hanging line) (w / 2)) else 0 in
        wrap_line ~hang w line)
      (lines t)

  let wrap w t = wrap_with ~hanging:false w t
  let wrap_hanging w t = wrap_with ~hanging:true w t

  let chop w t =
    if w <= 0 then invalid_arg "Console.View_core.chop: non-positive width";
    List.concat_map
      (fun line ->
        let f =
          {
            l = layout line;
            w;
            hang = 0;
            out = [];
            cur = [];
            cur_w = 0;
            first = true;
          }
        in
        split f 0 f.l.n;
        if f.cur <> [] || f.out = [] then finish f;
        List.rev f.out)
      (lines t)

  let osc8_open uri = "\027]8;;" ^ uri ^ "\027\\"
  let osc8_close = "\027]8;;\027\\"

  (* Neighbouring atoms of one style and link are written under one escape
     code. *)
  let runs t =
    let rec go acc = function
      | a :: b :: rest
        when Style.equal a.style b.style
             && Option.equal String.equal a.link b.link ->
          go acc ({ a with text = a.text ^ b.text } :: rest)
      | a :: rest -> go (a :: acc) rest
      | [] -> List.rev acc
    in
    go [] t

  (* Styles and links are closed before each line feed and opened again after
     it, so that no line leaves them on for what the terminal prints next. *)
  let add_styled buf ~depth t =
    List.iter
      (fun { style; link; text; _ } ->
        let sgr = Style.sgr depth style in
        List.iteri
          (fun i line ->
            if i > 0 then Buffer.add_char buf '\n';
            if line <> "" then begin
              Option.iter
                (fun uri -> Buffer.add_string buf (osc8_open uri))
                link;
              Buffer.add_string buf sgr;
              Buffer.add_string buf line;
              if sgr <> "" then Buffer.add_string buf Sgr.reset;
              Option.iter (fun _ -> Buffer.add_string buf osc8_close) link
            end)
          (String.split_on_char '\n' text))
      (runs t)

  let render ~look ?depth t =
    let t = resolve look t in
    let buf = Buffer.create 64 in
    (match depth with
    | None -> List.iter (fun a -> Buffer.add_string buf a.text) t
    | Some depth -> add_styled buf ~depth t);
    Buffer.contents buf

  let to_string ?(styled = false) t =
    render ~look:Look.default
      ?depth:(if styled then Some `True_color else None)
      t

  let pp ppf t =
    let t = if List.exists is_drawn t then resolve (Look.v ppf) t else t in
    match Fmt.style_renderer ppf with
    | `None -> List.iter (fun a -> Format.pp_print_string ppf a.text) t
    | `Ansi_tty ->
        Format.pp_print_string ppf
          (render ~look:Look.default ~depth:(Color.depth ppf) t)

  let pp_lines ppf lines =
    List.iteri
      (fun i line ->
        if i > 0 then Format.pp_print_char ppf '\n';
        pp ppf line)
      lines

  let string_of_lines ?(styled = false) lines =
    String.concat "\n" (List.map (to_string ~styled) lines)
end

(* A view is either flat, its atoms known, or deferred: laid out in the room
   and at the time it is printed in. A deferred part says what it reads
   besides its room: the time, the tasks, and other state outside the view.
   One that reads none caches its layout for its room; one that reads the
   time but no state caches its still size, which every frame of it takes. *)

type kind = { live : bool; tasks : bool; timed : bool }

let static = { live = false; tasks = false; timed = false }
let timed = { static with timed = true }
let tasks = { static with tasks = true; timed = true }
let moving = { static with live = true; timed = true }

(* One of the two when it reads all the other does, as most do, so that a
   frame that asks what its view reads allocates nothing. *)
let join a b =
  let covers a b =
    (a.live || not b.live) && (a.tasks || not b.tasks)
    && (a.timed || not b.timed)
  in
  if covers a b then a
  else if covers b a then b
  else
    {
      live = a.live || b.live;
      tasks = a.tasks || b.tasks;
      timed = a.timed || b.timed;
    }

type t = Flat of F.t | Deferred of node

and node = {
  kind : Look.t -> kind;
  lay : Ctx.t -> t;
  grow : int;
  min_width : (Ctx.t -> int) option;
  still : bool;
  mutable memo : (Ctx.t * F.t) option;
  mutable size : (Ctx.t * int list) option;
}

let kind t look = match t with Flat _ -> static | Deferred d -> d.kind look
let kinds ts look = List.fold_left (fun k t -> join k (kind t look)) static ts
let is_flat = function Flat _ -> true | Deferred _ -> false
let grow = function Flat _ -> 0 | Deferred d -> d.grow

let deferred ?(kind = fun _ -> static) ?(grow = 0) ?min_width ?(still = false)
    lay =
  let min_width = Option.map Fun.const min_width in
  Deferred { kind; lay; grow; min_width; still; memo = None; size = None }

(* Each line of [f] cut or padded to the cells of its line in a still frame,
   blank lines where it has fewer and none past them. *)
let to_size widths f =
  let lines = Array.of_list (F.seamed f) in
  F.unseam
    (List.mapi
       (fun i w ->
         if i < Array.length lines then
           let j, l = lines.(i) in
           (j, F.fit w l)
         else (false, F.fit w F.empty))
       widths)

let line_widths f = List.map F.width (F.lines f)

let rec flat_in c = function
  | Flat f -> f
  | Deferred d ->
      let k = d.kind (Ctx.look c) in
      let stateful = k.live || k.tasks in
      if (not stateful) && not k.timed then (
        match d.memo with
        | Some (c', f) when Ctx.same_room c c' -> f
        | _ ->
            let f = flat_in c (d.lay c) in
            d.memo <- Some (c, f);
            f)
      else
        let f = flat_in c (d.lay c) in
        if not d.still then f
        else
          let widths =
            match d.size with
            | Some (c', w) when (not stateful) && Ctx.same_room c c' -> w
            | _ ->
                let w =
                  if Ctx.time c = 0. then line_widths f
                  else line_widths (flat_in (Ctx.still c) (d.lay (Ctx.still c)))
                in
                if not stateful then d.size <- Some (c, w);
                w
          in
          if F.is_empty f && widths = [] then f else to_size widths f

let resolve c t = match t with Flat _ -> t | Deferred _ -> Flat (flat_in c t)

(* Where nothing gives a room: the natural size, in the default look, at
   time 0. *)
let flat t = flat_in Ctx.default t

let rec min_width c = function
  | Flat f -> F.widest_word f
  | Deferred { min_width = Some w; _ } -> w c
  | Deferred d -> min_width c (d.lay c)

(* [f] of the atoms of [ts], at once if they are all known, and otherwise
   where they are printed, each in the room [room] gives. *)
let lift ?(room = Fun.id) ts f =
  if List.for_all is_flat ts then Flat (f (List.map flat ts))
  else
    deferred ~kind:(kinds ts) (fun c ->
        let c = room c in
        Flat (f (List.map (flat_in c) ts)))

let map ?(room = Fun.id) f = function
  | Flat x -> Flat (f x)
  | t -> deferred ~kind:(kind t) (fun c -> Flat (f (flat_in (room c) t)))

let fit ?min_width ?grow f =
  (match grow with
  | Some g when g < 0 -> invalid_arg "Console.View.fit: negative grow"
  | _ -> ());
  deferred ~kind:(fun _ -> moving) ?grow ?min_width ~still:true f

let sized ?width ?height t =
  match t with
  | Flat _ -> t
  | Deferred _ ->
      let pick given outer =
        match (given, outer) with
        | Some g, Some o -> Some (min g o)
        | Some _, None -> given
        | None, _ -> outer
      in
      deferred ~kind:(kind t) (fun c ->
          Flat
            (flat_in
               (Ctx.with_room
                  ~width:(pick width (Ctx.width c))
                  ~height:(pick height (Ctx.height c))
                  c)
               t))

(* [f] of the atoms of [t], [f] adding no cell, so that [t] keeps its weight
   in an [hcat] and the width below which it should not be laid out. *)
let restyle f = function
  | Flat x -> Flat (f x)
  | t ->
      Deferred
        {
          kind = kind t;
          lay = (fun c -> Flat (f (flat_in c t)));
          grow = grow t;
          min_width = Some (fun c -> min_width c t);
          still = false;
          memo = None;
          size = None;
        }

let empty = Flat F.empty
let text s = Flat (F.text s)
let ink style s = Flat (F.ink style s)
let is_empty t = F.is_empty (flat t)
let link ~uri t = restyle (F.link ~uri) t
let styled s t = restyle (F.styled s) t
let role r t = restyle (F.role r) t
let gradient ?colors t = restyle (F.gradient ?colors) t

let ( ++ ) a b =
  match (a, b) with
  | Flat a, Flat b -> Flat F.(a ++ b)
  | _ -> lift [ a; b ] (function [ a; b ] -> F.(a ++ b) | _ -> assert false)

let concat ?(sep = empty) ts =
  lift (sep :: ts) (function
    | sep :: ts -> F.concat ~sep ts
    | [] -> assert false)

let vcat ?(gap = 0) ts =
  if gap < 0 then invalid_arg "Console.View.vcat: negative gap";
  lift ts (F.vcat ~gap)

(* [room] shared by [weights], each positive, exactly: each takes the
   whole part of its quota, and the cells left go one each to the largest
   remainders, the first on a tie. Weights too large for [room * weight]
   are scaled down first. *)
let apportion room weights =
  let n = List.length weights in
  if n = 0 then []
  else
    let biggest = List.fold_left max 0 weights in
    let limit = max 1 (max_int / max 1 room / max 1 n) in
    let weights =
      if biggest <= limit then weights
      else
        let d = (biggest / limit) + 1 in
        List.map (fun g -> max 1 (g / d)) weights
    in
    let total = List.fold_left ( + ) 0 weights in
    let quotas = Array.of_list (List.map (fun g -> room * g) weights) in
    let shares = Array.map (fun q -> q / total) quotas in
    let rest = room - Array.fold_left ( + ) 0 shares in
    let order = Array.init n Fun.id in
    Array.stable_sort
      (fun i j -> compare (quotas.(j) mod total) (quotas.(i) mod total))
      order;
    for k = 0 to rest - 1 do
      let i = order.(k) in
      shares.(i) <- shares.(i) + 1
    done;
    Array.to_list shares

(* Under a room [w] wide, the parts that do not grow take, left to right,
   the room still left; those that grow share what is left by their
   weights ({!apportion}). Without a room, each takes its natural size. *)
let share c ~gap ts =
  match Ctx.width c with
  | None ->
      List.map (flat_in (Ctx.with_room ~width:None ~height:(Ctx.height c) c)) ts
  | Some w ->
      let left = ref w and shown = ref 0 in
      let fixed =
        List.map
          (fun t ->
            if grow t > 0 then None
            else
              let room = max 0 (!left - if !shown > 0 then gap else 0) in
              let f =
                flat_in
                  (Ctx.with_room ~width:(Some room) ~height:(Ctx.height c) c)
                  t
              in
              if not (F.is_empty f) then begin
                left := room - F.width f;
                incr shown
              end;
              Some f)
          ts
      in
      let growers = List.filter (fun t -> grow t > 0) ts in
      let n = List.length growers in
      let gaps = if n = 0 then 0 else if !shown > 0 then n else n - 1 in
      let room =
        if gaps > 0 && gap > !left / gaps then 0 else !left - (gap * gaps)
      in
      let shares = ref (apportion room (List.map grow growers)) in
      List.map2
        (fun t f ->
          match (f, !shares) with
          | Some f, _ -> f
          | None, share :: rest ->
              shares := rest;
              flat_in
                (Ctx.with_room ~width:(Some share) ~height:(Ctx.height c) c)
                t
          | None, [] -> assert false)
        ts fixed

let hcat ?(gap = 2) ts =
  if gap < 0 then invalid_arg "Console.View.hcat: negative gap";
  if List.for_all is_flat ts then Flat (F.hcat ~gap (List.map flat ts))
  else
    let grows = List.exists (fun t -> grow t > 0) ts in
    deferred ~kind:(kinds ts)
      ~grow:(if grows then 1 else 0)
      (fun c -> Flat (F.hcat ~gap (share c ~gap ts)))

let width t = F.width (flat t)
let height t = F.height (flat t)

(* A view cut where it is printed ends its lines with the look's ellipsis. *)
let truncate ?ellipsis w t =
  match (ellipsis, t) with
  | Some e, _ -> map ~room:Ctx.natural (F.truncate ~ellipsis:e w) t
  | None, Flat f when w <= 0 || F.width f <= w -> Flat (F.truncate w f)
  | None, _ ->
      deferred ~kind:(kind t) (fun c ->
          let ellipsis = Look.ellipsis (Ctx.look c) in
          Flat (F.truncate ~ellipsis w (flat_in (Ctx.natural c) t)))

let crop ?(left = 0) ?(width = max_int) t =
  if left < 0 then invalid_arg "Console.View.crop: negative left";
  map ~room:Ctx.natural (F.crop ~left ~width) t

(* A line [w] cells wide at [align], cut with [ellipsis] if wider: a cut
   inside a wide character leaves a cell to pad. *)
let place_line ~ellipsis align w line =
  let line = if F.width line > w then F.truncate ~ellipsis w line else line in
  let pad = w - F.width line in
  let left, right =
    match align with
    | `Left -> (0, pad)
    | `Right -> (pad, 0)
    | `Center -> (pad / 2, pad - (pad / 2))
  in
  let sp n = if n = 0 then F.empty else F.text (String.make n ' ') in
  F.concat [ sp left; line; sp right ]

let place ?(align = `Left) ?width ?height t =
  let placed ellipsis f =
    let lines = F.lines f in
    let lines =
      match height with
      | None -> lines
      | Some h ->
          let h = max 0 h in
          List.init h (fun i ->
              match List.nth_opt lines i with Some l -> l | None -> F.empty)
    in
    let w = match width with Some w -> max 0 w | None -> F.width f in
    F.unlines (List.map (place_line ~ellipsis align w) lines)
  in
  let cuts f =
    match width with Some w -> F.width f > max 0 w | None -> false
  in
  match t with
  | Flat f when not (cuts f) -> Flat (placed F.ellipsis f)
  | _ ->
      deferred ~kind:(kind t) (fun c ->
          let ellipsis = Look.ellipsis (Ctx.look c) in
          let c =
            Ctx.with_room
              ~width:(match width with Some _ -> width | None -> Ctx.width c)
              ~height:
                (match height with Some _ -> height | None -> Ctx.height c)
              c
          in
          Flat (placed ellipsis (flat_in c t)))

(* A deferred view kept as lines, as {!wrap} and {!lines} keep it, is laid
   out at its natural size in the default look, each line a part drawn in the
   look it is printed in, cut or padded to the cells it takes in the default
   look. *)
let keep t =
  match t with
  | Flat f -> f
  | Deferred _ ->
      let base = F.seamed (flat t) in
      let last = ref None in
      let laid (look : Look.t) =
        match !last with
        | Some (l, a) when Look.equal l look -> a
        | _ ->
            let a = Array.of_list (F.lines (flat_in (Ctx.v look) t)) in
            last := Some (look, a);
            a
      in
      let line i (joined, l) =
        let w = F.width l in
        let draw look =
          if Look.equal look Look.default then l
          else
            let a = laid look in
            F.fit w (if i < Array.length a then a.(i) else F.empty)
        in
        (joined, F.drawn draw)
      in
      F.unseam (List.mapi line base)

let wrap w t = List.map (fun f -> Flat f) (F.wrap w (keep t))
let wrap_hanging w t = List.map (fun f -> Flat f) (F.wrap_hanging w (keep t))
let chop w t = List.map (fun f -> Flat f) (F.chop w (keep t))
let lines t = List.map (fun f -> Flat f) (F.lines (keep t))
let seamed t = List.map (fun (j, f) -> (j, Flat f)) (F.seamed (flat t))
let block ts = Flat (F.block (List.map flat ts))
let widest_word t = F.widest_word (flat t)
let pad_to w t = Flat (F.fit w (flat t))
let recell f t = Flat (F.recell f (flat t))
let color_at = F.color_at
let ellipsis = F.ellipsis
let drawn draw = Flat (F.drawn (fun look -> flat (draw look)))
let paint look t = Flat (F.resolve look (flat t))
let render ~look ?depth t = F.render ~look ?depth (flat t)

let pp ppf t =
  F.pp ppf (flat_in (Ctx.v ~width:(Format.pp_get_margin ppf ()) (Look.v ppf)) t)

let to_string ?styled t = F.to_string ?styled (flat t)
let pp_lines ppf ts = F.pp_lines ppf (List.map flat ts)
let string_of_lines ?styled ts = F.string_of_lines ?styled (List.map flat ts)
