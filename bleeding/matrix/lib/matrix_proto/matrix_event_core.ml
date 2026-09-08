module Timestamp = struct
  type t = int64

  let of_ms t = t
  let to_ms t = t

  let of_ptime pt =
    let span = Ptime.to_span pt in
    let d, ps = Ptime.Span.to_d_ps span in
    let days_ms = Int64.mul (Int64.of_int d) 86_400_000L in
    let ps_ms = Int64.div ps 1_000_000_000L in
    Int64.add days_ms ps_ms

  (* Floor division, so that an instant before the epoch keeps a picosecond
     count in the range [Ptime.Span.of_d_ps] accepts. *)
  let to_ptime_opt t =
    let days = Int64.div t 86_400_000L and rem_ms = Int64.rem t 86_400_000L in
    let days, rem_ms =
      if Int64.compare rem_ms 0L < 0 then
        (Int64.sub days 1L, Int64.add rem_ms 86_400_000L)
      else (days, rem_ms)
    in
    let ps = Int64.mul rem_ms 1_000_000_000L in
    Ptime.Span.of_d_ps (Int64.to_int days, ps)
    |> Option.map Ptime.of_span |> Option.join

  let equal = Int64.equal
  let compare = Int64.compare
  let pp ppf t = Format.fprintf ppf "%Ld" t
  let jsont = Matrix_json.Codec.int64
end

module Rel_type = struct
  type t = Annotation | Reference | Replace | Thread | Custom of string

  let to_string = function
    | Annotation -> "m.annotation"
    | Reference -> "m.reference"
    | Replace -> "m.replace"
    | Thread -> "m.thread"
    | Custom s -> s

  let of_string = function
    | "m.annotation" -> Annotation
    | "m.reference" -> Reference
    | "m.replace" -> Replace
    | "m.thread" -> Thread
    | s -> Custom s

  let equal a b = a = b
  let pp ppf t = Format.pp_print_string ppf (to_string t)

  let jsont =
    Jsont.of_of_string ~kind:"rel_type" ~enc:to_string (fun s ->
        Ok (of_string s))
end

module Relates_to = struct
  type t = {
    rel_type : Rel_type.t;
    event_id : Matrix_id.Event_id.t;
    key : string option;
  }

  let v ?key ~rel_type event_id = { rel_type; event_id; key }
  let reference event_id = v ~rel_type:Rel_type.Reference event_id

  let equal a b =
    Rel_type.equal a.rel_type b.rel_type
    && Matrix_id.Event_id.equal a.event_id b.event_id
    && Option.equal String.equal a.key b.key

  let pp ppf t =
    Format.fprintf ppf "@[%a -> %a@]" Rel_type.pp t.rel_type
      Matrix_id.Event_id.pp t.event_id

  let jsont =
    Jsont.Object.(
      map (fun rel_type event_id key -> { rel_type; event_id; key })
      |> mem "rel_type" Rel_type.jsont
           ~dec_absent:(fun () -> Rel_type.Reference)
           ~enc:(fun t -> t.rel_type)
      |> mem "event_id" Matrix_id.Event_id.jsont ~enc:(fun t -> t.event_id)
      |> opt_mem "key" Matrix_json.Codec.string ~enc:(fun t -> t.key)
      |> finish)
end

module Image_info = struct
  type t = {
    mimetype : string option;
    size : int option;
    h : int option;
    w : int option;
  }

  let v ?mimetype ?size ?h ?w () = { mimetype; size; h; w }

  let pp ppf t =
    Format.fprintf ppf "@[<hov 2>image_info:";
    (match (t.w, t.h) with
    | Some w, Some h -> Format.fprintf ppf "@ %dx%d" w h
    | _ -> ());
    (match t.mimetype with Some m -> Format.fprintf ppf "@ %s" m | None -> ());
    (match t.size with
    | Some s -> Format.fprintf ppf "@ %d bytes" s
    | None -> ());
    Format.fprintf ppf "@]"

  let jsont =
    Jsont.Object.(
      map (fun mimetype size h w -> { mimetype; size; h; w })
      |> opt_mem "mimetype" Matrix_json.Codec.string ~enc:(fun t -> t.mimetype)
      |> opt_mem "size" Matrix_json.Codec.int ~enc:(fun t -> t.size)
      |> opt_mem "h" Matrix_json.Codec.int ~enc:(fun t -> t.h)
      |> opt_mem "w" Matrix_json.Codec.int ~enc:(fun t -> t.w)
      |> finish)
end
