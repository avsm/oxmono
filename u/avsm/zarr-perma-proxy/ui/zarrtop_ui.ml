open! Core
open Bonsai_term
open Bonsai.Let_syntax

let member name = function
  | `Assoc fields ->
      Option.value
        (List.Assoc.find fields ~equal:String.equal name)
        ~default:`Null
  | _ -> `Null

let list = function `List xs -> xs | _ -> []
let text = function `String s -> s | _ -> ""

let number = function
  | `Int n -> Float.of_int n
  | `Intlit n -> Float.of_string n
  | `Float n -> n
  | _ -> 0.

let n row key = number (member key row)
let s row key = text (member key row)

let first json key =
  Option.value (List.hd (list (member key json))) ~default:`Null

let bytes value =
  if Float.(value >= 1073741824.) then sprintf "%.1f GiB" (value /. 1073741824.)
  else if Float.(value >= 1048576.) then sprintf "%.1f MiB" (value /. 1048576.)
  else if Float.(value >= 1024.) then sprintf "%.1f KiB" (value /. 1024.)
  else sprintf "%.0f B" value

let blue = Attr.Color.rgb ~r:110 ~g:180 ~b:245
let green = Attr.Color.rgb ~r:120 ~g:205 ~b:145
let amber = Attr.Color.rgb ~r:235 ~g:190 ~b:100
let red = Attr.Color.rgb ~r:245 ~g:125 ~b:125
let line ?(color = blue) string = View.text ~attrs:[ Attr.fg color ] string

let box title view =
  Bonsai_term_border_box.view ~title
    ~title_attrs:[ Attr.bold; Attr.fg blue ]
    view

let fit dimensions view =
  View.crop
    ~r:(Int.max 0 (View.width view - dimensions.Dimensions.width))
    ~b:(Int.max 0 (View.height view - dimensions.height))
    view

type model = {
  sample : Yojson.Safe.t option;
  error : string option;
  paused : bool;
  tab : int;
  filter : string;
  editing : bool;
  requests_per_sec : float;
  bytes_per_sec : float;
}

let initial =
  {
    sample = None;
    error = None;
    paused = false;
    tab = 0;
    filter = "";
    editing = false;
    requests_per_sec = 0.;
    bytes_per_sec = 0.;
  }

type action =
  | Key of Event.t * unit Effect.t
  | Sample of (string, string) Result.t
  | Pause
  | Tab
  | Filter
  | Type of char
  | Backspace
  | Done

let update model = function
  | Key _ -> model
  | Pause -> { model with paused = not model.paused }
  | Tab -> { model with tab = (model.tab + 1) mod 3 }
  | Filter -> { model with editing = true }
  | Type c -> { model with filter = model.filter ^ String.of_char c }
  | Backspace -> { model with filter = String.drop_suffix model.filter 1 }
  | Done -> { model with editing = false }
  | Sample (Error error) -> { model with error = Some error }
  | Sample (Ok raw) -> (
      try
        let sample = Yojson.Safe.from_string raw in
        let runtime = first sample "runtime" in
        let requests_per_sec, bytes_per_sec =
          match model.sample with
          | Some old
            when String.equal
                   (s (first old "runtime") "session")
                   (s runtime "session") ->
              let elapsed = n sample "monotonic" -. n old "monotonic" in
              if Float.(elapsed <= 0.) then (0., 0.)
              else
                let a = first sample "totals" and b = first old "totals" in
                ( Float.max 0. ((n a "requests" -. n b "requests") /. elapsed),
                  Float.max 0.
                    ((n a "served_bytes" -. n b "served_bytes") /. elapsed) )
          | _ -> (0., 0.)
        in
        {
          model with
          sample = Some sample;
          error = None;
          requests_per_sec;
          bytes_per_sec;
        }
      with _ -> { model with error = Some "Invalid telemetry snapshot" })

let is_live sample now =
  let runtime = first sample "runtime" in
  let age =
    Time_ns.Span.to_sec (Time_ns.to_span_since_epoch now)
    -. n runtime "heartbeat"
  in
  Float.(n runtime "running" = 1. && age >= 0. && age < 5.)

let header model now =
  match model.sample with
  | None -> line ~color:amber "Waiting for telemetry..."
  | Some sample ->
      let live = first sample "runtime"
      and totals = first sample "totals"
      and traffic = first sample "traffic" in
      let online = is_live sample now in
      let transfers =
        try Yojson.Safe.from_string (s live "transfers") |> list with _ -> []
      in
      let classified =
        n totals "hits" +. n totals "misses" +. n totals "shared"
      in
      let rate value =
        if Float.(classified = 0.) then "--"
        else sprintf "%.1f%%" (100. *. value /. classified)
      in
      let gauges =
        if online then
          sprintf "Connections %.0f   Downloads %d   Queued %.0f"
            (n live "connections") (List.length transfers) (n live "queued")
        else "Connections --   Downloads --   Queued --"
      in
      View.vcat
        [
          line
            ~color:(if online then green else amber)
            (sprintf "%s%s   %s"
               (if online then "LIVE" else "OFFLINE / STALE")
               (if model.paused then "  PAUSED" else "")
               gauges);
          line
            (sprintf "Hit %s   Avoided fetch %s   Requests %.0f   Errors %.0f"
               (rate (n totals "hits"))
               (rate (n totals "hits" +. n totals "shared"))
               (n totals "requests") (n totals "errors"));
          line
            (sprintf "%.1f req/s   %s/s out   Served %s   Fetched %s"
               model.requests_per_sec
               (bytes model.bytes_per_sec)
               (bytes (n totals "served_bytes"))
               (bytes (n traffic "fetched_bytes")));
        ]

let rows model now =
  match model.sample with
  | None -> View.text "Start the proxy, or select an existing --cache-dir."
  | Some sample when model.tab = 2 && not (is_live sample now) ->
      View.text "No live heartbeat. Old transfer gauges are hidden."
  | Some sample ->
      let rows =
        if model.tab = 0 then list (member "requests" sample)
        else if model.tab = 1 then list (member "downloads" sample)
        else
          try
            Yojson.Safe.from_string (s (first sample "runtime") "transfers")
            |> list
          with _ -> []
      in
      let rendered =
        List.map rows ~f:(fun row ->
            if model.tab = 0 then
              let cache = s row "cache" in
              ( sprintf "%-7s %3.0f %-6s %-5s %10s %7.1fms  %s" cache
                  (n row "status") (s row "method")
                  (if Float.(n row "range_request" = 1.) then "range"
                   else "whole")
                  (bytes (n row "served_bytes"))
                  (n row "duration_us" /. 1000.)
                  (s row "path"),
                if Float.(n row "status" >= 400.) then red
                else if String.equal cache "HIT" then green
                else amber )
            else if model.tab = 1 then
              ( sprintf "%-9s %3s %10s %8.1fms  %s" (s row "outcome")
                  (if Poly.equal (member "status" row) `Null then "--"
                   else sprintf "%.0f" (n row "status"))
                  (bytes (n row "fetched_bytes"))
                  (n row "duration_us" /. 1000.)
                  (s row "object_key"),
                blue )
            else
              ( sprintf "%7.1fs %10s / %-10s  %s" (n row "age")
                  (bytes (n row "bytes"))
                  (if Poly.equal (member "total" row) `Null then "unknown"
                   else bytes (n row "total"))
                  (s row "path"),
                amber ))
      in
      let rendered =
        List.filter rendered ~f:(fun (row, _) ->
            String.is_substring (String.lowercase row)
              ~substring:(String.lowercase model.filter))
      in
      let columns =
        if model.tab = 0 then
          sprintf "%-7s %3s %-6s %-5s %10s %9s  %s" "CACHE" "HTTP" "METHOD"
            "TYPE" "SERVED" "LATENCY" "PATH"
        else if model.tab = 1 then
          sprintf "%-9s %3s %10s %10s  %s" "OUTCOME" "HTTP" "FETCHED" "DURATION"
            "OBJECT KEY"
        else
          sprintf "%8s %10s   %-10s  %s" "AGE" "RECEIVED" "TOTAL"
            "UPSTREAM PATH"
      in
      View.vcat
        [
          View.text ~attrs:[ Attr.bold; Attr.fg blue ] columns;
          (if List.is_empty rendered then View.text "No matching activity."
           else
             View.vcat
               (List.map rendered ~f:(fun (row, color) -> line ~color row)));
        ]

let app ~poll ~interval ~exit ~dimensions (local_ graph) =
  let apply_action context model = function
    | Key (event, fallback) -> (
        let run scheduled =
          Bonsai.Apply_action_context.schedule_event context scheduled;
          model
        in
        match event with
        | Event.Key_press { key = ASCII ('c' | 'C'); mods = [ Ctrl ] } ->
            run (exit ())
        | Key_press { key = Enter | Escape; mods = [] } when model.editing ->
            update model Done
        | Key_press { key = Backspace; mods = [] } when model.editing ->
            update model Backspace
        | Key_press { key = ASCII c; mods = [] } when model.editing ->
            update model (Type c)
        | Key_press { key = ASCII 'q'; mods = [] } -> run (exit ())
        | Key_press { key = Tab; mods = [] } -> update model Tab
        | Key_press { key = ASCII '/'; mods = [] } -> update model Filter
        | Key_press { key = ASCII ' '; mods = [] } -> update model Pause
        | _ -> if model.editing then model else run fallback)
    | action -> update model action
  in
  let model, inject =
    Bonsai.state_machine ~default_model:initial ~apply_action graph
  in
  Bonsai.Clock.every ~trigger_on_activate:true
    ~when_to_start_next_effect:
      `Wait_period_after_previous_effect_finishes_blocking
    (Bonsai.return (Time_ns.Span.of_sec interval))
    (let%arr model = model and inject = inject in
     if model.paused then Effect.Ignore
     else
       let%bind.Effect result = poll () in
       inject (Sample result))
    graph;
  let now =
    Bonsai.Clock.approx_now ~tick_every:(Time_ns.Span.of_sec 1.) graph
  in
  let content =
    let%arr model = model and now = now in
    rows model now
  in
  let inner =
    let%arr dimensions = dimensions in
    {
      Dimensions.width = Int.max 1 (dimensions.Dimensions.width - 2);
      height = Int.max 1 (dimensions.Dimensions.height - 10);
    }
  in
  let%sub { view = body; less_keybindings_handler; _ } =
    Bonsai_term_scroller.component ~crop_width_if_too_big:`Yes ~dimensions:inner
      content graph
  in
  let view =
    let%arr model = model
    and body = body
    and now = now
    and dimensions = dimensions in
    let title =
      match model.tab with
      | 0 -> "Recent requests (latest 200)"
      | 1 -> "Recent downloads (latest 200)"
      | _ -> "Active downloads (heartbeat sample)"
    in
    fit dimensions
      (View.vcat
         [
           box "zarrtop" (header model now);
           line
             ~color:(if model.editing then amber else blue)
             (sprintf "Filter: %s%s" model.filter
                (if model.editing then "_" else ""));
           box title body;
           line
             "Tab views  / filter  Enter done  Space pause  j/k scroll  q quit";
           (match model.error with
           | None -> View.none
           | Some error -> line ~color:red error);
         ])
  in
  let handler =
    let%arr inject = inject and scroll = less_keybindings_handler in
    fun event ->
      let fallback =
        let%map.Effect _ = scroll event in
        ()
      in
      inject (Key (event, fallback))
  in
  (~view, ~handler)
