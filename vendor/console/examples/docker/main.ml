(*---------------------------------------------------------------------------
  Copyright (c) 2026 Thomas Gazagnaire. All rights reserved.
  SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A multi-stage image build, drawn as the tty progress of docker buildx
   build: a header that counts the steps, one row per step with its time
   against the right edge, the layers of a step under it, the last lines of a
   running step faint under those, and the rows that ended in one colour. No
   step leaves the region until the build ends, and the last frame stays.
   Pipe it through [cat] to see the lines a CI log gets, those of BuildKit's
   plain progress.

   The Dockerfile it builds:
   {v
   FROM golang:1.22-alpine AS build
   WORKDIR /src
   COPY go.mod go.sum ./
   RUN go mod download
   COPY . .
   RUN go build -v -o /out/hello ./cmd/hello

   FROM alpine:3.20
   RUN apk add --no-cache ca-certificates
   COPY --from=build /out/hello /usr/local/bin/hello
   v} *)

open Console

let tail_lines = 6

(* The labels of the steps the cache answers. *)
let cached = Hashtbl.create 8
let is_cached task = Hashtbl.mem cached (Task.label task)

let cached_step root name =
  Hashtbl.replace cached name ();
  Task.run root name ignore

(* The steps the build knows of, which grows as the Dockerfile is read. *)
let known = ref 1

(* Sizes as docker prints them, in powers of 1000. *)
let size n =
  let n = Int64.to_float n in
  if n < 1e3 then Fmt.str "%.0fB" n
  else if n < 1e6 then Fmt.str "%.2fkB" (n /. 1e3)
  else if n < 1e9 then Fmt.str "%.2fMB" (n /. 1e6)
  else Fmt.str "%.2fGB" (n /. 1e9)

let colour task =
  match Task.state task with
  | `Running -> Style.none
  | `Succeeded -> Style.fg Color.blue
  | `Failed -> Style.fg Color.red
  | `Cancelled -> Style.fg Color.yellow

let prefix task =
  match Task.state task with
  | `Failed -> "ERROR "
  | `Cancelled -> "CANCELED "
  | _ when is_cached task -> "CACHED "
  | _ -> ""

(* [name], cut to leave room for [status] after it, and [right] against the
   right edge. *)
let line width ?(status = "") ?(right = "") style name =
  let cells s = View.width (View.text s) in
  let room = width - cells right in
  let name = View.truncate (room - cells status) (View.text name) in
  View.styled style
    (View.hcat ~gap:0
       [
         View.place ~width:(max 1 room) View.(name ++ text status);
         View.text right;
       ])

let timer task = Fmt.str " %.1fs" (Task.elapsed task)

let header width root =
  let steps = Task.children root in
  let ended =
    List.length (List.filter (fun s -> Task.state s <> `Running) steps)
  in
  let finished = Task.state root <> `Running in
  let text =
    Fmt.str "[+] Building %.1fs (%d/%d)%s" (Task.elapsed root) ended
      (max !known (List.length steps))
      (if finished then " FINISHED" else "")
  in
  line width ~right:"docker:default"
    (if finished then colour root else Style.none)
    text

let last n l = List.drop (max 0 (List.length l - n)) l

let rows width step =
  let row =
    line width ~right:(timer step) (colour step)
      (" => " ^ prefix step ^ Task.label step)
  in
  let action a =
    let status =
      match Task.progress a with
      | `Bytes (count, total) -> Fmt.str " %s / %s" (size count) (size total)
      | _ -> ""
    in
    line width ~status ~right:(timer a) (colour a) (" => => " ^ Task.label a)
  in
  let log l =
    View.styled Style.faint (View.truncate width (View.text (" => => # " ^ l)))
  in
  let tail =
    if Task.state step = `Running then
      List.map log (last tail_lines (Task.output step))
    else []
  in
  (row :: List.map action (Task.children step)) @ tail

(* Set by [plain], off a terminal: its lines told the build, so the last frame
   is empty, as BuildKit's plain progress ends with the last step. *)
let plain_lines = ref false

(* The header, then as many of the last rows as the room holds. *)
let view root =
  View.fit (fun room ->
      let width = Option.value (Room.width room) ~default:100 in
      if !plain_lines then View.empty
      else
        let rows = List.concat_map (rows width) (Task.children root) in
        let rows =
          match Room.height room with
          | Some h -> last (h - 1) rows
          | None -> rows
        in
        View.vcat (header width root :: rows))

(* BuildKit's plain progress: each step is a vertex [#n], and a line of a
   vertex follows a blank line and its name when another vertex printed last.
   Output is timed from the start of its step. *)
type vertex = { n : int; name : string; started : float }

let vertices = Hashtbl.create 16

(* The bytes an action counts, by event id. *)
let counts = Hashtbl.create 16
let current = ref 0

let say v fmt =
  Fmt.kstr
    (fun s ->
      let s =
        if !current = v.n then s else Fmt.str "\n#%d %s\n%s" v.n v.name s
      in
      current := v.n;
      Some (View.text s))
    ("#%d " ^^ fmt) v.n

let plain e =
  plain_lines := true;
  let module E = Task.Event in
  match (E.kind e, E.task e) with
  | `Start, Some (_, [ name ]) ->
      let v = { n = Hashtbl.length vertices + 1; name; started = E.time e } in
      Hashtbl.replace vertices name v;
      let blank = if !current = 0 then "" else "\n" in
      current := v.n;
      Some (View.text (Fmt.str "%s#%d %s" blank v.n name))
  | `Progress, Some (id, _) -> (
      match (E.unit e, E.count e, E.total e) with
      | Some `Bytes, Some c, Some t ->
          Hashtbl.replace counts id (c, t);
          None
      | _ -> None)
  | `Output, Some (_, [ step ]) ->
      let v = Hashtbl.find vertices step in
      say v "%.3f %s"
        (E.time e -. v.started)
        (Option.value (E.text e) ~default:"")
  | `End, Some (_, [ step ]) -> (
      let v = Hashtbl.find vertices step in
      match E.status e with
      | Some `Succeeded when Hashtbl.mem cached step -> say v "CACHED"
      | Some `Succeeded ->
          say v "DONE %.1fs" (Option.value (E.elapsed e) ~default:0.)
      | Some `Failed ->
          say v "ERROR: %s" (Option.value (E.message e) ~default:step)
      | Some `Cancelled -> say v "CANCELED"
      | _ -> None)
  | `End, Some (id, [ step; action ]) ->
      let v = Hashtbl.find vertices step in
      let bytes =
        match Hashtbl.find_opt counts id with
        | Some (c, t) -> Fmt.str " %s / %s" (size c) (size t)
        | None -> ""
      in
      let time =
        match E.elapsed e with
        | Some s when s >= 0.05 -> Fmt.str " %.1fs" s
        | _ -> ""
      in
      let status =
        match E.status e with
        | Some `Succeeded -> "done"
        | Some `Failed -> "ERROR"
        | _ -> "CANCELED"
      in
      say v "%s%s%s %s" action bytes time status
  | _ -> None

let pull clock step ~layer ~bytes =
  Task.run step layer (fun a ->
      for i = 1 to 12 do
        Eio.Time.Mono.sleep clock 0.1;
        Task.set_bytes a
          ~count:(Int64.div (Int64.mul bytes (Int64.of_int i)) 12L)
          ~total:bytes
      done);
  Task.run step ("extracting " ^ layer) (fun _ -> Eio.Time.Mono.sleep clock 0.3)

let output clock step pace lines =
  List.iter
    (fun l ->
      Eio.Time.Mono.sleep clock pace;
      Task.log step l)
    lines

let go_build =
  [
    "internal/goarch";
    "internal/abi";
    "internal/cpu";
    "internal/runtime/atomic";
    "runtime";
    "errors";
    "sync";
    "io";
    "os";
    "fmt";
    "github.com/spf13/pflag";
    "github.com/spf13/cobra";
    "example.com/hello/internal/greet";
    "example.com/hello/cmd/hello";
  ]

let apk_add =
  [
    "fetch \
     https://dl-cdn.alpinelinux.org/alpine/v3.20/main/x86_64/APKINDEX.tar.gz";
    "fetch \
     https://dl-cdn.alpinelinux.org/alpine/v3.20/community/x86_64/APKINDEX.tar.gz";
    "(1/1) Installing ca-certificates (20240705-r0)";
    "Executing busybox-1.36.1-r29.trigger";
    "Executing ca-certificates-20240705-r0.trigger";
    "OK: 8 MiB in 15 packages";
  ]

let alpine =
  "sha256:beefdbd8a1da6d2915566fde36db9db0b524eb737fc57cd1367effd16dc0d06d"

let golang =
  "sha256:1699c10032ca2582ec89a24a1312d986a3f094aed3d5c1147b19880afe40e052"

let layer =
  "sha256:43c4264eed91be63b206e17d93e75256a6097070ce643c5e8f0379998b44f170"

let build_stage clock root =
  Task.run root
    ("[build 1/6] FROM docker.io/library/golang:1.22-alpine@" ^ golang)
    (fun s ->
      Task.run s
        ("resolve docker.io/library/golang:1.22-alpine@" ^ golang)
        ignore);
  Task.run root "[internal] load build context" (fun s ->
      Task.run s "transferring context: 18.42kB" (fun _ ->
          Eio.Time.Mono.sleep clock 0.2));
  cached_step root "[build 2/6] WORKDIR /src";
  cached_step root "[build 3/6] COPY go.mod go.sum ./";
  cached_step root "[build 4/6] RUN go mod download";
  Task.run root "[build 5/6] COPY . ." (fun _ -> Eio.Time.Mono.sleep clock 0.1);
  Task.run root "[build 6/6] RUN go build -v -o /out/hello ./cmd/hello"
    (fun s ->
      output clock s 0.3 go_build;
      Eio.Time.Mono.sleep clock 0.4)

let runtime_stage clock root =
  Task.run root ("[stage-1 1/3] FROM docker.io/library/alpine:3.20@" ^ alpine)
    (fun s ->
      Task.run s ("resolve docker.io/library/alpine:3.20@" ^ alpine) ignore;
      pull clock s ~layer ~bytes:3_623_807L);
  Task.run root "[stage-1 2/3] RUN apk add --no-cache ca-certificates" (fun s ->
      output clock s 0.4 apk_add)

let build clock root =
  Task.run root "[internal] load build definition from Dockerfile" (fun s ->
      Task.run s "transferring dockerfile: 389B" ignore);
  known := 4;
  Eio.Fiber.all
    [
      (fun () ->
        Task.run root
          "[internal] load metadata for docker.io/library/alpine:3.20" (fun _ ->
            Eio.Time.Mono.sleep clock 1.1));
      (fun () ->
        Task.run root
          "[internal] load metadata for docker.io/library/golang:1.22-alpine"
          (fun _ -> Eio.Time.Mono.sleep clock 1.4));
    ];
  Task.run root "[internal] load .dockerignore" (fun s ->
      Task.run s "transferring context: 2B" ignore);
  known := 15;
  Eio.Fiber.both
    (fun () -> build_stage clock root)
    (fun () -> runtime_stage clock root);
  Task.run root
    "[stage-1 3/3] COPY --from=build /out/hello /usr/local/bin/hello" (fun _ ->
      Eio.Time.Mono.sleep clock 0.1);
  Task.run root "exporting to image" (fun s ->
      Task.run s "exporting layers" (fun _ -> Eio.Time.Mono.sleep clock 0.2);
      Task.run s
        "writing image \
         sha256:9a3b5c7d1e2f40618293a4b5c6d7e8f9012a3b4c5d6e7f8091a2b3c4d5e6f708"
        ignore;
      Task.run s "naming to docker.io/library/hello:latest" ignore)

let () =
  Eio_main.run @@ fun env ->
  let clock = Console_eio.clock (Eio.Stdenv.mono_clock env) in
  Console_eio.run ~clock ~label:"Building" ~keep_ended:true ~view ~plain
    (build clock)
