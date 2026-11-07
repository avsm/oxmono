(*---------------------------------------------------------------------------
   Copyright (c) 2026 Anil Madhavapeddy. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type parts = Whole | Concatenated of string list | Gguf_shards of string list

type t = {
  name : string;
  aliases : string list;
  repo : string;
  files : string list;
  parts : parts;
  descr : string;
  deprecated : bool;
}

let deepseek_repo = "antirez/deepseek-v4-gguf"
let deepseek41_repo = "antirez/deepseek-v4.1-flash-gguf"
let glm_repo = "antirez/glm-5.3-flash-gguf"
let qwen_repo = "antirez/qwen3.8-flash-next-gguf"

(* Qwen's vision encoder is published apart from the language weights, unlike
   the other two, whose repositories carry both. *)
let qwen_vision_repo = "ggml-org/Qwen3.8-Flash-Next-GGUF"

(* The V4.1 Flash 1017 MXFP4 build is published as nine standard GGUF split
   files, which are merged back into the one page-aligned file it was built
   as. The sizes and digests are those upstream verified the merge against. *)
let ds41_mxfp4 = "DeepSeek-V4.1-Flash-1017-MXFP4-experts"
let ds41_mxfp4_file = ds41_mxfp4 ^ ".gguf"

let ds41_mxfp4_shards =
  List.init 9 (fun i ->
      Printf.sprintf "%s-%05d-of-00009.gguf" ds41_mxfp4 (i + 1))

(* The published length and SHA-256 of the files that are verified after they
   are downloaded or merged. *)
let checksums =
  ( ds41_mxfp4_file,
    ( 501609136128,
      "79608a69cd5bb919d8140c37105edc3a33419692d4e6caf4e68c901e4b359141" ) )
  :: List.map2
       (fun shard sum -> (shard, sum))
       ds41_mxfp4_shards
       [
         ( 49353784544,
           "636918b09de6253bf41af29f06aac3ceeda220e46379d8a2058ef92c4e433ed8" );
         ( 49412635712,
           "b26d3fc1e6ae1be6c52665542f3c1fee6a33791478f26e6cd90b5a75940a9f96" );
         ( 49582960352,
           "20c7fe7adb0578336d8a5e5f9eed521aeb88e8c119ba46e0916cdaab28931c80" );
         ( 49418206432,
           "67592ba15870c4292a615666adcc2736b53cc345b2cccb1c36710a0a6caf15b4" );
         ( 49412829632,
           "ecd04e0aa71bfab8cfe79ea7fef7f35b6be0b6772455139e2081d6ce1dcc18ed" );
         ( 49257603776,
           "88214e0cc80fbae578b80c114527e6ce725199684e0b6849475c7d00b3d0f54c" );
         ( 2406482112,
           "38939faf3e4258f3ae333f0fa1954ce89515b75ceee90d37bf4ebe5fafe285e6" );
         ( 101377628544,
           "a9d1634da2ac57459a64b10b69ddedfadda7dcde0e71ca6e55bea7c293c4a9f0" );
         ( 101380404256,
           "b9832b355288c005da6ef444d02424464dd27f6ebdefeadbec2244264eb65285" );
       ]

(* Targets are named for the build they carry. The short names are aliases on
   the newest build, so that asking for "q4" gets the current one, and an
   older build stays reachable under its own version. *)
let all =
  [
    {
      name = "ds41-mxfp4-1017";
      aliases = [ "ds41-mxfp4"; "v41-mxfp4"; "mxfp4"; "ds41f-mxfp4" ];
      repo = deepseek41_repo;
      files = [ ds41_mxfp4_file ];
      parts = Gguf_shards ds41_mxfp4_shards;
      descr =
        "V4.1 Flash 1017 MXFP4 experts, 467 GB on disk, 278 GB resident. For \
         512 GB.";
      deprecated = false;
    };
    {
      name = "ds41-q2-1017";
      aliases = [ "ds41-q2"; "v41-q2"; "ds41f-q2" ];
      repo = deepseek41_repo;
      files = [ "DeepSeek-V4.1-Flash-1017-IQ2_XXS-Q2_K-imatrix.gguf" ];
      parts = Whole;
      descr =
        "V4.1 Flash 1017 2-bit, 341 GB on disk, 152 GB resident. For 256 GB.";
      deprecated = false;
    };
    (* DeepSeek V4.1 Flash is a model of its own rather than a Flash rebuild,
       and its Engram tables stay on disk. The Q4 is published in two parts
       that are joined into one file. *)
    {
      name = "ds41-q4";
      aliases = [ "v41-q4" ];
      repo = deepseek41_repo;
      files = [ "DeepSeek-V4.1-Flash-Q4.gguf" ];
      parts =
        Concatenated
          [
            "DeepSeek-V4.1-Flash-Q4.gguf.part1";
            "DeepSeek-V4.1-Flash-Q4.gguf.part2";
          ];
      descr = "V4.1 Flash 4-bit, 483 GB on disk, 294 GB resident. For 512 GB.";
      deprecated = false;
    };
    {
      name = "ds41-q2-original";
      aliases = [];
      repo = deepseek41_repo;
      files = [ "DeepSeek-V4.1-Flash-Q2.gguf" ];
      parts = Whole;
      descr =
        "V4.1 Flash original 2-bit, 341 GB on disk, 152 GB resident. For 256 \
         GB.";
      deprecated = true;
    };
    {
      name = "q2-imatrix-0731";
      aliases = [ "q2"; "q2-imatrix" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix-0731.gguf";
        ];
      parts = Whole;
      descr = "2-bit 0731 experts, 81 GB. For 96 to 128 GB.";
      deprecated = false;
    };
    {
      name = "q2-q4-imatrix-0731";
      aliases = [ "q2q4"; "q2-q4-imatrix" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Layers37-42Q4KExperts-OtherExpertLayersIQ2XXSGateUp-Q2KDown-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix-fixed-0731.gguf";
        ];
      parts = Whole;
      descr = "Mixed 0731 quant, 98 GB. Best for 128 GB.";
      deprecated = false;
    };
    {
      name = "q4-imatrix-0731";
      aliases = [ "q4"; "q4-imatrix"; "0731" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Q4KExperts-F16HC-F16Compressor-F16Indexer-Q8Attn-Q8Shared-Q8Out-chat-v2-imatrix-0731.gguf";
        ];
      parts = Whole;
      descr = "4-bit 0731 experts, 153 GB. Best for 256 GB or more.";
      deprecated = false;
    };
    (* The first Flash release, which 0731 retrained. Same architecture and
       sizes, older weights. *)
    {
      name = "q2-imatrix-preview";
      aliases = [ "q2-preview" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix.gguf";
        ];
      parts = Whole;
      descr = "2-bit preview experts, 81 GB. Use q2 instead.";
      deprecated = true;
    };
    {
      name = "q2-q4-imatrix-preview";
      aliases = [ "q2q4-preview" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Layers37-42Q4KExperts-OtherExpertLayersIQ2XXSGateUp-Q2KDown-AProjQ8-SExpQ8-OutQ8-chat-v2-imatrix-fixed.gguf";
        ];
      parts = Whole;
      descr = "Mixed preview quant, 98 GB. Use q2q4 instead.";
      deprecated = true;
    };
    {
      name = "q4-imatrix-preview";
      aliases = [ "q4-preview" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Q4KExperts-F16HC-F16Compressor-F16Indexer-Q8Attn-Q8Shared-Q8Out-chat-v2-imatrix.gguf";
        ];
      parts = Whole;
      descr = "4-bit preview experts, 153 GB. Use q4 instead.";
      deprecated = true;
    };
    (* The 0813 PRO rebuild has only a 2-bit quant so far, so the Q4 halves
       below are still the original build and stay current. *)
    {
      name = "pro-q2-imatrix-0813";
      aliases = [ "pro-q2" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Pro-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-Instruct-imatrix-0813.gguf";
        ];
      parts = Whole;
      descr = "PRO 2-bit 0813 experts, 433 GB. For 512 GB.";
      deprecated = false;
    };
    {
      name = "pro-q2-imatrix";
      aliases = [];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Pro-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8-Instruct-imatrix.gguf";
        ];
      parts = Whole;
      descr = "PRO 2-bit, 430 GB. Use pro-q2 instead.";
      deprecated = true;
    };
    {
      name = "pro-q4-layers00-30";
      aliases = [ "pro-q4-a" ];
      repo = deepseek_repo;
      files = [ "DeepSeek-V4-Pro-Q4K-Layers00-30.gguf" ];
      parts = Whole;
      descr = "PRO Q4 layers 0 to 30, 426 GB. Coordinator half.";
      deprecated = false;
    };
    {
      name = "pro-q4-layers31-output";
      aliases = [ "pro-q4-b" ];
      repo = deepseek_repo;
      files = [ "DeepSeek-V4-Pro-Q4K-Layers-31-output.gguf" ];
      parts = Whole;
      descr = "PRO Q4 layers 31 to output, 412 GB. Worker half.";
      deprecated = false;
    };
    {
      name = "pro-q4-split";
      aliases = [ "pro-q4" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Pro-Q4K-Layers00-30.gguf";
          "DeepSeek-V4-Pro-Q4K-Layers-31-output.gguf";
        ];
      parts = Whole;
      descr = "Both PRO Q4 halves, 838 GB in total.";
      deprecated = false;
    };
    (* GLM 5.3 Flash, a different model the same engine runs. Its quants are
       published in a repository of their own. *)
    {
      name = "glm53-q2";
      aliases = [ "glm-q2" ];
      repo = glm_repo;
      files = [ "GLM-5.3-Flash-Q2.gguf" ];
      parts = Whole;
      descr = "GLM 5.3 Flash 2-bit, 90 GB. For 96 to 128 GB.";
      deprecated = false;
    };
    {
      name = "glm53-q4";
      aliases = [ "glm-q4" ];
      repo = glm_repo;
      files = [ "GLM-5.3-Flash-Q4_K.gguf" ];
      parts = Whole;
      descr = "GLM 5.3 Flash 4-bit, 178 GB. For 256 GB or more.";
      deprecated = false;
    };
    (* Qwen3.8 Flash Next, another model the engine runs. Its n-gram tables
       stay on disk, so far less of each file is resident than its size. *)
    {
      name = "qwen38-q4";
      aliases = [ "qwen-q4" ];
      repo = qwen_repo;
      files = [ "Qwen3.8-Flash-Next-Q4.gguf" ];
      parts = Whole;
      descr = "Qwen3.8 Flash Next 4-bit, 165 GB on disk, 70 GB resident.";
      deprecated = false;
    };
    {
      name = "qwen38-q2";
      aliases = [ "qwen-q2" ];
      repo = qwen_repo;
      files = [ "Qwen3.8-Flash-Next-Q2.gguf" ];
      parts = Whole;
      descr = "Qwen3.8 Flash Next 2-bit, 137 GB on disk, 42 GB resident.";
      deprecated = false;
    };
    (* Vision sidecars, loaded beside the matching language model with
       --vision. Small enough to fetch alongside the model they match, so each
       is its own target rather than bundled with one of the quants above. *)
    {
      name = "ds41-vision";
      aliases = [ "v41-vision" ];
      repo = deepseek41_repo;
      files = [ "DeepSeek-V4.1-Flash-Vision.gguf" ];
      parts = Whole;
      descr = "Vision sidecar for DeepSeek V4.1 Flash, 1 GB.";
      deprecated = false;
    };
    {
      name = "glm53-vision";
      aliases = [ "glm-vision" ];
      repo = glm_repo;
      files = [ "GLM-5.3-Flash-Vision-Encoder.gguf" ];
      parts = Whole;
      descr = "Vision sidecar for GLM 5.3 Flash, 1 GB.";
      deprecated = false;
    };
    {
      name = "qwen38-vision";
      aliases = [ "qwen-vision" ];
      repo = qwen_vision_repo;
      files = [ "mmproj-Qwen3.8-Flash-Next-Q8_0.gguf" ];
      parts = Whole;
      descr = "Vision sidecar for Qwen3.8 Flash Next, 0.6 GB.";
      deprecated = false;
    };
    (* DeepSeek V4 Flash Vision Experimental: its own checkpoint, frozen before
       the 0731 retrain, rather than the regular Flash weights with a sidecar
       bolted on. All three quants share one vision-encoder sidecar. *)
    {
      name = "ds4f-vision-q2";
      aliases = [ "vision-q2" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Vision-Exp-IQ2XXS-w2Q2K-AProjQ8-SExpQ8-OutQ8.gguf";
        ];
      parts = Whole;
      descr = "Vision Experimental 2-bit, 81 GB. For 96 to 128 GB.";
      deprecated = false;
    };
    {
      name = "ds4f-vision-q2-q4";
      aliases = [ "vision-q2q4" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Vision-Exp-Layers37-42Q4KExperts-OtherExpertLayersIQ2XXSGateUp-Q2KDown-AProjQ8-SExpQ8-OutQ8.gguf";
        ];
      parts = Whole;
      descr = "Vision Experimental mixed quant, 91 GB. Best for 128 GB.";
      deprecated = false;
    };
    {
      name = "ds4f-vision-mxfp4";
      aliases = [ "vision-mxfp4" ];
      repo = deepseek_repo;
      files =
        [
          "DeepSeek-V4-Flash-Vision-Exp-MXFP4Experts-F16HC-F16Compressor-F16Indexer-Q8Attn-Q8Shared-Q8Out.gguf";
        ];
      parts = Whole;
      descr = "Vision Experimental MXFP4, 145 GB. No imatrix.";
      deprecated = false;
    };
    {
      name = "ds4f-vision-encoder";
      aliases = [ "vision-exp-encoder" ];
      repo = deepseek_repo;
      files = [ "DeepSeek-V4-Flash-Vision-Encoder.gguf" ];
      parts = Whole;
      descr = "Vision sidecar shared by every Vision Experimental quant, 1 GB.";
      deprecated = false;
    };
  ]

let find s = List.find_opt (fun m -> m.name = s || List.mem s m.aliases) all

(* Which target a file on disk belongs to, matched on its name. A model
   obtained some other way belongs to none. *)
let of_path p =
  let base = Filename.basename p in
  List.find_opt (fun m -> List.mem base m.files) all

(* xdge already scopes its data directory to the application name, so this
   only renders it as a plain path for the helpers below. *)
let dir xdg = Eio.Path.native_exn (Xdge.data_dir xdg)

let present ~dir m =
  List.for_all (fun f -> Sys.file_exists (Filename.concat dir f)) m.files

(* Every message that sends a person to fetch a model names the command that
   ships with this library, since it is the one sure to be installed. *)
let download_command =
  Printf.sprintf "ds4-agent-%s download"
    (match Ds4.V4.backend with
    | `Metal -> "metal"
    | `Cuda -> "cuda"
    | `Cpu -> "cpu")

(* Preference order when no model is named, best first. Whichever of these is
   downloaded wins, so a machine that can hold only the 2-bit quant still gets
   a sensible default. The fallback in [first_gguf] picks by filename rather
   than by quality. *)
let preferred =
  [
    "ds41-mxfp4-1017";
    "ds41-q2-1017";
    "ds41-q4";
    "q4-imatrix-0731";
    "ds41-q2-original";
    "q2-q4-imatrix-0731";
    "q2-imatrix-0731";
  ]

let preferred_present ~dir =
  List.find_map
    (fun name ->
      match find name with
      | Some ({ files = [ f ]; _ } as m) when present ~dir m ->
          Some (Filename.concat dir f)
      | _ -> None)
    preferred

(* The first GGUF in [d] in alphabetical order, if there is one. *)
let first_gguf d =
  match Sys.readdir d with
  | exception Sys_error _ -> None
  | entries -> (
      Array.to_list entries
      |> List.filter (fun f -> Filename.check_suffix f ".gguf")
      |> List.sort String.compare
      |> function
      | [] -> None
      | f :: _ -> Some (Filename.concat d f))

let resolve ?(env = Sys.getenv_opt) ~dir:data override =
  match override with
  | Some s -> (
      match find s with
      | Some { files = [ f ]; _ } ->
          let path = Filename.concat data f in
          if Sys.file_exists path then Ok path
          else
            Error
              (Printf.sprintf "model '%s' is not downloaded. Run '%s %s'." s
                 download_command s)
      | Some { name; files; _ } ->
          Error
            (Printf.sprintf
               "model '%s' is split across %d files (%s) and cannot be loaded \
                on its own"
               name (List.length files) (String.concat ", " files))
      | None -> Ok s (* An unknown name is taken as a path. *))
  | None -> (
      match env "DS4_MODEL" with
      | Some m -> Ok m
      | None -> (
          match preferred_present ~dir:data with
          | Some m -> Ok m
          | None -> (
              match first_gguf data with
              | Some m -> Ok m
              | None ->
                  Error
                    (Printf.sprintf
                       "no model found. Pass --model, set DS4_MODEL, run '%s \
                        <target>', or put a .gguf in %s."
                       download_command data))))

let joining out = out ^ ".joining"
let file_size path = (Unix.LargeFile.stat path).Unix.LargeFile.st_size

(* Join the parts [first] and [rest] under [dir] into [out]. The first part
   becomes the file rather than being copied, so the join needs room only for
   the later parts. Its length is recorded before it is moved, so a join that
   is interrupted truncates back to it and appends again rather than fetching
   the first part a second time. The later parts are removed only once the
   joined length is right. *)
let join_parts ~dir ~out first rest =
  let path f = Filename.concat dir f in
  let joining = joining out and boundary_file = out ^ ".joining-length" in
  if not (Sys.file_exists joining) then (
    Out_channel.with_open_text boundary_file (fun oc ->
        output_string oc (Int64.to_string (file_size (path first))));
    Sys.rename (path first) joining);
  let boundary =
    In_channel.with_open_text boundary_file In_channel.input_all
    |> String.trim |> Int64.of_string
  in
  Unix.LargeFile.truncate joining boundary;
  let buf = Bytes.create (16 * 1024 * 1024) in
  Out_channel.with_open_gen [ Open_wronly; Open_append; Open_binary ]
    0o644 joining (fun oc ->
      List.iter
        (fun part ->
          In_channel.with_open_bin (path part) (fun ic ->
              let rec copy () =
                match In_channel.input ic buf 0 (Bytes.length buf) with
                | 0 -> ()
                | n ->
                    Out_channel.output oc buf 0 n;
                    copy ()
              in
              copy ()))
        rest);
  let expected =
    List.fold_left
      (fun acc part -> Int64.add acc (file_size (path part)))
      boundary rest
  in
  let got = file_size joining in
  if got <> expected then
    failwith
      (Printf.sprintf
         "joining %s produced %Ld bytes where the parts hold %Ld. The parts \
          are kept, so running the download again retries the join."
         out got expected);
  Sys.rename joining out;
  List.iter (fun part -> Sys.remove (path part)) rest;
  Sys.remove boundary_file

let gib = 1024 * 1024 * 1024

(* The bytes free on the filesystem holding [dir], read from POSIX [df]. None
   when its output is not understood, which is reported rather than treated
   as room. *)
let free_bytes ~proc dir =
  match Eioext.Process.capture ~process_mgr:proc [ "df"; "-Pk"; dir ] with
  | exception Eio.Exn.Io (_, _) -> None
  | { Eioext.Process.status = `Exited 0; stdout; _ } -> (
      match List.rev (String.split_on_char '\n' (String.trim stdout)) with
      | last :: _ -> (
          match
            String.split_on_char ' ' last |> List.filter (fun f -> f <> "")
          with
          | _ :: _ :: _ :: available :: _ -> (
              match int_of_string_opt available with
              | Some kib -> Some (kib * 1024)
              | None -> None)
          | _ -> None)
      | [] -> None)
  | _ -> None

let expected file =
  match List.assoc_opt file checksums with
  | Some sum -> sum
  | None ->
      invalid_arg (Printf.sprintf "Model: no published checksum for %s" file)

(* Refuse to start a download of hundreds of gigabytes that cannot finish.
   [files] are the files still to be written under [dir], each with its final
   size, of which whatever is already there counts as done. *)
let check_space ~proc ~dir files =
  let needed =
    List.fold_left
      (fun acc (file, size) ->
        let path = Filename.concat dir file in
        let have =
          if Sys.file_exists path then Int64.to_int (file_size path) else 0
        in
        acc + max 0 (size - have))
      gib files
  in
  match free_bytes ~proc dir with
  | Some free when free < needed ->
      failwith
        (Printf.sprintf
           "not enough disk space in %s: %d GiB are needed for the shards and \
            the merged model and %d GiB are free. Nothing was downloaded."
           dir (needed / gib) (free / gib))
  | Some _ -> ()
  | None ->
      Printf.eprintf
        "Could not read the free space in %s. About %d GiB are needed.\n%!" dir
        (needed / gib)

let download ~fs ~proc ~dir ?token m =
  Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 Eio.Path.(fs / dir);
  let hf_args file =
    [ "download"; m.repo; file; "--repo-type"; "model"; "--local-dir"; dir ]
  in
  let fetch file =
    Printf.eprintf
      "Downloading %s\n  from https://huggingface.co/%s\n  into %s\n%!" file
      m.repo dir;
    Hf.run ~process_mgr:proc ?token (hf_args file)
  in
  match (m.files, m.parts) with
  | _, Whole -> List.iter fetch m.files
  | [ file ], Concatenated (first :: rest) ->
      let out = Filename.concat dir file in
      if Sys.file_exists out then Printf.eprintf "%s is already present\n%!" out
      else (
        (* A join under way has already taken the first part. *)
        if not (Sys.file_exists (joining out)) then fetch first;
        List.iter fetch rest;
        Printf.eprintf "Joining %d parts into %s\n%!" (1 + List.length rest) out;
        join_parts ~dir ~out first rest)
  | [ file ], Gguf_shards shards ->
      let out = Filename.concat dir file in
      if Sys.file_exists out then Printf.eprintf "%s is already present\n%!" out
      else
        let bytes, sha256 = expected file in
        check_space ~proc ~dir
          ((file ^ ".assembling", bytes)
          :: List.map (fun shard -> (shard, fst (expected shard))) shards);
        (* A shard already of its published length is left alone. The merged
           file's own checksum covers every byte of it. *)
        List.iter
          (fun shard ->
            let path = Filename.concat dir shard in
            let expected_bytes, sha256 = expected shard in
            if
              Sys.file_exists path
              && Int64.to_int (file_size path) = expected_bytes
            then Printf.eprintf "%s is already present\n%!" path
            else (
              fetch shard;
              Gguf_shards.verify ~path ~expected_bytes ~sha256))
          shards;
        Printf.eprintf "Merging %d shards into %s\n%!" (List.length shards) out;
        let reported = ref 0 in
        let progress ~written ~total =
          if written / (8 * gib) > !reported || written = total then (
            reported := written / (8 * gib);
            Printf.eprintf "  %d of %d GiB\n%!" (written / gib) (total / gib))
        in
        Gguf_shards.merge ~progress ~out ~expected_bytes:bytes ~sha256
          (List.map (Filename.concat dir) shards);
        List.iter (fun shard -> Sys.remove (Filename.concat dir shard)) shards
  | files, (Concatenated _ | Gguf_shards _) ->
      invalid_arg
        (Printf.sprintf "Model %s: parts must join into one file, not %d" m.name
           (List.length files))

let part_files = function
  | Whole -> []
  | Concatenated parts | Gguf_shards parts -> parts

let others ~dir =
  let known =
    List.concat_map (fun m -> m.files @ part_files m.parts) all
    |> List.sort_uniq String.compare
  in
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | entries ->
      Array.to_list entries
      |> List.filter (fun f ->
             Filename.check_suffix f ".gguf" && not (List.mem f known))
      |> List.sort String.compare

open Cmdliner

let arg =
  Arg.(
    value
    & opt (some string) None
    & info [ "m"; "model" ] ~docv:"MODEL"
        ~doc:
          "The model to use, given as a download target, an alias such as \
           'mxfp4', or the path of a .gguf file. When omitted, DS4_MODEL is \
           used, then downloaded DeepSeek V4.1 Flash 1017, older preferred \
           models, then the first .gguf in the data directory.")

let target_arg =
  let enum_assoc =
    List.concat_map
      (fun m -> (m.name, m) :: List.map (fun a -> (a, m)) m.aliases)
      all
  in
  let doc =
    "The model to download, given as a target name or an alias. The $(b,list) \
     subcommand shows them all."
  in
  Arg.(
    required & pos 0 (some (enum enum_assoc)) None & info [] ~docv:"TARGET" ~doc)
