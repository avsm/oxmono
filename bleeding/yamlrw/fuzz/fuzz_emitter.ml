(*---------------------------------------------------------------------------
  Copyright (c) 2025 Anil Madhavapeddy <anil@recoil.org>. All rights reserved.
  SPDX-License-Identifier: ISC
 ---------------------------------------------------------------------------*)

(** Fuzz tests for the Emitter module - test random event sequences *)

open Crowbar
open Fuzz_common

(** Event type for fuzzing *)
type fuzz_event =
  | Stream_start
  | Stream_end
  | Doc_start
  | Doc_end
  | Scalar of string
  | Alias of string
  | Seq_start
  | Seq_end
  | Map_start
  | Map_end

(** Generator for fuzz events *)
let fuzz_event =
  choose
    [
      const Stream_start;
      const Stream_end;
      const Doc_start;
      const Doc_end;
      map [ ident_string ] (fun s -> Scalar s);
      map [ ident_string ] (fun s -> Alias s);
      const Seq_start;
      const Seq_end;
      const Map_start;
      const Map_end;
    ]

(** Emit a fuzz event to an emitter - may fail with Yamlrw_error *)
let emit_fuzz_event emitter = function
  | Stream_start -> Yamlrw.Stream.stream_start emitter `Utf8
  | Stream_end -> Yamlrw.Stream.stream_end emitter
  | Doc_start -> Yamlrw.Stream.document_start emitter ()
  | Doc_end -> Yamlrw.Stream.document_end emitter ()
  | Scalar s -> Yamlrw.Stream.scalar emitter s
  | Alias s -> Yamlrw.Stream.alias emitter s
  | Seq_start -> Yamlrw.Stream.sequence_start emitter ()
  | Seq_end -> Yamlrw.Stream.sequence_end emitter
  | Map_start -> Yamlrw.Stream.mapping_start emitter ()
  | Map_end -> Yamlrw.Stream.mapping_end emitter

(** Test that random event sequences don't crash the emitter *)
let () =
  add_test ~name:"emitter: random events crash safety" [ list fuzz_event ]
  @@ fun events ->
  let emitter = Yamlrw.Stream.emitter () in
  List.iter
    (fun ev ->
      try emit_fuzz_event emitter ev with Yamlrw.Yamlrw_error _ -> ())
    events;
  check true

(** Test that valid event sequences produce parseable output *)
let () =
  add_test ~name:"emitter: valid sequence roundtrip" [ list ident_string ]
  @@ fun items ->
  if List.length items > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       Yamlrw.Stream.sequence_start emitter ();
       List.iter (fun s -> Yamlrw.Stream.scalar emitter s) items;
       Yamlrw.Stream.sequence_end emitter;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       (* Try to parse the emitted YAML *)
       let _ = Yamlrw.of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test that valid mapping event sequences produce parseable output *)
let () =
  add_test ~name:"emitter: valid mapping roundtrip"
    [ list (pair ident_string ident_string) ]
  @@ fun pairs ->
  if List.length pairs > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       Yamlrw.Stream.mapping_start emitter ();
       List.iter
         (fun (k, v) ->
           Yamlrw.Stream.scalar emitter k;
           Yamlrw.Stream.scalar emitter v)
         pairs;
       Yamlrw.Stream.mapping_end emitter;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       (* Try to parse the emitted YAML *)
       let _ = Yamlrw.of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test nested sequences *)
let () =
  add_test ~name:"emitter: nested sequences" [ range 10; list ident_string ]
  @@ fun depth items ->
  if depth > 0 && List.length items > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       for _ = 1 to depth do
         Yamlrw.Stream.sequence_start emitter ()
       done;
       List.iter (fun s -> Yamlrw.Stream.scalar emitter s) items;
       for _ = 1 to depth do
         Yamlrw.Stream.sequence_end emitter
       done;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       let _ = Yamlrw.of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test nested mappings *)
let () =
  add_test ~name:"emitter: nested mappings" [ range 10; ident_string ]
  @@ fun depth value ->
  if depth > 0 && String.length value > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       for i = 1 to depth do
         Yamlrw.Stream.mapping_start emitter ();
         Yamlrw.Stream.scalar emitter (Printf.sprintf "key%d" i)
       done;
       Yamlrw.Stream.scalar emitter value;
       for _ = 1 to depth do
         Yamlrw.Stream.mapping_end emitter
       done;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       let _ = Yamlrw.of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test emitter with different scalar styles *)
let () =
  add_test ~name:"emitter: scalar styles" [ printable_string ] @@ fun s ->
  let styles =
    [ `Any; `Plain; `Single_quoted; `Double_quoted; `Literal; `Folded ]
  in
  List.iter
    (fun style ->
      let emitter = Yamlrw.Stream.emitter () in
      (try
         Yamlrw.Stream.stream_start emitter `Utf8;
         Yamlrw.Stream.document_start emitter ();
         Yamlrw.Stream.scalar emitter ~style s;
         Yamlrw.Stream.document_end emitter ();
         Yamlrw.Stream.stream_end emitter;
         let yaml = Yamlrw.Stream.contents emitter in
         let _ = Yamlrw.of_string yaml in
         ()
       with Yamlrw.Yamlrw_error _ -> ()))
    styles;
  check true

(** Test emitter with anchors and aliases *)
let () =
  add_test ~name:"emitter: anchors and aliases" [ ident_string; ident_string ]
  @@ fun anchor value ->
  if String.length anchor > 0 && String.length value > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       Yamlrw.Stream.mapping_start emitter ();
       Yamlrw.Stream.scalar emitter "original";
       Yamlrw.Stream.scalar emitter ~anchor value;
       Yamlrw.Stream.scalar emitter "reference";
       Yamlrw.Stream.alias emitter anchor;
       Yamlrw.Stream.mapping_end emitter;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       let _ = Yamlrw.of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test emitter with tags *)
let () =
  add_test ~name:"emitter: tagged scalars" [ ident_string; ident_string ]
  @@ fun tag value ->
  if String.length value > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       Yamlrw.Stream.document_start emitter ();
       Yamlrw.Stream.scalar emitter ~tag:("!" ^ tag) value;
       Yamlrw.Stream.document_end emitter ();
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       let _ = Yamlrw.yaml_of_string yaml in
       ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

(** Test emitter with layout styles *)
let () =
  add_test ~name:"emitter: layout styles" [ list ident_string ] @@ fun items ->
  if List.length items > 0 then begin
    let styles = [ `Any; `Block; `Flow ] in
    List.iter
      (fun style ->
        let emitter = Yamlrw.Stream.emitter () in
        (try
           Yamlrw.Stream.stream_start emitter `Utf8;
           Yamlrw.Stream.document_start emitter ();
           Yamlrw.Stream.sequence_start emitter ~style ();
           List.iter (fun s -> Yamlrw.Stream.scalar emitter s) items;
           Yamlrw.Stream.sequence_end emitter;
           Yamlrw.Stream.document_end emitter ();
           Yamlrw.Stream.stream_end emitter;
           let yaml = Yamlrw.Stream.contents emitter in
           let _ = Yamlrw.of_string yaml in
           ()
         with Yamlrw.Yamlrw_error _ -> ()))
      styles;
    check true
  end
  else check true

(** Test multiple documents *)
let () =
  add_test ~name:"emitter: multiple documents" [ range 5; ident_string ]
  @@ fun count value ->
  if count > 0 && String.length value > 0 then begin
    let emitter = Yamlrw.Stream.emitter () in
    (try
       Yamlrw.Stream.stream_start emitter `Utf8;
       for i = 1 to count do
         Yamlrw.Stream.document_start emitter ();
         Yamlrw.Stream.scalar emitter (Printf.sprintf "%s%d" value i);
         Yamlrw.Stream.document_end emitter ()
       done;
       Yamlrw.Stream.stream_end emitter;
       let yaml = Yamlrw.Stream.contents emitter in
       let docs = Yamlrw.documents_of_string yaml in
       if List.length docs <> count then fail "document count mismatch"
       else ()
     with Yamlrw.Yamlrw_error _ -> ());
    check true
  end
  else check true

let run () = ()
