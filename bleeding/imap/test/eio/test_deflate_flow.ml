module D=Imap_eio_core.Deflate_flow
module Raw = struct
  type t={mutable input:string;mutable at:int;written:Buffer.t;
    mutable closes:int;mutable reads:int;fragment:int;mutable refill:unit -> string}
  let read_methods=[]
  let single_read t buffer =
    t.reads<-t.reads+1;
    if t.at=String.length t.input then (t.input<-t.refill ();t.at<-0);
    if t.at=String.length t.input then raise End_of_file;
    let n=min t.fragment (min (Cstruct.length buffer) (String.length t.input-t.at)) in
    Cstruct.blit_from_string t.input t.at buffer 0 n;
    t.at<-t.at+n;n
  let single_write t (buffers @ local) =
    let buffers=Cstruct.globalize_list buffers in
    List.iter (fun b -> Buffer.add_string t.written (Cstruct.to_string b)) buffers;
    Cstruct.lenv buffers
  let copy t ~src=Eio.Flow.Pi.simple_copy ~single_write t ~src
  let shutdown _ _=()
  let close t=t.closes<-t.closes+1
end
let handler=Eio.Resource.handler (
  Eio.Resource.H (Eio.Resource.Close,Raw.close) ::
  Eio.Resource.bindings (Eio.Flow.Pi.two_way (module Raw)))
let connection ?(fragment=65536) input =
  let raw={Raw.input;at=0;written=Buffer.create 32;closes=0;reads=0;fragment;
    refill=(fun () -> raise End_of_file)} in
  raw,D.create (Eio.Resource.T (raw,handler))
let read_exact flow length =
  let result=Buffer.create length and buffer=Cstruct.create 17 in
  while Buffer.length result<length do
    let n=D.read flow (Cstruct.sub buffer 0 (min 17 (length-Buffer.length result))) in
    Buffer.add_string result (Cstruct.to_string (Cstruct.sub buffer 0 n))
  done;
  Buffer.contents result

(* Python zlib.compressobj(wbits=-15), continuous Z_SYNC_FLUSH. The second
   block references the first block's dictionary; neither has BFINAL. *)
let first="\210\082\240\247\086\072\206\207\045\040\074\045\046\206\204\207\083\040\074\077\076\169\228\229\002\000\000\000\255\255"
let second="\210\194\042\174\144\152\158\152\153\199\203\005\000\000\000\255\255"
let test_external_vectors () =
  List.iter (fun fragment ->
    let raw,flow=connection ~fragment first in
    let a="* OK compression ready\r\n" and b="* OK compression ready again\r\n" in
    if read_exact flow (String.length a)<>a then failwith "raw zlib first block mismatch";
    (* No EOF or subsequent input was needed to release a short response. *)
    raw.refill<-(fun () -> second);
    if read_exact flow (String.length b)<>b then failwith "dictionary continuity lost";
    D.close flow;D.close flow;
    if raw.closes<>1 then failwith "raw flow closed more than once") [1;65536]

let encoded_sample () =
  let raw,flow=connection "" in
  D.write flow [Cstruct.of_string "A1 NOOP\r\n"];
  let first_length=Buffer.length raw.written in
  let body=String.concat "" (List.init 5000 (fun _ -> "A2 UID FETCH 1:* (UID FLAGS)\r\n")) in
  D.write flow [Cstruct.of_string body];
  D.close flow;
  let encoded=Buffer.contents raw.written in
  if String.length encoded>=String.length body/4 then failwith "DEFLATE did not compress repetition";
  encoded,first_length,"A1 NOOP\r\n" ^ body

let test_outbound () =
  let encoded,_,expected=encoded_sample () in
  let _,flow=connection ~fragment:31 encoded in
  if read_exact flow (String.length expected)<>expected then failwith "continuous output did not round-trip";
  D.close flow

let test_incompressible () =
  let random=Random.State.make [|401;932|] in
  let bytes=String.init 200_000 (fun _ -> Char.chr (Random.State.int random 256)) in
  let raw,flow=connection "" in
  D.write flow [Cstruct.of_string bytes];
  let encoded=Buffer.contents raw.written in
  let _,reader=connection encoded in
  if read_exact reader (String.length bytes)<>bytes then
    failwith "encoder output refill corrupted incompressible bytes";
  D.close flow;D.close reader

let test_invalid_and_end () =
  List.iter (fun data ->
    let raw,flow=connection data in
    (match D.read flow (Cstruct.create 100) with
     | _ -> failwith "invalid/final DEFLATE stream accepted"
     | exception Eio.Io (D.Deflate message,_) ->
         if data="\007" && not (String.starts_with ~prefix:"invalid stream: "
             message && String.length message>16) then
           failwith "decompress diagnostic was dropped");
    D.close flow;
    if raw.closes<>1 then failwith "codec failure close not idempotent")
    ["\007";"\003\000"]

let test_no_output_budget () =
  let raw,flow=connection "" in
  let empties=String.concat "" (List.init 13107 (fun _ -> "\000\000\000\255\255")) in
  raw.refill<-(fun () -> empties);
  (match D.read flow (Cstruct.create 1) with
   | _ -> failwith "empty blocks bypassed no-output budget"
   | exception Eio.Io (D.Deflate message,_) when message=
       "input budget exceeded without decoded output" -> ());
  if raw.reads>258 || raw.closes<>1 then failwith "no-output work was not bounded"

let test_cancel () =
  let raw,flow=connection "" in
  raw.refill<-(fun () -> "\000\000\000\255\255");
  Eio.Fiber.first (fun () -> ignore (D.read flow (Cstruct.create 1)))
    (fun () -> Eio.Fiber.yield ());
  if raw.closes<>1 then failwith "cancellation did not close codec"

let () = Eio_mock.Backend.run (fun () ->
  if Array.length Sys.argv>1 && Sys.argv.(1)="--emit" then (
    let encoded,first_length,_=encoded_sample () in
    Printf.eprintf "%d\n%!" first_length;
    output_string stdout encoded)
  else (
    test_external_vectors ();test_outbound ();test_incompressible ();test_invalid_and_end ();
    test_no_output_budget ();test_cancel ()))
