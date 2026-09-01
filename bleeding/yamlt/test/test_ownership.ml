type node = { count : int ref; children : node list }

let codec =
  Jsont.Portable_lazy.from_fun_fixed (fun self ->
    let count = Jsont.map ~dec:ref ~enc:( ! ) Jsont.int in
    Jsont.Object.map (fun count children -> { count; children })
    |> Jsont.Object.mem "count" count ~enc:(fun n -> n.count)
         ~dec_absent:(fun () -> ref 0)
    |> Jsont.Object.mem "children" (Jsont.list (Jsont.rec' self))
         ~enc:(fun n -> n.children) ~dec_absent:(fun () -> [])
    |> Jsont.Object.finish)
  |> Jsont.Portable_lazy.force

let get = function Ok value -> value | Error message -> failwith message
let check message condition = if not condition then failwith message

let () =
  let first = get (Yamlt.decode_string codec "{}") in
  let second = get (Yamlt.decode_string codec "null") in
  first.count := 7;
  check "absent mutable defaults are independent" (!(second.count) = 0);
  let tree = get (Yamlt.decode_string codec "children:\n  - {}\n  - count: 4\n") in
  (match tree.children with
   | [left; right] ->
       left.count := 2;
       check "recursive defaults are independent"
         (!(tree.count) = 0 && !(right.count) = 4)
   | _ -> failwith "recursive children were not decoded");
  let buffer = Buffer.create 128 in
  get (Yamlt.encode codec tree ~eod:true (Bytesrw.Bytes.Writer.of_buffer buffer));
  let encoded = Buffer.contents buffer in
  let decoded = get (Yamlt.decode_string codec encoded) in
  check "recursive codec round trip" (decoded = tree)

let () =
  let case = Jsont.Object.Case.map "node" codec ~dec:Fun.id in
  let tagged =
    Jsont.Object.map Fun.id
    |> Jsont.Object.case_mem "kind" Jsont.string [Jsont.Object.Case.make case]
         ~tag_to_string:Fun.id ~dec_absent:"node"
    |> Jsont.Object.finish
  in
  let node = get (Yamlt.decode_string tagged "count: 3") in
  check "absent case tag selects the default codec" (!(node.count) = 3)
