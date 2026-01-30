open Base
open Hardcaml
module Out_channel = Stdio.Out_channel

module Make (Regs : Interface.S_with_ast) = struct
  let get_docs ast =
    let rec docs ast =
      match ast with
      | { Interface.Ast.Field.name = _
        ; type_ = Signal { bits = _; rtlname }
        ; sequence = None
        ; doc
        } -> [ rtlname, doc ]
      | { Interface.Ast.Field.name = _
        ; type_ = Signal { bits = _; rtlname }
        ; sequence = Some { kind = _; length }
        ; doc
        } ->
        let flds =
          List.init length ~f:(fun j ->
            rtlname ^ "_" ^ Int.to_string j, if j = 0 then doc else None)
        in
        flds
      | { Interface.Ast.Field.name = _
        ; type_ = Module { name = _; ast }
        ; sequence = None
        ; doc = _
        } -> List.map ast ~f:docs |> List.concat
      | _ ->
        raise_s
          [%message "Can only get [docs] from simple fields, or arrays of simple fields"]
    in
    let rec docs_ast alist ast =
      match ast with
      | [] -> Regs.Unsafe_assoc_by_port_name.of_alist alist
      | hd :: tl ->
        let l = docs hd in
        docs_ast (l @ alist) tl
    in
    docs_ast [] ast
  ;;

  let format_doc_string ~indent str =
    String.split_lines str
    |> List.map
         ~f:
           (String.lstrip ~drop:(function
             | ' ' -> true
             | _ -> false))
    |> List.map
         ~f:
           (String.rstrip ~drop:(function
             | ' ' | '\n' | '\r' -> true
             | _ -> false))
    |> List.map ~f:(fun s -> if String.is_empty s then s else indent ^ s)
    |> String.concat ~sep:"\n"
  ;;

  let output_struct_type
    ?(don't_add_trailing_semicolon = false)
    ?(indent = "")
    ?(typ_name = "_registers")
    f
    =
    Out_channel.fprintf f "%sstruct %s {\n" indent typ_name;
    let offset = ref 0 in
    Regs.iter2
      Regs.port_names_and_widths
      (get_docs Regs.ast)
      ~f:(fun (register_name, width) docs ->
        Out_channel.fprintf
          f
          "%s    /* %s (bits = %i, address offset = %i)"
          indent
          register_name
          width
          !offset;
        offset := !offset + 4;
        (match docs with
         | None -> Out_channel.fprintf f " */\n"
         | Some docs ->
           Out_channel.fprintf
             f
             "\n\n%s\n%s    */\n"
             (format_doc_string ~indent:(indent ^ "       ") docs)
             indent);
        Out_channel.fprintf f "%s    uint32_t %s;\n" indent register_name);
    Out_channel.fprintf f "%s}" indent;
    if not don't_add_trailing_semicolon then Out_channel.fprintf f ";\n"
  ;;

  let output_struct_address_offsets
    ?(c90 = true)
    ?(indent = "")
    ?(typ_name = "_registers")
    ?name
    f
    =
    let offset = ref 0 in
    let name =
      match name with
      | None -> typ_name ^ "_addresses"
      | Some name -> name
    in
    Out_channel.fprintf f "%sstatic const struct %s %s = {\n" indent typ_name name;
    Regs.iter Regs.port_names ~f:(fun register_name ->
      let register_name =
        if c90
        then Printf.sprintf "/* %-40s */" register_name
        else Printf.sprintf ".%-40s = " register_name
      in
      Out_channel.fprintf f "%s    %s %d,\n" indent register_name !offset;
      offset := !offset + 4);
    Out_channel.fprintf f "%s};" indent
  ;;
end

module Make_read_write (Read : Interface.S_with_ast) (Write : Interface.S_with_ast) =
struct
  module Read = Make (Read)
  module Write = Make (Write)

  let output ?(name = "_registers") f =
    Out_channel.fprintf f "union %s {\n" name;
    Read.output_struct_type
      ~don't_add_trailing_semicolon:true
      ~indent:"  "
      ~typ_name:"_read"
      f;
    Out_channel.fprintf f " read;\n";
    Write.output_struct_type
      ~don't_add_trailing_semicolon:true
      ~indent:"  "
      ~typ_name:"_write"
      f;
    Out_channel.fprintf f " write;\n";
    Out_channel.fprintf f "}"
  ;;
end
