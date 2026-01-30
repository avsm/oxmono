open Base
open Hardcaml

module type S = Register_bank_intf.S

module Packed_array = struct
  module type S = Register_bank_intf.Packed_array.S

  module Make (X : sig
      include Interface.S

      val name : string
    end) =
  struct
    type 'a unpacked = 'a X.t

    module Pre = struct
      type 'a t = 'a array [@@deriving equal ~localize, compare ~localize, sexp_of]

      let word_size = 32
      let num_words = (X.sum_of_port_widths + word_size - 1) / word_size

      let port_names_and_widths =
        Array.init num_words ~f:(fun i -> X.name ^ "_" ^ Int.to_string i, 32)
      ;;

      let map = Array.map
      let map2 = Array.map2_exn
      let iter = Array.iter
      let iter2 = Array.iter2_exn
      let to_list = Array.to_list
    end

    include Pre
    include Interface.Make (Pre)

    let to_packed_array (type a) (module Comb : Comb.S with type t = a) (x : a X.t) =
      let module C = X.Make_comb (Comb) in
      C.pack x
      |> Comb.split_lsb ~part_width:32 ~exact:false
      |> List.map ~f:(fun x -> Comb.uresize x ~width:32)
      |> Array.of_list
    ;;

    let to_packed_array_latch_on_read
      how_to_latch
      spec
      (x : Signal.t X.t)
      ~(read_enable : Signal.t t)
      =
      let open Signal in
      match how_to_latch with
      | `Latch_all ->
        to_packed_array (module Signal) x
        |> Array.mapi ~f:(fun i d ->
          (* The lowest index must be cut through for read latencies of 0 and latched for
             nonzero read latencies. the upper indices are latched when the lowest index
             is read. *)
          let enable = read_enable.(0) in
          if i = 0 then cut_through_reg spec ~enable d else reg spec ~enable d)
      | `Latch_by_field ->
        X.map3 x (X.offsets ()) X.port_widths ~f:(fun d bit_offset width ->
          (* Similar to above, the bits of the field that are contained in the lowest
             array index must be cut through for read latencies of 0, and latched for
             nonzero read latencies. The remaining bits can be latched. *)
          let base_address = bit_offset / word_size in
          let lowest_index_bits = word_size - (bit_offset % word_size) in
          let enable = read_enable.(base_address) in
          if lowest_index_bits >= width
          then cut_through_reg spec ~enable d
          else (
            let d_hi, d_lo = split_in_half_lsb ~lsbs:lowest_index_bits d in
            reg spec ~enable d_hi @: cut_through_reg spec ~enable d_lo))
        |> to_packed_array (module Signal)
    ;;

    let of_packed_array (type a) (module Comb : Comb.S with type t = a) (t : a t) : a X.t =
      let module C = X.Make_comb (Comb) in
      Comb.uresize (Comb.concat_lsb (Array.to_list t)) ~width:X.sum_of_port_widths
      |> C.unpack
    ;;

    let bit_positions = X.map2 (X.offsets ()) X.port_widths ~f:(fun o w -> o, w)

    let of_packed_array_with_valid
      (type a)
      (module Comb : Comb.S with type t = a)
      (t : a With_valid.t t)
      : a With_valid.t X.t
      =
      let x = of_packed_array (module Comb) (Array.map t ~f:(fun v -> v.value)) in
      X.map2 x bit_positions ~f:(fun value (offset, width) ->
        { With_valid.valid = t.((offset + width - 1) / 32).valid; value })
    ;;

    module Set_in (I : Int.S) = struct
      let set_in_int ~(orig : int) ~(offset : int) ~(width : int) ~(v : I.t) =
        assert (offset >= 0 && offset < Int.num_bits);
        assert (width > 0 && offset + width < Int.num_bits);
        (* compute the piece from [v] that we will insert into [orig] *)
        let width_mask =
          if width = I.(num_bits |> to_int_exn)
          then I.minus_one
          else I.((one lsl width) - one)
        in
        let v_mask = I.((v land width_mask) lsl offset |> to_int_exn) in
        (* compute the mask to unset the (width, offset) region of [orig] *)
        let unset_region_mask =
          let width_mask =
            if width = Int.num_bits then Int.minus_one else Int.((one lsl width) - one)
          in
          lnot (width_mask lsl offset)
        in
        (* set the region in [orig] *)
        orig land unset_region_mask lor v_mask
      ;;
    end

    module Extract_field_as (I : Int.S) = struct
      let () = assert (I.(num_bits |> to_int_exn) >= 32)

      open Set_in (I)

      let rec loop f (offset, width) (out_offset, out_word) packed =
        let word = offset / 32 in
        let bit_offset = offset land 31 in
        let bits_left = 32 - bit_offset in
        let out_word = f word bit_offset bits_left (out_offset, out_word) packed in
        if bits_left >= width
        then out_word
        else
          loop
            f
            (offset + bits_left, width - bits_left)
            (out_offset + bits_left, out_word)
            packed
      ;;

      let loop_set (offset, width) (out_offset, out_word) packed =
        let f word bit_offset bits_left (set_offset, set_word) packed =
          packed.(word)
          <- set_in_int
               ~orig:packed.(word)
               ~offset:bit_offset
               ~width:bits_left
               ~v:I.(set_word lsr set_offset);
          set_word
        in
        ignore (loop f (offset, width) (out_offset, out_word) packed : I.t)
      ;;

      let loop_extract =
        let f word bit_offset _bits_left (out_offset, out_word) packed =
          I.(out_word lor ((I.of_int_exn packed.(word) lsr bit_offset) lsl out_offset))
        in
        loop f
      ;;

      let extract (offset, width) packed =
        if width > I.(num_bits |> to_int_exn)
        then
          raise_s
            [%message
              "Cannot extract field as int -  too wide" (width : int) (I.num_bits : I.t)];
        let mask =
          if width = I.(num_bits |> to_int_exn)
          then I.minus_one
          else I.((one lsl width) - one)
        in
        let out_word = loop_extract (offset, width) (0, I.zero) packed in
        I.(out_word land mask)
      ;;

      let set (offset, width) packed set_word =
        if width > I.(num_bits |> to_int_exn)
        then
          raise_s
            [%message
              "Cannot set field as int - too wide" (width : int) (I.num_bits : I.t)];
        loop_set (offset, width) (0, set_word) packed
      ;;
    end

    module E_int = Extract_field_as (Int)

    let extract_field_as_int = X.map bit_positions ~f:E_int.extract
    let set_field_as_int = X.map bit_positions ~f:E_int.set

    module E_int64 = Extract_field_as (Int64)

    let extract_field_as_int64 = X.map bit_positions ~f:E_int64.extract
    let set_field_as_int64 = X.map bit_positions ~f:E_int64.set

    let set_field_as_bytes =
      let module Set_in = Set_in (Int) in
      let set_in_int = Set_in.set_in_int in
      let rec loop (offset, width) (byte_pos, bytes) packed =
        if width <= 0
        then ()
        else (
          let cur_width = Int.min width 8 in
          let start_word = offset / 32 in
          let start_bit_offset = offset land 31 in
          let end_word = (offset + cur_width - 1) / 32 in
          let byte = Bytes.get bytes byte_pos |> Char.to_int in
          if start_word = end_word
          then
            packed.(start_word)
            <- set_in_int
                 ~orig:packed.(start_word)
                 ~offset:start_bit_offset
                 ~width:cur_width
                 ~v:byte
          else (
            assert (end_word = start_word + 1);
            let start_byte_bits = 32 - start_bit_offset in
            packed.(start_word)
            <- set_in_int
                 ~orig:packed.(start_word)
                 ~offset:start_bit_offset
                 ~width:start_byte_bits
                 ~v:byte;
            let end_byte_bits = cur_width - start_byte_bits in
            packed.(end_word)
            <- set_in_int
                 ~orig:packed.(end_word)
                 ~offset:0
                 ~width:end_byte_bits
                 ~v:(byte lsr start_byte_bits));
          loop (offset + 8, width - 8) (byte_pos + 1, bytes) packed)
      in
      X.map bit_positions ~f:(fun (offset, width) packed bytes ->
        assert (Bytes.length bytes = Int.round_up ~to_multiple_of:8 width / 8);
        loop (offset, width) (0, bytes) packed)
    ;;

    let extract_field_as_bytes =
      let rec loop (offset, width) b packed cur_byte =
        if width <= 0
        then ()
        else (
          let cur_width = Int.min width 8 in
          let cur_bitmask = (1 lsl cur_width) - 1 in
          let start_word = offset / 32 in
          let start_bit_offset = offset land 31 in
          let end_word = (offset + cur_width - 1) / 32 in
          let c =
            (if start_word = end_word
             then
               (* byte is in one word; just pull it out *)
               (packed.(start_word) lsr start_bit_offset) land cur_bitmask
             else (
               assert (end_word = start_word + 1);
               let start_byte_bits = 32 - start_bit_offset in
               let start_byte_bitmask = (1 lsl start_byte_bits) - 1 in
               let start_piece =
                 (packed.(start_word) lsr start_bit_offset) land start_byte_bitmask
               in
               let end_byte_bits = cur_width - start_byte_bits in
               let end_byte_bitmask = (1 lsl end_byte_bits) - 1 in
               let end_piece = packed.(end_word) land end_byte_bitmask in
               (end_piece lsl start_byte_bits) lor start_piece))
            |> Char.of_int_exn
          in
          Bytes.set b cur_byte c;
          loop (offset + 8, width - 8) b packed (cur_byte + 1))
      in
      X.map bit_positions ~f:(fun (offset, width) packed b ->
        assert (8 * Bytes.length b >= width);
        loop (offset, width) b packed 0)
    ;;

    let extract_field_as_string =
      X.map2 X.port_widths extract_field_as_bytes ~f:(fun bit_width f ->
        let byte_width = Int.round_up ~to_multiple_of:8 bit_width / 8 in
        let b = Bytes.create byte_width in
        fun i ->
          f i b;
          Bytes.to_string b)
    ;;

    let set_field_as_string =
      X.map set_field_as_bytes ~f:(fun f i s ->
        let b = Bytes.of_string s in
        f i b)
    ;;

    let hold = Array.init num_words ~f:(Fn.const Register_mode.hold)

    let of_packed_int_array t =
      X.map extract_field_as_int ~f:(fun extract_fn -> extract_fn t)
    ;;

    let of_packed_int_array_to_int64 t =
      X.map extract_field_as_int64 ~f:(fun extract_fn -> extract_fn t)
    ;;

    let to_packed_int_array unpacked =
      let packed = Array.create ~len:num_words 0 in
      X.iter2 set_field_as_int unpacked ~f:(fun set_fn field -> set_fn packed field);
      packed
    ;;
  end

  module Include = struct
    module type S = sig
      type 'a unpacked

      module Packed : S with type 'a unpacked = 'a unpacked
    end

    module type F = functor (X : Interface.S) -> S with type 'a unpacked := 'a X.t

    module Make (X : sig
        include Interface.S

        val name : string
      end) =
    struct
      module Packed = Make (X)
    end
  end

  module Int64 = struct
    module T = struct
      let name = "int64"

      include (val Types.scalar ~wave_format:Int ~name 64)
    end

    include T

    (* Cannot use include functor here. *)
    module Packed = Make (T)
  end
end

module Make_read_enable (X : Interface.S) = struct
  module T = struct
    include X

    let port_names_and_widths = map port_names ~f:(fun n -> "ren$" ^ n, 1)
  end

  include T
  include Interface.Make (T)
end

module Make
    (Master_to_slave : Internal_bus_ports.Master_to_slave)
    (Slave_to_master : Internal_bus_ports.Slave_to_master) =
struct
  open Signal
  module Master_to_slave = Master_to_slave
  module Slave_to_master = Slave_to_master

  module Without_interface = struct
    type result =
      { write_values : Signal.t With_valid.t list
      ; read_enables : Signal.t list
      }
    [@@deriving sexp_of]

    type t = (Signal.t Slave_to_master.t, result) Slave_with_data.t

    include Internal_bus_utils.Make (Master_to_slave) (Slave_to_master)

    let tree_mux ~cycles ~reg selector data =
      if cycles = 0
      then mux selector data
      else
        Hardcaml_circuits.Pipelined_tree_mux.pipelined_tree_mux
          ~cycles
          ~reg
          ~selector
          data
    ;;

    type pipelined_read_depth =
      { external_cycles : int
      ; internal_mux_cycles : int
      }

    let create
      ?(pipelined_read_depth = { external_cycles = 0; internal_mux_cycles = 0 })
      reg_spec
      ~clear_write_values
      ~(master : _ Master_to_slave.t)
      ~write_modes
      ~read_values
      =
      List.iter read_values ~f:(fun read_value ->
        assert (width read_value <= Master_to_slave.data_bits));
      let total_pipelined_read_depth =
        pipelined_read_depth.external_cycles + pipelined_read_depth.internal_mux_cycles
      in
      let num_read_values = List.length read_values in
      let num_write_values = List.length write_modes in
      let read_addr = word_address ~master ~size:num_read_values in
      let write_addr = word_address ~master ~size:num_write_values in
      let write_values =
        let wa1h = binary_to_onehot write_addr.value in
        List.mapi write_modes ~f:(fun i (mode : Register_mode.t) ->
          let e = master.write_first &: wa1h.:(i) &: write_addr.valid in
          let d = master.write_data in
          let e, d =
            match Register_mode.mode mode with
            | Toggle_low -> vdd, mux2 e d (width d |> zero)
            | Toggle_high -> vdd, mux2 e d (width d |> ones)
            | Hold -> e, d
          in
          (* internal clear, if required *)
          let clear =
            if Register_mode.internal_clear mode
            then Some (Reg_spec.clear_exn reg_spec |: clear_write_values)
            else None
          in
          (* default value after clear *)
          let clear_to =
            if Register_mode.clear_to mode <> 0
            then Some (of_int_trunc ~width:(width d) (Register_mode.clear_to mode))
            else None
          in
          { With_valid.valid = reg reg_spec e
          ; value =
              reg reg_spec ?clear ?clear_to ~enable:e d
              |> Fn.flip add_attribute (Rtl_attribute.Vivado.extract_enable false)
          })
      in
      let slave =
        let read_mux =
          match read_values with
          | [] -> zero 32
          | [ x ] -> x
          | _ ->
            let cycles = pipelined_read_depth.internal_mux_cycles in
            mux2
              read_addr.valid
              (tree_mux ~cycles ~reg:(reg reg_spec) read_addr.value read_values)
              (pipeline reg_spec ~n:cycles (ones 32))
        in
        create_slave
          ~read_latency:(total_pipelined_read_depth + 1)
          ~write_latency:1
          ~reg_spec
          ~master
          ~read_data:
            (Signal.reg
               reg_spec
               ~enable:(pipeline reg_spec ~n:total_pipelined_read_depth master.read_first)
               read_mux)
      in
      let read_enables =
        if num_read_values = 0
        then []
        else (
          let read_enables =
            (binary_to_onehot read_addr.value).:[num_read_values - 1, 0]
          in
          read_enables
          &: repeat (master.read_first &: read_addr.valid) ~count:(width read_enables)
          |> bits_lsb)
      in
      { Slave_with_data.slave; data = { write_values; read_enables } }
    ;;
  end

  module With_interface (Read : Interface.S) (Write : Interface.S) = struct
    module Write_with_valid = With_valid.Fields.Make (Write)
    module Read_enable = Make_read_enable (Read)

    module I = struct
      type 'a t =
        { clock : 'a
        ; clear : 'a
        ; clear_write_values : 'a
        ; master : 'a Master_to_slave.t
        ; read_values : 'a Read.t
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    module O = struct
      type 'a t =
        { slave : 'a Slave_to_master.t
        ; write_values : 'a Write_with_valid.t
        ; read_enable : 'a Read_enable.t
        }
      [@@deriving hardcaml ~rtlmangle:false]
    end

    let write_addresses =
      Write.(scan port_names ~init:0 ~f:(fun addr _ -> addr + 1, addr * 4))
    ;;

    let read_addresses =
      Read.(scan port_names ~init:0 ~f:(fun addr _ -> addr + 1, addr * 4))
    ;;

    let create ?pipelined_read_depth _scope ~write_modes (i : _ I.t) =
      let reg_spec = Reg_spec.create ~clock:i.clock ~clear:i.clear () in
      let write_modes = Write.to_list write_modes in
      let read_values =
        Read.zip Read.port_names_and_widths i.read_values
        |> Read.to_list
        |> List.map ~f:(fun ((name, intf_width), s) ->
          if Signal.width s > 32
          then
            raise_s
              [%message
                "register width > 32 bit"
                  (name : string)
                  (Signal.width s : int)
                  (intf_width : int)]
          else Signal.uresize s ~width:32)
      in
      let { Slave_with_data.slave
          ; data = { Without_interface.write_values; read_enables }
          }
        =
        Without_interface.create
          ?pipelined_read_depth
          reg_spec
          ~master:i.master
          ~write_modes
          ~read_values
          ~clear_write_values:i.clear_write_values
      in
      let write_values =
        let t =
          Write.to_list Write.port_names
          |> List.map2_exn write_values ~f:(fun s n -> n, s)
        in
        Write.map Write.port_names_and_widths ~f:(fun (n, width) ->
          if width > 32
          then raise_s [%message "write register width >32b" (n : string) (width : int)];
          let { With_valid.valid; value } = List.Assoc.find_exn t n ~equal:String.equal in
          { With_valid.valid; value = value.Signal.:[width - 1, 0] })
      in
      let read_enable =
        let t = List.zip_exn (Read.to_list Read.port_names) read_enables in
        Read.map Read.port_names ~f:(fun name ->
          List.Assoc.find_exn t name ~equal:String.equal)
      in
      { O.slave; write_values; read_enable }
    ;;

    let hierarchical ?instance ?pipelined_read_depth scope ~write_modes =
      let module H = Hierarchy.In_scope (I) (O) in
      H.hierarchical
        ?instance
        ~scope
        ~name:"register_bank"
        (create ?pipelined_read_depth ~write_modes)
    ;;
  end

  include Without_interface
end
