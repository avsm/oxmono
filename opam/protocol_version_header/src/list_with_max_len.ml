open! Core
include List_with_max_len_intf

module Make (Config : Config) = struct
  open Stable_witness.Export
  open Config

  let max_len = Config.max_len

  type 'a t = 'a iarray [@@deriving bin_write ~localize, globalize, stable_witness]

  let __bin_read_t__ = Iarray.__bin_read_t__

  (* Our tests show that this bin-prots equivalently to list, and we evolved this type
     from one that originally derived bin_shape for list, so we maintain that to avoid
     bin_digests changing. *)
  let bin_shape_t = bin_shape_list

  let bin_read_t bin_read_el buf ~pos_ref : 'a t =
    let old_pos = !pos_ref in
    let len = (Bin_prot.Read.bin_read_nat0 buf ~pos_ref :> int) in
    pos_ref := old_pos;
    if len > max_len
    then
      raise_s
        [%message "Array is too large" (context : Info.t) (len : int) (max_len : int)]
    else Bin_prot.Read.bin_read_iarray bin_read_el buf ~pos_ref
  ;;

  let bin_read_t__local bin_read_el buf ~pos_ref : 'a t =
    let old_pos = !pos_ref in
    let len = (Bin_prot.Read.bin_read_nat0__local buf ~pos_ref :> int) in
    pos_ref := old_pos;
    if len > max_len
    then
      raise_s
        [%message "Array is too large" (context : Info.t) (len : int) (max_len : int)]
    else exclave_ Bin_prot.Read.bin_read_iarray__local bin_read_el buf ~pos_ref
  ;;

  let bin_reader_t (bin_reader_a : 'a Bin_prot.Type_class.reader) =
    { Bin_prot.Type_class.read = bin_read_t bin_reader_a.read
    ; vtag_read = __bin_read_t__ bin_reader_a.read
    }
  ;;

  let bin_t (bin_a : 'a Bin_prot.Type_class.t) =
    { Bin_prot.Type_class.shape = bin_shape_t bin_a.shape
    ; writer = bin_writer_t bin_a.writer
    ; reader = bin_reader_t bin_a.reader
    }
  ;;

  let of_list_exn l =
    let len = List.length l in
    if len > max_len
    then
      raise_s
        [%message "List is too large" (context : Info.t) (len : int) (max_len : int)];
    Iarray.of_list l
  ;;

  let sexp_of_t = Iarray.sexp_of_t

  let t_of_sexp t_of_a sexp =
    let t = List.t_of_sexp t_of_a sexp in
    of_list_exn t
  ;;

  let to_list = Iarray.to_list
  let fold = Iarray.fold
  let to_iarray = Fn.id
end
