open! Stdppx
open! Import
open Language.Typed
module Type = Language.Type
open Result.Let_syntax
include Monomorphize_intf.Definitions

module Context = struct
  module Defaults = struct
    type t = Value.Basic.packed list Maybe_explicit.t Axis.Map.t

    let empty = Axis.Map.empty
  end

  type t =
    { ghostify : bool
    ; env : Env.t
    ; defaults : Defaults.t
    }

  let top = { ghostify = false; env = Env.initial; defaults = Defaults.empty }
end

module Maybe_hide_in_docs = struct
  type 'a t =
    { node : 'a
    ; hide_in_docs : bool
    }

  let node t = t.node
  let hide_in_docs t = t.hide_in_docs
end

let extract_loc (type a) (ctx : a Attributes.Context.poly) (node : a) =
  match ctx with
  | Value_binding -> node.pvb_loc
  | Value_description -> node.pval_loc
  | Module_binding -> node.pmb_loc
  | Module_declaration -> node.pmd_loc
  | Type_declaration -> node.ptype_loc
  | Module_type_declaration -> node.pmtd_loc
  | Include_infos -> node.pincl_loc
;;

let extract_attrs (type a) (ctx : a Attributes.Context.poly) (node : a) =
  match ctx with
  | Value_binding -> node.pvb_attributes
  | Value_description -> node.pval_attributes
  | Module_binding -> node.pmb_attributes
  | Module_declaration -> node.pmd_attributes
  | Type_declaration -> node.ptype_attributes
  | Module_type_declaration -> node.pmtd_attributes
  | Include_infos -> node.pincl_attributes
;;

type error =
  { loc : Location.t
  ; err : Syntax_error.t
  ; attrs : attributes
  }

let extract_bindings (type a) (ctx : a Attributes.Context.poly) node =
  match Attributes.consume Attributes.poly ctx node with
  | Ok (node, bindings) -> Ok (node, bindings)
  | Error err -> Error { loc = extract_loc ctx node; err; attrs = extract_attrs ctx node }
;;

let extract_floating (type a) (ctx : a Attributes.Floating.Context.poly) item =
  match Attributes.Floating.convert ctx item with
  | Ok _ as ok -> ok
  | Error err ->
    let loc =
      match ctx with
      | Structure_item -> item.pstr_loc
      | Signature_item -> item.psig_loc
    in
    Error (loc, err)
;;

let stable_dedup list ~compare =
  List.mapi list ~f:(fun i x -> i, x)
  |> List.sort_uniq ~cmp:(fun (_, x) (_, y) -> compare x y)
  |> List.sort ~cmp:(fun (i, _) (j, _) -> Int.compare i j)
  |> List.map ~f:snd
;;

let handle_one_binding
  ~original_env
  envs
  ( explicitness
  , Attributes.Poly.Binding
      { binding = { pattern; expression; lookup }; mangle; mangle_axis } )
  =
  envs
  >>| List.map ~f:(fun (env, manglers) ->
    expression
    |> Env.eval original_env lookup
    (* If multiple expressions evaluate to the same value, only generate one instance. *)
    >>| Nonempty_list.to_list
    >>| stable_dedup ~compare:Value.compare
    >>| List.map ~f:(fun value ->
      let+ env =
        Env.bind env ~loc:expression.loc ~is_set:(Axis.is_set mangle_axis) pattern value
      in
      let mangle_value = Value.Basic.P (mangle value) in
      let manglers =
        Axis.Map.update
          (P mangle_axis)
          (function
            | None -> Some (explicitness, [ mangle_value ])
            | Some (_old_explicitness, values) ->
              (* Drop previous explicit flag and use new explicit flag *)
              Some (explicitness, mangle_value :: values))
          manglers
      in
      env, manglers)
    >>= Result.all)
  >>= Result.all
  >>| List.concat
;;

let handle_one_attribute
  ~original_env
  envs
  (explicit, Attributes.Poly.Poly (mangle_axis, bindings))
  =
  (* Explicitly set the entry in the manglers for the current [type_] to empty so that
     putting an empty [[@@kind]] attribute disables the default kind mangling from
     [[@@@kind.default]]. *)
  let init =
    envs
    >>| List.map ~f:(fun (env, manglers) ->
      ( env
      , Axis.Map.add
          (P mangle_axis)
          (* The initial state should be no explicit mangling. *)
          (Maybe_explicit.Drop_axis_if_all_defaults, [])
          manglers ))
  in
  bindings
  |> List.map ~f:(fun binding -> explicit, binding)
  |> List.fold_left ~init ~f:(handle_one_binding ~original_env)
;;

type 'a instance =
  { node : 'a
  ; env : Env.t
  ; manglers : Value.Basic.packed list Maybe_explicit.t Axis.Map.t
  }

let instantiate poly_attributes ~env:(original_env : Env.t) =
  let envs =
    List.fold_left
      poly_attributes
      ~init:(Ok [ original_env, Context.Defaults.empty ])
      ~f:(handle_one_attribute ~original_env)
  in
  envs
  >>| List.map ~f:(fun (env, manglers) ->
    { node = ()
    ; env
    ; manglers =
        (* Manglers were added in reverse order *)
        Axis.Map.map (Maybe_explicit.map ~f:List.rev) manglers
    })
;;

let consume_poly
  (type a)
  ~env
  ~(defaults : Context.Defaults.t)
  (attr_ctx : a Attributes.Context.poly)
  nodes
  =
  List.map nodes ~f:(fun node ->
    match extract_bindings attr_ctx node with
    | Error _ as err -> err
    | Ok (node, polys) ->
      (match instantiate polys ~env with
       | Error err ->
         Error
           { loc = extract_loc attr_ctx node; err; attrs = extract_attrs attr_ctx node }
       | Ok list ->
         let instances =
           List.map list ~f:(fun { node = (); env; manglers } ->
             (* Use current default manglers when axis is unspecified. *)
             let manglers =
               Axis.Map.merge
                 (fun _ defaults manglers ->
                   match defaults, manglers with
                   | None, None -> None
                   | _, Some _ -> manglers
                   | Some _, None -> defaults)
                 defaults
                 manglers
             in
             (* Associate node with the instance *)
             { node; env; manglers })
         in
         Ok instances))
  |> List.fold_right ~init:(Ok []) ~f:(fun head tail ->
    match head, tail with
    | Ok head, Ok tail -> Ok (head :: tail)
    | Error head, Error tail -> Error (head :: tail)
    | Error head, Ok _ -> Error [ head ]
    | Ok _, Error tail -> Error tail)
  |> function
  | Ok instances -> Ok (List.concat instances)
  | Error errs ->
    let locs = List.map errs ~f:(fun err -> err.loc)
    and errs = List.map errs ~f:(fun err -> err.err)
    and attrs = List.concat_map errs ~f:(fun err -> err.attrs) in
    let loc =
      List.fold_left ~init:(List.hd locs) (List.tl locs) ~f:(fun a b ->
        { loc_start = Location.min_pos a.loc_start b.loc_start
        ; loc_end = Location.max_pos a.loc_end b.loc_end
        ; loc_ghost = a.loc_ghost || b.loc_ghost
        })
    in
    let err =
      match errs with
      | err :: (_ :: _ as errs) -> Syntax_error.combine err errs
      | [ err ] -> err
      | [] -> Syntax_error.createf ~loc "empty error list"
    in
    Error { loc; err; attrs }
;;

(* Ppxlib individually applies every [@@deriving] or [@@deriving_inline] attribute it
   encounters to all type declarations in the same group, so if we naively copy them to
   each synthetic declaration produced by ppx_template, we end up with many copies of the
   same code. To fix this, we deduplicate attributes from the same source location. *)
let deduplicate_deriving_attrs tds =
  let module Location_ignoring_ghost =
    Set.Make (struct
      type t = location

      let compare a b =
        match Location.compare_pos a.loc_start b.loc_start with
        | 0 -> Location.compare_pos a.loc_end b.loc_end
        | c -> c
      ;;
    end)
  in
  let seen = ref Location_ignoring_ghost.empty in
  List.map tds ~f:(fun td ->
    { td with
      ptype_attributes =
        List.filter td.ptype_attributes ~f:(function
          | { attr_name =
                { txt =
                    ( "ppxlib.deriving"
                    | "ppxlib.deriving_inline"
                    | "deriving"
                    | "deriving_inline" )
                ; loc
                }
            ; _
            } ->
            if Location_ignoring_ghost.mem loc !seen
            then false
            else (
              seen := Location_ignoring_ghost.add loc !seen;
              true)
          | _ -> true)
    })
;;

let is_local (mode : (Type.mode, Expression.singleton) Expression.t Loc.t) ~env =
  let loc = mode.loc in
  Result.bind (Env.eval_singleton env mode) ~f:(fun (Identifier ident) ->
    match ident.ident with
    | "local" -> Ok true
    | "global" -> Ok false
    | mode ->
      Error (Syntax_error.createf ~loc "Unknown or invalid mode identifier: %s" mode))
;;

let is_stack (alloc : (Type.alloc, Expression.singleton) Expression.t Loc.t) ~env =
  let loc = alloc.loc in
  Result.bind (Env.eval_singleton env alloc) ~f:(fun (Identifier ident) ->
    match ident.ident with
    | "stack" -> Ok true
    | "heap" -> Ok false
    | alloc -> Error (Syntax_error.createf ~loc "Unbound alloc identifier: %s" alloc))
;;

let should_wrap_with_exclave
  expr
  ~(exclave_because_stack : _ Attributes.Exclave_if.t option)
  ~(exclave_because_local : _ Attributes.Exclave_if.t option)
  ~env
  =
  let stack_result =
    match exclave_because_stack with
    | None -> None
    | Some { expr = alloc; reasons = _ } ->
      (match is_stack alloc ~env with
       | (Ok true | Error _) as result ->
         Some
           (Result.map_error
              result
              ~f:(Syntax_error_conversion.to_extension_node Expression expr))
       | Ok false -> None)
  in
  match stack_result with
  | Some result -> result
  | None ->
    (match exclave_because_local with
     | Some { expr = mode; reasons } ->
       let rec is_ident_or_field_or_constant = function
         | { pexp_desc = Pexp_ident _ | Pexp_constant _ | Pexp_construct (_, None); _ } ->
           true
         | { pexp_desc = Pexp_field (expr, _); _ } -> is_ident_or_field_or_constant expr
         | _ -> false
       in
       let rec is_pure_allocation ({ pexp_desc; pexp_loc; _ } as expr) =
         match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc:pexp_loc with
         | Pexp_tuple labeled_exprs ->
           List.for_all ~f:(fun (_, expr) -> is_pure_allocation expr) labeled_exprs
         | Pexp_construct (_, None) | Pexp_variant (_, None) -> true
         | Pexp_construct (_, Some expr) | Pexp_variant (_, Some expr) ->
           is_pure_allocation expr
         | Pexp_record (fields, expr) ->
           List.for_all ~f:(fun (_, e) -> is_pure_allocation e) fields
           &&
             (match expr with
             | None -> true
             | Some expr -> is_pure_allocation expr)
         | Pexp_array (_mut, exprs) -> List.for_all ~f:is_pure_allocation exprs
         | _ -> is_ident_or_field_or_constant expr
       in
       let is_allowable =
         match expr.pexp_desc with
         | Pexp_apply (f, args) ->
           is_ident_or_field_or_constant f
           && List.for_all ~f:(fun (_, e) -> is_ident_or_field_or_constant e) args
         | _ -> is_pure_allocation expr
       in
       if is_allowable || not (List.is_empty reasons)
       then
         is_local mode ~env
         |> Result.map_error
              ~f:(Syntax_error_conversion.to_extension_node Expression expr)
       else
         Error
           ((* Ideally this check is conservative, which seems fine for now. *)
            Syntax_error.createf
              ~loc:expr.pexp_loc
              "[%%template]: exclave_if_local is only allowed on: tailcalls; syntactic \
               allocations (e.g. tuples) consisting entirely of identifiers, record \
               fields, and/or constants; or when given a nonempty ~reasons argument."
            |> Syntax_error_conversion.to_extension_node Expression expr)
     | None -> Ok false)
;;

let consume_zero_alloc_if
  (type a)
  (ctx : a Attributes.Context.zero_alloc_if)
  (node : a)
  ~env
  =
  let* node, zero_alloc_if_local =
    Attributes.consume Attributes.zero_alloc_if_local ctx node
  in
  let* node, zero_alloc_if_stack =
    Attributes.consume Attributes.zero_alloc_if_stack ctx node
  in
  let* add_zero_alloc_if_local =
    match zero_alloc_if_local with
    | None -> Ok None
    | Some { loc; expr = mode; args } ->
      (match is_local mode ~env with
       | Ok true -> Ok (Some (loc, args))
       | Ok false -> Ok None
       | Error _ as err -> err)
  in
  let+ add_zero_alloc_if_stack =
    match zero_alloc_if_stack with
    | None -> Ok None
    | Some { loc; expr = alloc; args } ->
      (match is_stack alloc ~env with
       | Ok true -> Ok (Some (loc, args))
       | Ok false -> Ok None
       | Error _ as err -> err)
  in
  let add_zero_alloc =
    match add_zero_alloc_if_local, add_zero_alloc_if_stack with
    | (Some _ as some), _ | _, (Some _ as some) -> some
    | None, None -> None
  in
  match add_zero_alloc with
  | None -> node
  | Some (loc, payload) ->
    let loc = { loc with loc_ghost = true } in
    let payload =
      match payload with
      | [] -> []
      | [ expr ] -> [ Ast_builder.pstr_eval ~loc expr [] ]
      | f :: args ->
        let args = List.map args ~f:(fun arg -> Nolabel, arg) in
        [ Ast_builder.pstr_eval ~loc (Ast_builder.pexp_apply ~loc f args) [] ]
    in
    let attribute =
      Ast_builder.attribute ~loc ~name:{ txt = "zero_alloc"; loc } ~payload:(PStr payload)
    in
    (match ctx with
     | Expression -> { node with pexp_attributes = attribute :: node.pexp_attributes }
     | Value_binding -> { node with pvb_attributes = attribute :: node.pvb_attributes }
     | Value_description ->
       { node with pval_attributes = attribute :: node.pval_attributes })
;;

let t ~mk_struct ~mk_sig :> Context.t t =
  let mk_struct ~loc nodes ~f =
    let loc = { loc with loc_ghost = true } in
    (mk_struct ~loc (List.map nodes ~f:(f ~loc))).pstr_desc
  in
  let mk_sig ~loc nodes =
    let loc = { loc with loc_ghost = true } in
    (mk_sig ~loc nodes).psig_desc
  in
  let content_span =
    object
      inherit [Location.t option] Ast_traverse.fold

      method! location loc acc =
        match acc with
        | None -> Some loc
        | Some acc ->
          Some
            { loc_start = Location.min_pos acc.loc_start loc.loc_start
            ; loc_end = Location.max_pos acc.loc_end loc.loc_end
            ; loc_ghost = true
            }
    end
  in
  object (self)
    inherit [Context.t] Ppxlib_jane.Ast_traverse.map_with_context as super
    method! location ctx loc = { loc with loc_ghost = loc.loc_ghost || ctx.ghostify }

    method! jkind_annotation ctx ({ pjkind_desc; pjkind_loc } as jkind) =
      match pjkind_desc with
      | Pjk_abbreviation kind ->
        (match Env.find ctx.env { ident = kind; type_ = Type.kind } with
         | None -> super#jkind_annotation ctx jkind
         | Some kind ->
           let (Jkind_annotation kind) = Value.to_node ~loc:pjkind_loc kind in
           kind)
      | Pjk_mod (base, modifiers) ->
        (* Even though [modifiers] is represented as a list of [mode]s, they conceptually
           act more like modalities. *)
        let modifiers =
          List.map modifiers ~f:(fun { txt = Mode name; loc } ->
            { txt = Ppxlib_jane.Modality name; loc })
          |> self#modalities ctx
          |> List.map ~f:(fun { txt = Modality name; loc } ->
            { txt = Ppxlib_jane.Mode name; loc })
        in
        { pjkind_loc = self#location ctx pjkind_loc
        ; pjkind_desc = Pjk_mod (self#jkind_annotation ctx base, modifiers)
        }
      | _ -> super#jkind_annotation ctx jkind

    method! modes ctx modes =
      List.map modes ~f:(fun { txt = Mode mode; loc } ->
        let loc = self#location ctx loc in
        match Env.find ctx.env { ident = mode; type_ = Type.mode } with
        | None -> { txt = Mode mode; loc }
        | Some mode ->
          let (Mode mode) = Value.to_node ~loc mode in
          mode)

    method! modalities ctx modalities =
      List.map modalities ~f:(fun { txt = Modality modality; loc } ->
        let loc = self#location ctx loc in
        match Env.find ctx.env { ident = modality; type_ = Type.modality } with
        | None -> { txt = Modality modality; loc }
        | Some modality ->
          let (Modality modality) = Value.to_node ~loc modality in
          modality)

    (* The [@kind] attribute can appear on various identifier nodes that reference values,
       modules, or types that were defined using [@kind]. For each node that could be such
       an identifier, we check if the [@kind] attribute is present, and if so, mangle that
       identifier according to the provided layouts. An error node will be created instead
       if the attribute is attached to a node which is not an identifier. *)
    method private visit_mono : type a. a Attributes.Context.mono -> Context.t -> a -> a =
      fun attr_ctx ctx node ->
        (* We can't define a single [visit] function as in [visit_poly] because we need to
           call the superclass method, or else we loop infinitely, and [super] can't be
           passed around as a value (it can only be used directly with a method call). *)
        match Attributes.consume Attributes.mono attr_ctx node with
        | Ok (node, mangle_exprs) ->
          let node = Mangle.mangle attr_ctx node mangle_exprs ~env:ctx.env in
          (match attr_ctx with
           | Expression -> super#expression ctx node
           | Module_expr -> super#module_expr ctx node
           | Core_type -> super#core_type ctx node
           | Module_type -> super#module_type ctx node)
        | Error error ->
          Syntax_error_conversion.to_extension_node
            (Attributes.Context.mono_to_any attr_ctx)
            node
            error

    method! module_expr = self#visit_mono Module_expr
    method! core_type = self#visit_mono Core_type

    method! module_type ctx mty =
      let mty = self#visit_mono Module_type ctx mty in
      let mty_res =
        let* mty, with_ = Attributes.consume Attributes.with_ Module_type mty in
        match with_ with
        | Some with_ ->
          let with_ = self#signature ctx with_ in
          With_constraint.convert mty with_
        | None -> Ok mty
      in
      match mty_res with
      | Ok mty -> mty
      | Error err -> Syntax_error_conversion.to_extension_node Module_type mty err

    method! expression ctx expr =
      let loc = { expr.pexp_loc with loc_ghost = true } in
      let expr_res =
        let* expr, exclave_because_stack =
          Attributes.consume Attributes.exclave_if_stack Expression expr
        in
        let* expr, exclave_because_local =
          Attributes.consume Attributes.exclave_if_local Expression expr
        in
        let expr =
          match
            should_wrap_with_exclave
              expr
              ~exclave_because_stack
              ~exclave_because_local
              ~env:ctx.env
          with
          | Ok false -> expr
          | Ok true -> [%expr exclave_ [%e expr]]
          | Error error -> error
        in
        consume_zero_alloc_if Expression expr ~env:ctx.env
      in
      match expr_res with
      | Ok expr -> self#visit_mono Expression ctx expr
      | Error err -> Syntax_error_conversion.to_extension_node Expression expr err

    method! expression_desc ctx pexp_desc =
      let loc =
        content_span#expression_desc pexp_desc None |> Option.value ~default:Location.none
      in
      match Ppxlib_jane.Shim.Expression_desc.of_parsetree pexp_desc ~loc with
      | Pexp_let (mutable_flag, rec_flag, bindings, expr) ->
        self#visit_poly Value_binding ctx bindings ~f:(fun bindings ->
          let bindings = List.map bindings ~f:Maybe_hide_in_docs.node in
          Pexp_let
            ( self#mutable_flag ctx mutable_flag
            , self#rec_flag ctx rec_flag
            , bindings
            , self#expression ctx expr )
          |> Ppxlib_jane.Shim.Expression_desc.to_parsetree ~loc)
      | _ -> super#expression_desc ctx pexp_desc

    method! value_binding ctx vb =
      match consume_zero_alloc_if Value_binding vb ~env:ctx.env with
      | Ok vb -> super#value_binding ctx vb
      | Error err -> Syntax_error_conversion.to_extension_node Value_binding vb err

    method! value_description ctx vd =
      match consume_zero_alloc_if Value_description vd ~env:ctx.env with
      | Ok vd -> super#value_description ctx vd
      | Error err -> Syntax_error_conversion.to_extension_node Value_description vd err

    (* The [@@kind] attribute can appear on various nodes that define or declare values,
       modules, or types. For each such node, we determine what layout mappings are being
       requested (no attribute is equivalent to [@@kind _ = value]), and for each such
       mapping, we duplicate the definition/declaration, and mangle the defined/declared
       name accordingly. *)
    method
      private visit_poly
      : type a
      b.  a Attributes.Context.poly
         -> Context.t
         -> a list
         -> f:(a Maybe_hide_in_docs.t list -> b)
         -> b =
      fun attr_ctx ctx nodes ~f ->
        let ( (visit_self : Context.t -> a -> a)
            , (visit_mangle : Mangle.Suffix.t -> a -> a * Mangle.Result.t) )
          =
          match attr_ctx with
          | Value_binding -> self#value_binding, Mangle.t#value_binding
          | Value_description -> self#value_description, Mangle.t#value_description
          | Module_binding -> self#module_binding, Mangle.t#module_binding
          | Module_declaration -> self#module_declaration, Mangle.t#module_declaration
          | Type_declaration -> self#type_declaration, Mangle.t#type_declaration
          | Module_type_declaration ->
            self#module_type_declaration, Mangle.t#module_type_declaration
          | Include_infos ->
            let open Either in
            ( self#include_infos (fun ctx -> function
                | Left x -> Left (self#module_expr ctx x)
                | Right x -> Right (self#module_type ctx x))
            , Mangle.t#include_infos (fun ctx -> function
                | Left x ->
                  let x, res = Mangle.t#module_expr ctx x in
                  Left x, res
                | Right x ->
                  let x, res = Mangle.t#module_type ctx x in
                  Right x, res) )
        in
        (match consume_poly ~env:ctx.env ~defaults:ctx.defaults attr_ctx nodes with
         | Ok instances ->
           List.mapi
             instances
             ~f:(fun i { node; env; manglers } : _ Maybe_hide_in_docs.t ->
               let node, res = visit_mangle (Mangle.Suffix.create manglers) node in
               let node =
                 visit_self
                   { Context.ghostify = ctx.ghostify || i > 0
                   ; env
                   ; defaults = Context.Defaults.empty
                   }
                   node
               in
               { node; hide_in_docs = Mangle.Result.did_mangle res })
         | Error { loc = _; err; attrs = _ } ->
           let hd_nodes, tl_nodes =
             match nodes with
             | hd :: tl -> hd, tl
             | [] -> assert false
           in
           [ { node =
                 Syntax_error_conversion.to_extension_node
                   (Attributes.Context.poly_to_any attr_ctx)
                   hd_nodes
                   ~also_drop:tl_nodes
                   err
             ; hide_in_docs = true
             }
           ])
        |> f

    method
      private visit_floating_poly
      : type a.  a Attributes.Floating.Context.poly
                -> local_ (Context.t -> a list -> a list) =
      fun attr_ctx -> exclave_
        let (visit : Context.t -> a -> a), (wrap_include : loc:location -> a list -> a) =
          match attr_ctx with
          | Structure_item ->
            let include_ ~loc items =
              [%stri (** @inline *)
                     include [%m Ast_builder.pmod_structure ~loc items]]
            in
            self#structure_item, include_
          | Signature_item ->
            let include_ ~loc items =
              [%sigi:
                (** @inline *)
                include
                  [%m
                (* You might think we could [simplify_doc_toggling] here, but the items
                   are pre-[%template.inline] expansion. *)
                Ast_builder.pmty_signature ~loc (Ast_builder.signature ~loc items)]]
            in
            self#signature_item, include_
        in
        fun ctx nodes ->
          match nodes with
          | [] -> []
          | first_item :: rest_items ->
            let last_item = List.last rest_items |> Option.value ~default:first_item in
            let last_item_loc =
              match attr_ctx with
              | Structure_item -> last_item.pstr_loc
              | Signature_item -> last_item.psig_loc
            in
            let[@tail_mod_cons] rec loop ctx (nodes : a list) =
              match nodes with
              | [] -> []
              | item :: items ->
                (* [Ppxlib] complains if we give it something besides an attribute. *)
                (match attr_ctx, item with
                 | ( Structure_item
                   , { pstr_desc = Pstr_attribute _; pstr_loc = attr_loc; _ } )
                 | ( Signature_item
                   , { psig_desc = Psig_attribute _; psig_loc = attr_loc; _ } ) ->
                   (match extract_floating attr_ctx item with
                    | Ok None -> visit ctx item :: loop ctx items
                    | Ok (Some (Define (Define bindings))) ->
                      let loc = Attributes.Floating.Context.location attr_ctx item in
                      let original_env = ctx.env in
                      let new_env =
                        List.fold_left
                          bindings
                          ~init:(Ok original_env)
                          ~f:(fun env ({ pattern; expression; lookup } : _ Binding.t) ->
                            let* env = env in
                            let* set_value = Env.eval original_env lookup expression in
                            Env.bind_set env ~loc pattern set_value)
                      in
                      (match new_env with
                       | Ok env -> loop { ctx with env } items
                       | Error err ->
                         Syntax_error_conversion.to_extension_node_floating
                           attr_ctx
                           ~loc
                           err
                         :: loop ctx items)
                    | Ok (Some (Poly (explicitness, { bindings; kind }))) ->
                      (match instantiate [ explicitness, bindings ] ~env:ctx.env with
                       | Ok instances ->
                         let is_many = List.length instances > 1 in
                         List.mapi instances ~f:(fun i { node = (); env; manglers } ->
                           let defaults =
                             match kind with
                             | Never_add_mangler -> ctx.defaults
                             | Add_mangler_if_more_than_one_elt when not is_many ->
                               ctx.defaults
                             | Always_add_mangler | Add_mangler_if_more_than_one_elt ->
                               Axis.Map.merge
                                 (fun _ ctx_defaults new_defaults ->
                                   match ctx_defaults, new_defaults with
                                   | None, None -> None
                                   | Some defaults, None | None, Some defaults ->
                                     Some defaults
                                   | ( Some (_old_explicitness, ctx_defaults)
                                     , Some new_defaults ) ->
                                     Some
                                       (* Always use the explicit flag from the new
                                          attribute. *)
                                       (Maybe_explicit.map
                                          new_defaults
                                          ~f:(fun new_defaults ->
                                            ctx_defaults @ new_defaults)))
                                 ctx.defaults
                                 manglers
                           in
                           let ctx : Context.t =
                             { ghostify = ctx.ghostify || i > 0; env; defaults }
                           in
                           (* The location of the include statement spans from the start
                              of the attribute to the end of the last item in [items].
                              This ensures that the locations of every item is
                              well-nested, which helps Merlin. *)
                           wrap_include
                             ~loc:
                               { loc_start = attr_loc.loc_start
                               ; loc_end = last_item_loc.loc_end
                               ; loc_ghost = true
                               }
                             (loop ctx items))
                       | Error err ->
                         Syntax_error_conversion.to_extension_node_floating
                           attr_ctx
                           ~loc:attr_loc
                           err
                         :: loop ctx items)
                    | Error (loc, err) ->
                      Syntax_error_conversion.to_extension_node_floating attr_ctx ~loc err
                      :: loop ctx items)
                 | _ -> visit ctx item :: loop ctx items)
            in
            loop ctx nodes

    method! structure ctx items = self#visit_floating_poly Structure_item ctx items
    method! signature_items ctx items = self#visit_floating_poly Signature_item ctx items

    method! structure_item ctx =
      function
      | [%stri [%%template.portable [%%i? { pstr_desc = Pstr_module mod_; _ }]]] as stri
        ->
        Portable_stateless.module_binding Portable ~loc:stri.pstr_loc ~mod_
        |> self#structure_item ctx
      | [%stri [%%template.stateless [%%i? { pstr_desc = Pstr_module mod_; _ }]]] as stri
        ->
        Portable_stateless.module_binding Stateless ~loc:stri.pstr_loc ~mod_
        |> self#structure_item ctx
      | stri -> super#structure_item ctx stri

    method! structure_item_desc =
      let visit_poly attr_ctx ctx nodes ~f =
        self#visit_poly attr_ctx ctx nodes ~f:(fun nodes ->
          f (List.map nodes ~f:Maybe_hide_in_docs.node))
      in
      fun ctx -> function
        | Pstr_value (rec_flag, bindings) ->
          visit_poly Value_binding ctx bindings ~f:(fun bindings ->
            Pstr_value (self#rec_flag ctx rec_flag, bindings))
        | Pstr_primitive desc ->
          visit_poly Value_description ctx [ desc ] ~f:(function
            | [ desc ] -> Pstr_primitive desc
            | descs -> mk_struct ~loc:desc.pval_loc descs ~f:Ast_builder.pstr_primitive)
        | Pstr_type (rec_flag, decls) ->
          visit_poly Type_declaration ctx decls ~f:(fun decls ->
            Pstr_type (self#rec_flag ctx rec_flag, deduplicate_deriving_attrs decls))
        | Pstr_module binding ->
          visit_poly Module_binding ctx [ binding ] ~f:(function
            | [ binding ] -> Pstr_module binding
            | bindings ->
              mk_struct ~loc:binding.pmb_loc bindings ~f:Ast_builder.pstr_module)
        | Pstr_recmodule bindings ->
          visit_poly Module_binding ctx bindings ~f:(fun bindings ->
            Pstr_recmodule bindings)
        | Pstr_modtype decl ->
          visit_poly Module_type_declaration ctx [ decl ] ~f:(function
            | [ decl ] -> Pstr_modtype decl
            | decls -> mk_struct ~loc:decl.pmtd_loc decls ~f:Ast_builder.pstr_modtype)
        | Pstr_include info ->
          visit_poly
            Include_infos
            ctx
            [ { info with pincl_mod = Left info.pincl_mod } ]
            ~f:(fun (infos : _ Either.t include_infos list) ->
              List.map infos ~f:(fun info ->
                { info with
                  pincl_mod =
                    (match info.pincl_mod with
                     | Left x -> x
                     | Right _ -> assert false)
                })
              |> function
              | [ info ] -> Pstr_include info
              | infos -> mk_struct ~loc:info.pincl_loc infos ~f:Ast_builder.pstr_include)
        | desc ->
          (* We reset the defaults here since they are supposed to act "shallowly", i.e.
             they only apply to the current layer of structure items, not nested items. In
             particular, this avoids poor interaction with [let%expect_test] items. *)
          super#structure_item_desc { ctx with defaults = Context.Defaults.empty } desc

    method! signature_item ctx =
      function
      | [%sigi: [%%template.portable: [%%i? { psig_desc = Psig_module mod_; _ }]]] as sigi
        ->
        Portable_stateless.module_declaration Portable ~loc:sigi.psig_loc ~mod_
        |> self#signature_item ctx
      | [%sigi: [%%template.stateless: [%%i? { psig_desc = Psig_module mod_; _ }]]] as
        sigi ->
        Portable_stateless.module_declaration Stateless ~loc:sigi.psig_loc ~mod_
        |> self#signature_item ctx
      | sigi -> super#signature_item ctx sigi

    method! signature_item_desc =
      let visit_poly attr_ctx ctx item ~loc ~f =
        self#visit_poly attr_ctx ctx [ item ] ~f:(fun items ->
          let loc = { loc with loc_ghost = true } in
          items
          |> List.concat_map ~f:(fun { Maybe_hide_in_docs.node = desc; hide_in_docs } ->
            let sig_ = [ f ~loc desc ] in
            if hide_in_docs then Ppx_helpers.Docs.hide ~loc sig_ else sig_)
          |> Ppx_helpers.Docs.simplify
          |> function
          | [ { psig_desc; psig_loc = _ } ] -> psig_desc
          | sigis -> mk_sig ~loc sigis)
      in
      (* It is difficult to isolate individual types in e.g. a [type ... and ...] group.
         In particular, a [(**/**)] above the first item hides the entire group (and is
         not disabled by a [(**/**)] inside the group). For now, it's probably enough to
         hide the entire group if all items in it should be hidden, and to leave the
         entire group otherwise. *)
      let visit_poly_group attr_ctx ctx items ~loc ~f =
        self#visit_poly attr_ctx ctx items ~f:(fun items ->
          let all_hidden = List.for_all items ~f:Maybe_hide_in_docs.hide_in_docs in
          let nodes = List.map items ~f:(fun { node; hide_in_docs = _ } -> node) in
          let sigi = f nodes in
          let loc = { loc with loc_ghost = true } in
          if all_hidden
          then
            Ppx_helpers.Docs.hide ~loc [ { psig_desc = sigi; psig_loc = loc } ]
            |> mk_sig ~loc
          else sigi)
      in
      fun ctx desc ->
        match Ppxlib_jane.Shim.Signature_item_desc.of_parsetree desc with
        | Psig_value desc ->
          visit_poly
            Value_description
            ctx
            desc
            ~loc:desc.pval_loc
            ~f:Ast_builder.psig_value
        | Psig_type (rec_flag, decls) ->
          visit_poly_group
            Type_declaration
            ctx
            decls
            ~loc:(List.hd decls).ptype_loc
            ~f:(fun decls ->
              Psig_type (self#rec_flag ctx rec_flag, deduplicate_deriving_attrs decls))
        | Psig_typesubst decls ->
          visit_poly_group
            Type_declaration
            ctx
            decls
            ~loc:(List.hd decls).ptype_loc
            ~f:(fun decls -> Psig_typesubst decls)
        | Psig_module decl ->
          visit_poly
            Module_declaration
            ctx
            decl
            ~loc:decl.pmd_loc
            ~f:Ast_builder.psig_module
        | Psig_recmodule decls ->
          visit_poly_group
            Module_declaration
            ctx
            decls
            ~loc:(List.hd decls).pmd_loc
            ~f:(fun decls -> Psig_recmodule decls)
        | Psig_modtype decl ->
          visit_poly
            Module_type_declaration
            ctx
            decl
            ~loc:decl.pmtd_loc
            ~f:Ast_builder.psig_modtype
        | Psig_modtypesubst decl ->
          visit_poly
            Module_type_declaration
            ctx
            decl
            ~loc:decl.pmtd_loc
            ~f:Ast_builder.psig_modtypesubst
        | Psig_include (info, moda) ->
          visit_poly
            Include_infos
            ctx
            { info with pincl_mod = Right info.pincl_mod }
            ~loc:info.pincl_loc
            ~f:(fun ~loc info ->
              Ast_builder.psig_include
                ~loc
                { info with
                  pincl_mod =
                    (match info.pincl_mod with
                     | Left _ -> assert false
                     | Right x -> x)
                }
                ~modalities:(self#modalities ctx moda))
        | _ ->
          (* We reset the defaults here since they are supposed to act "shallowly", i.e.
             they only apply to the current layer of structure items, not nested items. *)
          super#signature_item_desc { ctx with defaults = Context.Defaults.empty } desc
  end
;;

let inline_struct ~loc nodes =
  Ast_builder.pstr_extension ~loc ({ txt = "template.inline"; loc }, PStr nodes) []
;;

let inline_sig ~loc nodes =
  Ast_builder.psig_extension
    ~loc
    ({ txt = "template.inline"; loc }, PSig (Ast_builder.signature ~loc nodes))
    []
;;

let t_inline = t ~mk_struct:inline_struct ~mk_sig:inline_sig

let include_struct ~loc items =
  Ast_builder.pstr_include
    ~loc
    (Ast_builder.include_infos
       ~loc
       ~kind:Structure
       (Ast_builder.pmod_structure ~loc items))
;;

let include_sig ~loc nodes =
  Ast_builder.psig_include
    ~loc
    ~modalities:[]
    (Ast_builder.include_infos
       ~loc
       ~kind:Structure
       (Ast_builder.pmty_signature ~loc (Ast_builder.signature ~loc nodes)))
;;

let t_no_inline = t ~mk_struct:include_struct ~mk_sig:include_sig
