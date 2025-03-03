open! Import
open! Ppx_staged_staging;;
open! Modules;;

let compound_generator_new ~loc (x, y) =
  [%expr
    G_SR.bind [%e x] ~f:(fun x' ->
      G_SR.bind [%e y] ~f:(fun y' ->
        G_SR.return (G_SR.C.pair x' y')
      )
    )
  ]
;;

let rec generator_of_core_type ~loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_tuple fields -> compound_generator_new ~loc ([%expr G_SR.bool], [%expr G_SR.bool])
  | _ -> failwith "Only compound (tuple) types a re supported."
;;

let rec generator_of_core_type ~loc core_type =
  match core_type.ptyp_desc with
  | Ptyp_tuple (t1 :: t2 :: _) -> 
      let gen_of_type ty =
        match ty.ptyp_desc with
        | Ptyp_constr ({ txt = Lident "bool"; _ }, _) -> [%expr G_SR.bool]
        | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> [%expr (G_SR.int ~lo:(G_SR.C.lift 0) ~hi:(G_SR.C.lift 100))]
        | Ptyp_constr ({ txt = Lident "float"; _ }, _) -> [%expr G_SR.float ~lo:(G_SR.C.lift 0.0) ~hi:(G_SR.C.lift 1.0)]
        | _ -> failwith "Unsupported type in tuple."
      in
      compound_generator_new ~loc (gen_of_type t1, gen_of_type t2)
  | _ -> failwith "Only compound (tuple) types with at least two elements are supported."
;;

type impl =
  { loc : location
  ; typ : core_type
  ; pat : pattern
  ; var : expression
  ; exp : expression
  }

let generator_impl type_decl =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] G_SR.c G_SR.t])
  in
  let pat = pgenerator type_decl.ptype_name in
  let var = egenerator type_decl.ptype_name in
  let exp =
    match type_decl.ptype_kind with
    | Ptype_record fields -> failwith "Unimpl"
    | Ptype_abstract ->
      (match type_decl.ptype_manifest with
       | Some core_type -> generator_of_core_type ~loc:loc core_type
       | None -> failwith "Abstract type without manifest is not supported")
    | _ -> failwith "Only tuple types are supported"
  in
  { loc; typ; pat; var; exp }
;;

let generator_intf type_decl =
  let loc = type_decl.ptype_loc in
  let name = loc_map type_decl.ptype_name ~f:generator_name in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] G_SR.c G_SR.t])
  in
  psig_value ~loc (value_description ~loc ~name ~type_:typ ~prim:[])
;;

let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_, decls) ->
    List.map decls ~f:generator_intf)
;;

let str_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_, [decl]) ->
    let impl = generator_impl decl in
    [ pstr_value ~loc Nonrecursive
        [ value_binding ~loc:impl.loc ~pat:impl.pat ~expr:impl.exp ] ])
;;

(*
let compound_generator_new generator_list =
  match generator_list with
  | [x; y] -> G_SR.bind x ~f:(fun x' ->
      G_SR.bind y ~f:(fun y' ->
        G_SR.return (G_SR.C.pair x' y')
      )
    )
  | _ -> failwith ""
;;

let compound_new
      (type field)
      ~generator_of_core_type
      ~fields
      (module Field : Field_syntax.S with type ast = field)
  =
  let fields = List.map fields ~f:Field.create in 
  compound_generator_new
      (List.map fields ~f:(fun field -> G_SR.bool))

let generator_attribute =
  Attribute.declare
    "wh.generator"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

let rec generator_of_core_type core_type ~gen_env ~obs_env =
  match core_type.ptyp_desc with
     | Ptyp_tuple fields -> compound_new
        ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
        ~fields
        (module Field_syntax.Tuple)
     | _ -> failwith "Only compound types are supported at this time."

(*
let rec generator_of_core_type core_type ~gen_env ~obs_env =
  let loc = { core_type.ptyp_loc with loc_ghost = true } in
  match Attribute.get generator_attribute core_type with
  | Some expr -> expr
  | None ->
    (match core_type.ptyp_desc with
     | Ptyp_constr (constr, args) ->
       type_constr_conv
         ~loc
         ~f:generator_name
         constr
         (List.map args ~f:(generator_of_core_type ~gen_env ~obs_env))
     | Ptyp_var tyvar -> Environment.lookup gen_env ~loc ~tyvar
     | Ptyp_arrow (arg_label, input_type, output_type) -> unsupported ~loc "Arrow types are not supported, %s" (short_string_of_core_type core_type)
     | Ptyp_tuple fields -> compound_new
        ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
        ~fields
        (module Field_syntax.Tuple)
     (*
       Ppx_generator_expander.compound
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~fields
         (module Field_syntax.Tuple)
      *)
     | Ptyp_variant (clauses, Closed, None) -> unsupported ~loc "Variant types are not supported, %s" (short_string_of_core_type core_type)
      (*
       Ppx_generator_expander.variant
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~variant_type:core_type
         ~clauses
         ~rec_names:(Set.empty (module String))
         (module Clause_syntax.Polymorphic_variant)
      *)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | Ptyp_any
     | Ptyp_object _
     | Ptyp_class _
     | Ptyp_alias _
     | Ptyp_poly _
     | Ptyp_package _ -> unsupported ~loc "%s" (short_string_of_core_type core_type))
*)

type impl =
  { loc : location
  ; typ : core_type
  ; pat : pattern
  ; var : expression
  ; exp : core_type G_SR.c G_SR.t
  }

let generator_impl type_decl ~rec_names =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] G_SR.c G_SR.t])
  in
  let pat = pgenerator type_decl.ptype_name in
  let var = egenerator type_decl.ptype_name in
  let exp =
    let pat_list, `Covariant gen_env, `Contravariant obs_env =
      Environment.create_with_variance
        ~loc
        ~covariant:"generator"
        ~contravariant:"observer"
        type_decl.ptype_params
    in
    let body =
      match type_decl.ptype_kind with
      | Ptype_open -> unsupported ~loc "open type"
      | Ptype_variant clauses -> unsupported ~loc "variant type"
        (*
        Ppx_generator_expander.variant
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          ~rec_names
          (module Clause_syntax.Variant)
        *)
      | Ptype_record fields -> unsupported ~loc "field type"
        (*
        Ppx_generator_expander.compound
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~fields
          (module Field_syntax.Record)
        *)
      | Ptype_abstract ->
        (match type_decl.ptype_manifest with
         | Some core_type -> generator_of_core_type core_type ~gen_env ~obs_env
         | None -> unsupported ~loc "abstract type")
    in
    List.fold_right pat_list ~init:body ~f:(fun pat body ->
      [%expr fun [%p pat] -> [%e body]])
  in
  { loc; typ; pat; var; exp }
;;

let close_the_loop ~of_lazy decl impl =
  let loc = impl.loc in
  let exp = impl.var in
  match decl.ptype_params with
  | [] -> eapply ~loc of_lazy [ exp ]
  | params ->
    let pats, exps =
      gensyms "recur" (List.map params ~f:(fun (core_type, _) -> core_type.ptyp_loc))
    in
    eabstract
      ~loc
      pats
      (eapply
         ~loc
         of_lazy
         [ [%expr
           lazy
             [%e
               eapply
                 ~loc
                 (eapply ~loc [%expr Ppx_quickcheck_runtime.Base.Lazy.force] [ exp ])
                 exps]]
         ])
;;

let maybe_mutually_recursive decls ~loc ~rec_flag ~of_lazy ~impl =
  let decls = List.map decls ~f:name_type_params_in_td in
  let rec_names =
    match rec_flag with
    | Nonrecursive -> Set.empty (module String)
    | Recursive ->
      Set.of_list (module String) (List.map decls ~f:(fun decl -> decl.ptype_name.txt))
  in
  let impls = List.map decls ~f:(fun decl -> impl decl ~rec_names) in
  match rec_flag with
  | Nonrecursive ->
    pstr_value_list
      ~loc
      Nonrecursive
      (List.map impls ~f:(fun impl ->
         value_binding ~loc:impl.loc ~pat:impl.pat ~expr:impl.exp))
  | Recursive ->
    let recursive_bindings =
      let inner_bindings =
        List.map2_exn decls impls ~f:(fun decl inner ->
          value_binding
            ~loc:inner.loc
            ~pat:inner.pat
            ~expr:(close_the_loop ~of_lazy decl inner))
      in
      let rec wrap exp =
        match exp.pexp_desc with
        | Pexp_fun (arg_label, default, pat, body) ->
          { exp with pexp_desc = Pexp_fun (arg_label, default, pat, wrap body) }
        | _ ->
          List.fold impls ~init:exp ~f:(fun acc impl ->
            let ign = [%expr ignore [%e impl.var]] in
            pexp_sequence ~loc ign acc)
          |> pexp_let ~loc Nonrecursive inner_bindings
      in
      List.map2_exn decls impls ~f:(fun decl impl ->
        let body = wrap impl.exp in
        let lazy_expr = [%expr lazy [%e body]] in
        let typed_pat =
          [%type: [%t impl.typ] Ppx_quickcheck_runtime.Base.Lazy.t]
          |> ptyp_poly ~loc (List.map decl.ptype_params ~f:get_type_param_name)
          |> ppat_constraint ~loc impl.pat
        in
        value_binding ~loc:impl.loc ~pat:typed_pat ~expr:lazy_expr)
    in
    [%str
      include struct
        open [%m pmod_structure ~loc (pstr_value_list ~loc Recursive recursive_bindings)]

        [%%i
          pstr_value
            ~loc
            Nonrecursive
            (List.map2_exn decls impls ~f:(fun decl impl ->
               value_binding ~loc ~pat:impl.pat ~expr:(close_the_loop ~of_lazy decl impl)))]
      end]
;;

let generator_impl_list decls ~loc ~rec_flag =
  maybe_mutually_recursive
    decls
    ~loc
    ~rec_flag
    ~of_lazy:[%expr Ppx_quickcheck_runtime.Base_quickcheck.Generator.of_lazy]
    ~impl:generator_impl
;;

let intf type_decl ~f ~covar ~contravar =
  let covar =
    Longident.parse ("Ppx_quickcheck_runtime.Base_quickcheck." ^ covar ^ ".t")
  in
  let contravar =
    Longident.parse ("Ppx_quickcheck_runtime.Base_quickcheck." ^ contravar ^ ".t")
  in
  let type_decl = name_type_params_in_td type_decl in
  let loc = type_decl.ptype_loc in
  let name = loc_map type_decl.ptype_name ~f in
  let result =
    ptyp_constr
      ~loc
      { loc; txt = covar }
      [ ptyp_constr
          ~loc
          (lident_loc type_decl.ptype_name)
          (List.map type_decl.ptype_params ~f:fst)
      ]
  in
  let type_ =
    List.fold_right
      type_decl.ptype_params
      ~init:result
      ~f:(fun (core_type, (variance, _)) result ->
        let id =
          match variance with
          | NoVariance | Covariant -> covar
          | Contravariant -> contravar
        in
        let arg = ptyp_constr ~loc { loc; txt = id } [ core_type ] in
        [%type: [%t arg] -> [%t result]])
  in
  psig_value ~loc (value_description ~loc ~name ~type_ ~prim:[])
;;

(*
let generator_intf = intf ~f:generator_name ~covar:"Generator" ~contravar:"Observer"
let generator_intf_list type_decl_list = List.map type_decl_list ~f:generator_intf
*)

let generator_intf type_decl =
  let loc = type_decl.ptype_loc in
  let name = loc_map type_decl.ptype_name ~f:generator_name in

  let bool_type = Ast_helper.Typ.constr ~loc { txt = Longident.Lident "bool"; loc } [] in
  let c_type = Ast_helper.Typ.constr ~loc { txt = Longident.Lident "G_SR.c"; loc } [bool_type] in
  let t_type = Ast_helper.Typ.constr ~loc { txt = Longident.Lident "G_SR.t"; loc } [c_type] in

  psig_value ~loc (value_description ~loc ~name ~type_:t_type ~prim:[])
;;

let try_include_decl type_decl_list ~loc =
  match type_decl_list with
  | [ type_decl ] ->
    let has_contravariant_arg =
      List.exists type_decl.ptype_params ~f:(fun (_type, (variance, _inj)) ->
        match variance with
        | Contravariant -> true
        | NoVariance | Covariant -> false)
    in
    if has_contravariant_arg
    then None
    else (
      let sg_name = "Ppx_quickcheck_runtime.Quickcheckable.S" in
      mk_named_sig ~loc ~sg_name ~handle_polymorphic_variant:true type_decl_list
      |> Option.map ~f:(fun include_info -> psig_include ~loc include_info))
  | _ ->
    (* Don't bother testing anything since [mk_named_sig] will definitely return
       [None] anyway *)
    None
;;

(*
let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_, decls) ->
    match try_include_decl ~loc decls with
    | Some decl -> [ decl ]
    | None ->
      generator_intf_list decls)
;;
*)

let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_, decls) ->
    List.map decls ~f:generator_intf)
;;

let str_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    generator_impl_list ~loc ~rec_flag decls)
;;
*)