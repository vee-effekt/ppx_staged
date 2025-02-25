open! Import

let custom_extension ~loc tag payload =
  match String.equal tag.txt "custom" with
  | false -> unsupported ~loc "unknown extension: %s" tag.txt
  | true ->
    (match payload with
     | PStr [ { pstr_desc = Pstr_eval (expr, attributes); _ } ] ->
       assert_no_attributes attributes;
       expr
     | _ -> invalid ~loc "[%%custom] extension expects a single expression as its payload")
;;

let generator_attribute =
  Attribute.declare
    "wh.generator"
    Attribute.Context.core_type
    Ast_pattern.(pstr (pstr_eval __ nil ^:: nil))
    (fun x -> x)
;;

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
     | Ptyp_tuple fields ->
       Ppx_generator_expander.compound
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~fields
         (module Field_syntax.Tuple)
     | Ptyp_variant (clauses, Closed, None) ->
       Ppx_generator_expander.variant
         ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
         ~loc
         ~variant_type:core_type
         ~clauses
         ~rec_names:(Set.empty (module String))
         (module Clause_syntax.Polymorphic_variant)
     | Ptyp_variant (_, Open, _) -> unsupported ~loc "polymorphic variant type with [>]"
     | Ptyp_variant (_, _, Some _) -> unsupported ~loc "polymorphic variant type with [<]"
     | Ptyp_extension (tag, payload) -> custom_extension ~loc tag payload
     | Ptyp_any
     | Ptyp_object _
     | Ptyp_class _
     | Ptyp_alias _
     | Ptyp_poly _
     | Ptyp_package _ -> unsupported ~loc "%s" (short_string_of_core_type core_type))

type impl =
  { loc : location
  ; typ : core_type
  ; pat : pattern
  ; var : expression
  ; exp : expression
  }

let generator_impl type_decl ~rec_names =
  let loc = type_decl.ptype_loc in
  let typ =
    combinator_type_of_type_declaration type_decl ~f:(fun ~loc ty ->
      [%type: [%t ty] Ppx_quickcheck_runtime.Base_quickcheck.Generator.t])
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
      | Ptype_variant clauses ->
        Ppx_generator_expander.variant
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~variant_type:[%type: _]
          ~clauses
          ~rec_names
          (module Clause_syntax.Variant)
      | Ptype_record fields ->
        Ppx_generator_expander.compound
          ~generator_of_core_type:(generator_of_core_type ~gen_env ~obs_env)
          ~loc
          ~fields
          (module Field_syntax.Record)
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

let generator_intf = intf ~f:generator_name ~covar:"Generator" ~contravar:"Observer"
let generator_intf_list type_decl_list = List.map type_decl_list ~f:generator_intf

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

let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (_, decls) ->
    match try_include_decl ~loc decls with
    | Some decl -> [ decl ]
    | None ->
      generator_intf_list decls)
;;

let str_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:_ (rec_flag, decls) ->
    let rec_flag = really_recursive rec_flag decls in
    generator_impl_list ~loc ~rec_flag decls)
;;

let generator_extension ~loc:_ ~path:_ core_type =
  generator_of_core_type core_type ~gen_env:Environment.empty ~obs_env:Environment.empty
;;
