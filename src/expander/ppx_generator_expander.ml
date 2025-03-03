open! Import
open! Ppx_staged_staging;;
open! Modules;;
open! Codelib;;


let compound_generator ~loc ~make_compound_expr generator_list =
  let loc = { loc with loc_ghost = true } in
  let size_pat, size_expr = gensym "size" loc in
  let random_pat, random_expr = gensym "random" loc in
  [%expr
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.create
      (fun ~size:_ ~random:[%p random_pat] ->
         [%e
           make_compound_expr
             ~loc
             (List.map generator_list ~f:(fun generator ->
                let loc = { generator.pexp_loc with loc_ghost = true } in
                [%expr
                  Ppx_quickcheck_runtime.Base_quickcheck.Generator.generate
                    [%e generator]
                    ~size:0
                    ~random:[%e random_expr]]))
                    ])]
;;

let compound
      (type field)
      ~generator_of_core_type
      ~loc
      ~fields
      (module Field : Field_syntax.S with type ast = field)
  =
  let fields = List.map fields ~f:Field.create in
  compound_generator
    ~loc
    ~make_compound_expr:(Field.expression fields)
    (List.map fields ~f:(fun field -> generator_of_core_type (Field.core_type field)))
;;


let does_refer_to name_set =
  object (self)
    inherit [bool] Ast_traverse.fold as super

    method! core_type ty acc =
      match ty.ptyp_desc with
      | Ptyp_constr (name, args) ->
        acc
        || Set.mem name_set (Longident.name name.txt)
        || List.exists args ~f:(fun arg -> self#core_type arg false)
      | _ -> super#core_type ty acc
  end
;;

let clause_is_recursive
      (type clause)
      ~clause
      ~rec_names
      (module Clause : Clause_syntax.S with type t = clause)
  =
  List.exists (Clause.core_type_list clause) ~f:(fun ty ->
    (does_refer_to rec_names)#core_type ty false)
;;

let variant
      (type clause)
      ~generator_of_core_type
      ~loc
      ~variant_type
      ~clauses
      ~rec_names
      (module Clause : Clause_syntax.S with type ast = clause)
  =
  let clauses = Clause.create_list clauses in
  let make_generator clause =
    compound_generator
      ~loc:(Clause.location clause)
      ~make_compound_expr:(Clause.expression clause variant_type)
      (List.map (Clause.core_type_list clause) ~f:generator_of_core_type)
  in
  let make_pair clause =
    Option.map (Clause.weight clause) ~f:(fun weight ->
      pexp_tuple
        ~loc:{ (Clause.location clause) with loc_ghost = true }
        [ weight; make_generator clause ])
  in
  (* We filter out clauses with weight None now to avoid generating bindings that won't be used. *)
  let clauses =
    List.filter clauses ~f:(fun clause -> Option.is_some (Clause.weight clause))
  in
  let pairs = List.filter_map clauses ~f:make_pair in
  [%expr
    Ppx_quickcheck_runtime.Base_quickcheck.Generator.weighted_union
      [%e elist ~loc pairs]]
;;
