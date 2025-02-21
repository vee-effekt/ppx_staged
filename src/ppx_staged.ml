open! Core
open! Ppxlib
open! Ast_builder.Default
open! StdLabels
open! Ppx_staged_expander
open Fast_gen.Staged_generator

let ppx_namespace = "ppx_staged"
let pp_quoted ppf s = Format.fprintf ppf "`%s`" s
let raise_errorf ~loc fmt = Location.raise_errorf ~loc ("%s: " ^^ fmt) ppx_namespace

let (_ : Deriving.t) = 
  Deriving.add "stage" 
    ~sig_type_decl
    ~str_type_decl

let (_ : Deriving.t) = 
  Deriving.add "stage.generator" ~extension:generator_extension
let (_ : Deriving.t) = 
  Deriving.add "stage.observer" ~extension:observer_extension
let (_ : Deriving.t) = 
  Deriving.add "stage.shrinker" ~extension:shrinker_extension

let () =
  List.iter ~f:Reserved_namespaces.reserve [ "stage" ]

(*
let constant_nat =
  Extension.declare
    "nat"
    Expression
    Ast_pattern.(
      (* suffix `n` is taken by nativeint *)
      single_expr_payload (pexp_constant (pconst_integer __ none)))
    (fun ~loc ~path:_ int ->
      let int = Int.of_string int in
      if int < 0
      then raise_errorf ~loc "invalid natural number %a." pp_quoted (Int.to_string int)
      else [%expr Ppx_staged_runtime.Nat.of_int_unsafe [%e eint ~loc int]])
;;

let () =
  Reserved_namespaces.reserve ppx_namespace;
  Driver.register_transformation ~extensions:[ constant_nat ] ppx_namespace
;;
*)

