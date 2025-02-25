open Stdio
open Base
open Ppx_staged
open Base_quickcheck
open Ppx_staged_staging;;
open Modules;;

type variant = 
    Single of string 
  | Pair of string * string [@@deriving wh]

let () =
  let gen = quickcheck_generator_variant in
  Stdio.print_endline "Derived generator for int is ready!"