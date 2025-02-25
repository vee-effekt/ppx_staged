open Stdio
open Base
open Ppx_staged
open Base_quickcheck
open Ppx_staged_staging;;
open Modules;;

type tree =
  | Leaf of int
  | Node of tree * int * tree
  [@@deriving wh]

let () = Stdio.print_endline "Hello World!"
