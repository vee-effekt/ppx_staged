open Stdio
open Base
open Ppx_staged
open Base_quickcheck

type tree =
  | Leaf of int
  | Node of tree * int * tree
  [@@deriving wh]

let () = Stdio.print_endline "Hello World!"
