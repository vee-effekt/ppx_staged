open Stdio
open Base
open Ppx_staged
open Base_quickcheck

type tree =
  | Leaf of int
  | Node of int
  | Name of string
  | List of int * tree
[@@deriving stage]

let () = Stdio.print_endline "Hello World!"
