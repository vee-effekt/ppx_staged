open Stdio
open Base
open Ppx_staged
open Base_quickcheck
open Ppx_staged_staging;;
open Modules;;

type variant = 
    Single of string 
  | Pair of string * string 
  [@@deriving wh]

let variant_to_string = function
  | Single s -> Printf.sprintf "Single %S" s
  | Pair (s1, s2) -> Printf.sprintf "Pair (%S, %S)" s1 s2

let () =
  let generator = quickcheck_generator_variant in
  let random = Splittable_random.State.create (Random.State.make [| 42 |]) in
  let size = 10 in
  for _ = 1 to 10 do
    let value = Generator.generate generator ~size ~random in
    printf "%s\n" (variant_to_string value)
  done