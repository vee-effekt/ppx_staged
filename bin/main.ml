open Stdio
open Base
open Ppx_staged
open Ppx_staged_staging;;
open Ppx_staged_expander;;
(* open Base_quickcheck;; *)
open Modules;;

module Pair = struct 
  type t = bool * bool [@@deriving wh]
end

let () =
let generator = G_SR.jit (Pair.quickcheck_generator) in
let () = G_SR.print (Pair.quickcheck_generator) in
let random = Splittable_random.State.of_int 5 in
let size = 10 in
for _ = 1 to 10 do
  let (b,b') = Base_quickcheck.Generator.generate generator ~size ~random in
  printf "\n\n%s,%s" (Bool.to_string b) (Bool.to_string b')
done