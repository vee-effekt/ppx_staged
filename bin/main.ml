open Stdio
open Base
open Ppx_staged
open Ppx_staged_staging;;
open Ppx_staged_expander;;
(* open Base_quickcheck;; *)
open Modules;;


let quickcheck_generator_int = Modules.G_SR.int
let quickcheck_generator_bool = Modules.G_SR.bool
let quickcheck_generator_float = Modules.G_SR.float

module Pair = struct 
  type t = bool * bool [@@deriving wh, sexp]
end

let () =
let generator = Pair.quickcheck_generator in
let random = Splittable_random.State.create (Random.State.make [| 42 |]) in
let size = 10 in
for _ = 1 to 10 do
  let value = Generator.generate generator ~size ~random in
  printf "%s\n" (Sexp.to_string_hum (Pair.sexp_of_t value))
done