open Stdio
open Base
open Ppx_staged
open Ppx_staged_staging;;
open Ppx_staged_expander;;
(* open Base_quickcheck;; *)
open Modules;;

type staged_int = int [@wh.generator G_SR.int ~lo:.<0>. ~hi:.<100>.]
type staged_bool = bool [@wh.generator G_SR.bool]
type staged_float = float [@wh.generator G_SR.float ~lo:.<0.0>. ~hi:.<1.0>.]

module Pair = struct 
  type t = staged_bool * staged_bool [@@deriving wh, sexp]
end

let () =
let generator = Pair.quickcheck_generator in
let random = Splittable_random.State.create (Random.State.make [| 42 |]) in
let size = 10 in
for _ = 1 to 10 do
  let value = G_SR.jit generator in
  printf "%s\n" (Sexp.to_string_hum (Pair.sexp_of_t value))
done