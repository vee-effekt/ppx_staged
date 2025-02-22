open Codelib

module type S = sig
  type t

  val bool : t code -> bool code
  val int : t code -> lo:(int code) -> hi:(int code) -> int code
  val float : t code -> lo:(float code) -> hi:(float code) -> float code

  val of_sr : Splittable_random.State.t code -> t code

  val dep_paths : string list
end