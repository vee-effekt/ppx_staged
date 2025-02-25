open Codelib;;

module type S = sig
  type t
  type f

  val split : t code -> f Codecps.t
end