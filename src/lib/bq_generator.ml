type 'a t = 'a Base_quickcheck.Generator.t

module C = struct
  type 'a t = 'a
  let lift x = x
  let i2f = Float.of_int
  let pair x y = (x,y)
  let pred n = n - 1
  let cons x xs = x :: xs
end

type 'a c = 'a C.t

module R = Sr_random

module For_applicative = Base.Applicative.Make (struct
    type nonrec 'a t = 'a t

    let return = Base_quickcheck.Generator.return
    let apply = Base_quickcheck.Generator.apply
    let map = `Custom Base_quickcheck.Generator.map
  end)

(* let both = For_applicative.both
let map2 = For_applicative.map2
let map3 = For_applicative.map3 *)

include For_applicative
module Applicative_infix = For_applicative.Applicative_infix
include Applicative_infix

module For_monad = Base.Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = Base_quickcheck.Generator.return
    let bind = Base_quickcheck.Generator.bind
    let map = `Custom Base_quickcheck.Generator.map
  end)

(* let ignore_m = For_monad.ignore_m *)
(* let join = For_monad.join *)

include For_monad
(* module Monad_infix = For_monad.Monad_infix *)
include Monad_infix
(* module Let_syntax = For_monad.Let_syntax *)
(* open Let_syntax *)

let weighted_union = Base_quickcheck.Generator.weighted_union
let jit ?extra_cmi_paths:_ g = g
let union = Base_quickcheck.Generator.union
let of_list = Base_quickcheck.Generator.of_list
let of_list_dyn = Base_quickcheck.Generator.of_list
let int ~lo ~hi = Base_quickcheck.Generator.int_uniform_inclusive lo hi
let float ~lo ~hi = Base_quickcheck.Generator.float_uniform_exclusive lo hi
(* let float ~lo ~hi = Base_quickcheck.Generator.float_uniform_exclusive lo hi *)
let bool = Base_quickcheck.Generator.bool
let size = Base_quickcheck.Generator.size
let with_size g ~size_c = Base_quickcheck.Generator.with_size ~size:size_c g

type ('a,'r) recgen = 'r -> 'a t
let recurse f = f
let recursive (type a) (type r) (x0 : r ) (step : (a,r) recgen -> r -> a t) =
  let rec go x =
    step go x 
  in
  (go x0)

let to_bq x = x