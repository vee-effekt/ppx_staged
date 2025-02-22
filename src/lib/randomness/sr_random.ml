type t = Splittable_random.State.t

open Splittable_random


let int st ~lo ~hi = .< int .~st ~lo:.~lo ~hi:.~hi >.
let bool st = .< bool .~st >.
let float st ~lo ~hi = .< float .~st ~lo:.~lo ~hi:.~hi >.

let dep_paths = List.map Util.run_ocamlfind_query ["splittable_random";"base_quickcheck";"base"]

let of_sr x = x