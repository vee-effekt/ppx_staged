type t = Splittable_random.State.t

let int st ~lo ~hi = .< if .~lo > .~ hi then failwith "Crossed bounds!" else C_sr_dropin_random_runtime.int_c_unchecked .~st .~lo .~hi >.
let bool st = .< C_sr_dropin_random_runtime.bool_c .~st >.

let float st ~(lo : float Codelib.code) ~hi = .<
  if .~lo > .~ hi then failwith "Crossed bounds!" else
  if (not (Float.is_finite .~lo && Float.is_finite .~hi)) then failwith "Infite floats" else
  C_sr_dropin_random_runtime.float_c_unchecked .~st .~lo .~hi
>.

(* THIS IS A HACK!
*)
let dep_paths = ["/home/ubuntu/waffle-house/staged-ocaml/_build/default/lib/.fast_gen.objs/byte/"]

let of_sr sr_t = sr_t