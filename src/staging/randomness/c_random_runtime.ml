(*
The external functions need to be included in their own .ml file, so that there is a
*separate* cmi from the C_Random.ml that can be linked in at JIT-time.
Otherwise, MetaOCaml tries to serialize these external functions and gets confused.
*)

type t

external int_c_unchecked : t -> int -> int -> int = "int_c_unchecked"
external bool_c : t -> bool = "bool_c"
external float_c_unchecked : t -> float -> float -> float = "float_c_unchecked"
external create : int64 -> int64 -> t = "create_state"
