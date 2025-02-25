module type C_INTF = sig
  type 'a t
  val lift : 'a -> 'a t
  val i2f : Int.t t -> Float.t t
  val pair : 'a t -> 'b t -> ('a * 'b) t
  val pred : Int.t t -> Int.t t
  val cons : 'a t -> 'a list t -> 'a list t
end
module type S = sig
  type 'a t
  module C : C_INTF
  (* A generator module also includes its particular randomness source (splittable_random or unboxed)*)
  module R : Random_intf.S
  type 'a c = 'a C.t

  include Base.Applicative.S with type 'a t := 'a t
  include Base.Monad.S with type 'a t := 'a t


  val jit : ?extra_cmi_paths:string list -> 'a c t -> 'a Base_quickcheck.Generator.t

  val int : lo:(int c) -> hi:(int c) -> int c t
  val float : lo:(float c) -> hi:(float c) -> float c t
  val bool : bool c t

  val weighted_union : (float c * 'a c t) list -> 'a c t
  val union : 'a t list -> 'a t
  val of_list : 'a list -> 'a t
  val of_list_dyn : 'a list c -> 'a c t

  val size : int c t
  val with_size : 'a t -> size:(int c) -> 'a t

  (* val to_fun : 'a c t -> (size:int -> random:R.t -> 'a) c *)
  val to_bq : 'a c t -> ('a Base_quickcheck.Generator.t) c

  type ('a,'r) recgen
  val recurse : ('a,'r) recgen -> 'r c -> 'a c t
  val recursive : 'r c -> (('a,'r) recgen -> 'r c -> 'a c t) -> 'a c t

end