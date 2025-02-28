open Base_quickcheck
open! Ppx_staged_staging;;
open! Modules;;

module type S = sig
  type t

  val quickcheck_generator : t G_SR.c G_SR.t
end

module type S1 = sig
  type 'a t

  val quickcheck_generator : 'a Generator.t -> 'a t Generator.t
end

module type S2 = sig
  type ('a, 'b) t

  val quickcheck_generator : 'a Generator.t -> 'b Generator.t -> ('a, 'b) t Generator.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val quickcheck_generator
    :  'a Generator.t
    -> 'b Generator.t
    -> 'c Generator.t
    -> ('a, 'b, 'c) t Generator.t

  val quickcheck_observer
    :  'a Observer.t
    -> 'b Observer.t
    -> 'c Observer.t
    -> ('a, 'b, 'c) t Observer.t

  val quickcheck_shrinker
    :  'a Shrinker.t
    -> 'b Shrinker.t
    -> 'c Shrinker.t
    -> ('a, 'b, 'c) t Shrinker.t
end
