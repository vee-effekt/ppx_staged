(* staged_generator.mli *)

(** A module for generating staged code computations. *)

open Codelib

(** The type of a staged computation that produces code.  
    For any result type ['z], if you provide a continuation 
    that takes an ['a] and returns a ['z code], the field [code_gen] 
    produces a ['z code]. *)
type 'a t = { code_gen : 'z. (('a -> 'z code) -> 'z code) }

(** [v2c vc] converts a value code to a code value. *)
val v2c : 'a val_code -> 'a code

(** [run_code_gen t k] runs the code generator [t] with the continuation [k]. *)
val run_code_gen : 'a t -> ('a -> 'b code) -> 'b code

(** [code_generate t] executes a code generator (whose result is itself code)
    and returns the resulting code value. *)
val code_generate : ('a code) t -> 'a code

(** [return x] is a code generator that simply returns [x]. *)
val return : 'a -> 'a t

(** [bind t f] sequentially composes the code generator [t] with [f]. *)
val bind : 'a t -> ('a -> 'b t) -> 'b t

(** [all xs] runs a list of code generators [xs] and collects their results 
    into a list. *)
val all : 'a t list -> 'a list t

(** [let_insert cx] performs a let-insertion of the code [cx]. *)
val let_insert : 'a code -> 'a code t

(** [let_insert_smart cx] performs a let-insertion of the code [cx] *)
val let_insert_smart : 'a code -> 'a code t

(** [let_insertv cx] performs a let-insertion for a value-code [cx]. *)
val let_insertv : 'a code -> 'a val_code t

(** [split_bool cb] takes a boolean code [cb] and returns a code generator 
    that branches on its value. *)
val split_bool : bool code -> bool t

(** [split_option cx] takes an option code [cx] and returns a code generator 
    that produces either [`None] or [`Some of 'a code]. *)
val split_option : 'a option code -> [ `None | `Some of 'a code ] t

(** [split_pair cp] takes a code for a pair and returns a code generator 
    that produces a pair of code values. *)
val split_pair : ('a * 'b) code -> ('a code * 'b code) t

(** [split_triple ct] takes a code for a triple and returns a code generator 
    that produces a triple of code values. *)
val split_triple : ('a * 'b * 'c) code -> ('a code * 'b code * 'c code) t

(** [split_list cxs] takes a code for a list and returns a code generator that 
    produces either [`Nil] or [`Cons of head code * tail list code]. *)
val split_list : 'a list code -> [ `Nil | `Cons of 'a code * ('a list code) ] t
