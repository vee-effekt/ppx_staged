open Codelib;;

(* This is stolen from andras kovacs *)
type 'a t = {code_gen : 'z. (('a -> 'z code) -> 'z code)}

let v2c (vc : 'a val_code) = (vc : 'a val_code :> 'a code)

let run_code_gen {code_gen=f} k = f k

let code_generate : ('a code) t -> 'a code =
  fun g -> g.code_gen (fun x -> x)

let return x = { code_gen = fun k -> k x}
let bind ({code_gen = inC } : 'a t) (f : 'a -> 'b t) : 'b t = { code_gen = fun k -> inC (fun a -> run_code_gen (f a) k) }

(* module For_monad = Monad.Make (struct
    type nonrec 'a t = 'a t

    let return = return
    let bind = bind
    let map = `Define_using_bind
  end) *)

(* module Monad_infix = For_monad.Monad_infix
include Monad_infix
module Let_syntax = For_monad.Let_syntax

let all = For_monad.all *)

let rec all xs =
  match xs with
  | [] -> return []
  | cx :: cxs ->
      bind cx @@ fun x ->
        bind (all cxs) @@ fun xs ->
          return (x :: xs)

let let_insert (cx : 'a code) : 'a code t = {
  code_gen = fun k -> letl cx k
}

let let_insert_smart (cx : 'a code) : 'a code t = {
  code_gen = fun k -> k (genlet cx)
}

let let_insertv (cx : 'a code) : 'a val_code t = {
  code_gen = fun k -> letlv cx k
}


let split_bool (cb : bool code) : bool t = {
  code_gen = fun k ->
    Codelib.letl cb (fun bvc ->
    .<
      if .~bvc then .~(k true) else .~(k false)
    >.
    )
}

let split_option (cb : ('a option) code) : [`None | `Some of 'a code] t = {
  code_gen = fun k ->
    Codelib.letl cb (fun bvc ->
    .<
      match .~bvc with
      | None -> .~(k `None)
      | Some x -> .~(k (`Some (.<x>.)))
    >.
    )
}

let split_pair (cp : ('a * 'b) code) : ('a code *'b code) t = {
  code_gen = fun k ->
    .<
      let (a,b) = .~(Codelib.genlet cp) in .~(k (.<a>.,.<b>.))
    >.
}

let split_triple (ct : ('a * 'b * 'c) code) : ('a code *'b code * 'c code) t = {
  code_gen = fun k ->
    .<
      let (a,b,c) = .~(Codelib.genlet ct) in .~(k (.<a>.,.<b>.,.<c>.))
    >.
}

let split_list cxs = {
  code_gen = fun k ->
    .<
      match .~cxs with
      | [] -> .~(k `Nil)
      | x::xs -> .~(k (`Cons (.<x>.,.<xs>.)))
    >.
}