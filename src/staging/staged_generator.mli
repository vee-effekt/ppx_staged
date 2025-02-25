module MakeStaged(R : Random_intf.S) : sig

    include Generator_intf.S with type 'a C.t = 'a Codelib.code with module R = R

    val print : 'a c t -> unit

    val split_bool : bool c -> bool t
    val split_list : 'a list c -> [`Nil | `Cons of 'a c * ('a list c)] t
    val split_option : 'a option c -> [`None | `Some of 'a c] t
    val split_pair : ('a * 'b) c -> ('a c * 'b c) t
    val split_triple : ('a * 'b * 'c) c -> ('a c * 'b c * 'c c) t

    module MakeSplit(X : Splittable.S) : sig
        val split : X.t c -> X.f t
    end

end

