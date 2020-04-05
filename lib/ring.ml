module type S =
sig
    type t
    val to_string: t -> string

    val (+): t -> t -> t
    val zero: t
    val neg: t -> t
    val ( * ): t -> t -> t
    val one: t
end
