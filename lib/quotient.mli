module type Arg =
sig
    module R: Euclidean.S
    val m: R.t
end

module Make (Arg: Arg): Field.S with type t = Arg.R.t