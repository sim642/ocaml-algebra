module type Arg =
sig
    val m: int
end

module Make (Arg: Arg): Field.S with type t = int