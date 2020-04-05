module type Arg =
sig
    val m: int
end

module Make (Arg: Arg):
    Field.S with type t = int =
struct
    module QuotientArg =
    struct
        module R = Integer
        include Arg
    end

    include Quotient.Make (QuotientArg)
end