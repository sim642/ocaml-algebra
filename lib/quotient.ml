module type Arg =
sig
    module R: Euclidean.S
    val m: R.t
end

module Make (Arg: Arg):
    Field.S with type t = Arg.R.t =
struct
    include Arg
    module REuclid = Euclidean.Algorithm (R)

    type t = R.t

    let create a = snd @@ R.quot_rem a m

    let to_string a = R.to_string a

    let (+) a b = create R.(a + b)
    let zero = R.zero
    let neg a = create R.(neg a)
    let ( * ) a b = create R.(a * b)
    let one = R.one
    let inv a =
        (* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers *)
        let (d, _, v) = REuclid.extended_gcd m a in
        assert (d = one);
        create v
end