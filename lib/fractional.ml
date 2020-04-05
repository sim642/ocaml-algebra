module Make (R: Euclidean.S) =
struct
    type t = R.t * R.t

    module REuclid = Euclidean.Algorithm (R)

    (* let create a b =
        (* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Simplification_of_fractions *)
        let (_, _, ss, _, tt) = REuclid.full_extended_gcd a b in
        (* TODO: canonical form: non-negative denom? *)
        R.(neg tt, ss) *)
    let create a b =
        let d = REuclid.gcd a b in
        (fst @@ R.quot_rem a d, fst @@ R.quot_rem b d)

    let to_string (a, b) = Printf.sprintf "%s/%s" (R.to_string a) (R.to_string b)

    let (+) (a, b) (c, d) = create R.(a * d + b * c) R.(b * d)
    let zero = create R.zero R.one
    let neg (a, b) = create (R.neg a) b
    let ( * ) (a, b) (c, d) = create R.(a * c) R.(b * d)
    let one = create R.one R.one
    let inv (a, b) = create b a
end

module Integer = Make (Integer)