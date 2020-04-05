module type S =
sig
    include Ring.S

    val quot_rem: t -> t -> t * t
end

module Algorithm (R: S) =
struct
    open R

    (* https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#B%C3%A9zout's_identity_and_extended_GCD_algorithm *)
    let full_extended_gcd a b =
        let rec egcd r rr s ss t tt =
            if rr = zero then
                (r, s, ss, t, tt)
            else
                let (q, rrr) = quot_rem r rr in
                let sss = s + neg (q * ss) in
                let ttt = t + neg (q * tt) in
                egcd rr rrr ss sss tt ttt
        in
        egcd a b one zero zero one

    let extended_gcd a b =
        let (r, s, _, t, _) = full_extended_gcd a b in
        (r, s, t)

    let gcd a b = let (d, _, _) = extended_gcd a b in d
    let lcm a b = fst @@ quot_rem (a * b) (gcd a b)
end