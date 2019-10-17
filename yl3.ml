open Common

module type EuclideanRing =
sig
    include Ring
    val quot_rem: t -> t -> t * t
end

module EuclideanAlgorithm (R: EuclideanRing) =
struct
    open R

    (* https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#B%C3%A9zout's_identity_and_extended_GCD_algorithm *)
    let extended_gcd a b =
        let rec egcd r rr s ss t tt =
            if rr = zero then
                (r, s, t)
            else
                let (q, rrr) = quot_rem r rr in
                let sss = s + neg (q * ss) in
                let ttt = t + neg (q * tt) in
                egcd rr rrr ss sss tt ttt
        in
        egcd a b one zero zero one

    let gcd a b = let (d, _, _) = extended_gcd a b in d
    let lcm a b = fst @@ quot_rem (a * b) (gcd a b)
end


module ZEuclid = EuclideanAlgorithm (Z)

module Zn (Arg: sig val n: int end) =
struct
    type t = int
    let n = Arg.n

    let create a = snd @@ Z.quot_rem a n

    let to_string a = string_of_int a

    let (+) a b = create (a + b)
    let zero = 0
    let neg a = create (-a)
    let ( * ) a b = create (a * b)
    let one = 1
    let inv a =
        (* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers *)
        let (d, _, v) = ZEuclid.extended_gcd n a in
        assert (d = 1);
        v
end

module Q =
struct
    type t = int * int

    let create a b =
        let d = ZEuclid.gcd a b in
        (a / d, b / d)

    let to_string (a, b) = Printf.sprintf "%d/%d" a b    

    let (+) (a, b) (c, d) = create (a * d + b * c) (b * d)
    let zero = create 0 1
    let neg (a, b) = create (-a) b
    let ( * ) (a, b) (c, d) = create (a * c) (b * d)
    let one = create 1 1
    let inv (a, b) = create b a
end


let solve_euclid_task (type a) (module F: Field with type t = a) f g =
    let module PolF = Pol (F) in
    let module PolFEuclid = EuclideanAlgorithm (PolF) in
    Printf.printf "f kordajad: %s; g kordajad: %s\n" (PolF.to_string f) (PolF.to_string g);
    let (d, u, v) = PolFEuclid.extended_gcd f g in
    Printf.printf "  SÜT kordajad: %s\n" (PolF.to_string d);
    Printf.printf "  u kordajad: %s; v kordajad: %s\n" (PolF.to_string u) (PolF.to_string v);
    let m = PolFEuclid.lcm f g in
    Printf.printf "  VÜK kordajad: %s\n" (PolF.to_string m)


module Z7 = Zn (struct let n = 7 end)
let f7 = [-28; -50; 16; -6; 3; 2]
let g7 = [-14; -53; -28; 17; 6]
let () = solve_euclid_task (module Z7) f7 g7

let fq = [-28,1; -50,1; 16,1; -6,1; 3,1; 2,1]
let gq = [-14,1; -53,1; -28,1; 17,1; 6,1]
let () = solve_euclid_task (module Q) fq gq