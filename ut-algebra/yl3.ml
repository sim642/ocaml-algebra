open Lib.Common

module Frac (R: EuclideanRing) =
struct
    type t = R.t * R.t

    module REuclid = EuclideanAlgorithm (R)

    let create a b =
        (* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Simplification_of_fractions *)
        let (_, _, ss, _, tt) = REuclid.full_extended_gcd a b in
        (* TODO: canonical form: non-negative denom? *)
        R.(neg tt, ss)

    let to_string (a, b) = Printf.sprintf "%s/%s" (R.to_string a) (R.to_string b)

    let (+) (a, b) (c, d) = create R.(a * d + b * c) R.(b * d)
    let zero = create R.zero R.one
    let neg (a, b) = create (R.neg a) b
    let ( * ) (a, b) (c, d) = create R.(a * c) R.(b * d)
    let one = create R.one R.one
    let inv (a, b) = create b a
end

module Q = Frac (Z)
(* module Q =
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
end *)


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