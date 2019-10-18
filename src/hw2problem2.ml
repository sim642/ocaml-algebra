open Common

let solve_euclid_task (type t) (module F: Field with type t = t) a b =
    let module PolF = Pol (F) in
    let module PolFEuclid = EuclideanAlgorithm (PolF) in

    Printf.printf "a: %s; b: %s\n" (PolF.to_string a) (PolF.to_string b);
    let (d, s, t) = PolFEuclid.extended_gcd a b in
    Printf.printf "  gcd: %s\n" (PolF.to_string d);
    Printf.printf "  s: %s; t: %s\n" (PolF.to_string s) (PolF.to_string t)

module Z2 = Zn (struct let n = 2 end)
let a1 = [0; 0; 1; 0; 1]
let b1 = [0; 1; 0; 0; 1]
let () = solve_euclid_task (module Z2) a1 b1

module Z5 = Zn (struct let n = 5 end)
let a2 = [2; 4; 1; 1]
let b2 = [0; 1; 0; 1]
let () = solve_euclid_task (module Z5) a2 b2
