open Lib.Common

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