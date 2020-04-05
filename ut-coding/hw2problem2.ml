open Lib

let solve_euclid_task (type t) (module F: Field.S with type t = t) a b =
    let module PolF = Polynomial.Make (F) in
    let module PolFEuclid = Euclidean.Algorithm (PolF) in

    Printf.printf "a: %s; b: %s\n" (PolF.to_string a) (PolF.to_string b);
    let (d, s, t) = PolFEuclid.extended_gcd a b in
    Printf.printf "  gcd: %s\n" (PolF.to_string d);
    Printf.printf "  s: %s; t: %s\n" (PolF.to_string s) (PolF.to_string t)

let () =
    let module F = IntegerModulo.Make (struct let m = 2 end) in
    let a = [0; 0; 1; 0; 1] in
    let b = [0; 1; 0; 0; 1] in
    solve_euclid_task (module F) a b


let () =
    let module F = IntegerModulo.Make (struct let m = 5 end) in
    let a = [2; 4; 1; 1] in
    let b = [0; 1; 0; 1] in
    solve_euclid_task (module F) a b
