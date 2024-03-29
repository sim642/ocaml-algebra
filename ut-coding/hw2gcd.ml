open Lib

let rec last = function
    | [] -> failwith "last: empty list"
    | [x] -> x
    | _ :: xs -> last xs

let solve_gcd_task () =
    let p = read_int () in
    let module F = IntegerModulo.Make (struct let m = p end) in
    let module PolF = Polynomial.Make (F) in

    let module PolFIO = Polynomial.IntIO (PolF) in
    let a = PolFIO.read_pol () in
    let b = PolFIO.read_pol () in

    let module PolFEuclid = Euclidean.Algorithm (PolF) in
    let d = PolFEuclid.gcd a b in
    let monic_d = PolF.([F.inv (last d)] * d) in
    PolFIO.print_pol monic_d

let () = solve_gcd_task ()