open Lib

let solve_div_task () =
    let p = read_int () in
    let module F = IntegerModulo.Make (struct let m = p end) in
    let module PolF = Polynomial.Make (F) in

    let module PolFIO = Polynomial.IntIO (PolF) in
    let a = PolFIO.read_pol () in
    let b = PolFIO.read_pol () in

    let (q, r) = PolF.quot_rem a b in
    PolFIO.print_pol q;
    PolFIO.print_pol r

let () = solve_div_task ()