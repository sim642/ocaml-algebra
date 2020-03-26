open Lib.Common

let solve_sub_task () =
    let p = read_int () in
    let module F = Zn (struct let n = p end) in
    let module PolF = Pol (F) in

    let module PolFIO = IntPolIO (PolF) in
    let a = PolFIO.read_pol () in
    let b = PolFIO.read_pol () in

    let c = PolF.(a + neg b) in
    PolFIO.print_pol c

let () = solve_sub_task ()