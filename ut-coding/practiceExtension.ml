open Lib

let () =
    let module F2 = IntegerModulo.Make (struct let m = 2 end) in
    let module PolF2 = Polynomial.Make (F2) in
    let module F8 = Quotient.Make (struct
            module R = PolF2
            let m = [1; 1; 0; 1]
        end)
    in

    let print_vector vector =
        vector
        |> List.map F8.to_string
        |> String.concat "; "
        |> print_endline
    in

    let pow = Common.repeat F8.( * ) F8.one in
    let beta n = pow n [0; 1] in

    let module CParam =
    struct
        module F = F8
        let n = 7
        let k = 1
        let alpha = [beta 0; beta 1; beta 2; beta 3; beta 4; beta 5; beta 6]
        let v = F.[one; one; one; one; one; one; one]
    end
    in
    let module C = Grs.Code (CParam) in

    let y = [beta 1; beta 1; beta 2; F8.zero; beta 4; beta 5; beta 0] in
    let c = C.decode y in
    print_vector c
