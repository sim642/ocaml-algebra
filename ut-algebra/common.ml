open Lib

let solve_quot_rem_task (module F: Field.S with type t = int) =
    let module PolF = Polynomial.Make (F) in
    let input_pol s =
        Printf.printf "%s kordajad: " s;
        read_line ()
        |> String.split_on_char ' '
        |> List.map int_of_string
        |> PolF.create
    in
    let f = input_pol "f" in
    let g = input_pol "g" in
    let (q, r) = PolF.quot_rem f g in
    Printf.printf "  q kordajad: %s\n" (PolF.to_string q);
    Printf.printf "  r kordajad: %s\n" (PolF.to_string r)