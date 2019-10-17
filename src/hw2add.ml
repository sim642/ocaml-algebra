open Common

let solve_add_task () =
    let p = read_int () in
    let module F = Zn (struct let n = p end) in
    let module PolF = Pol (F) in

    let read_pol () =
        let deg = read_int () in
        let f =
            read_line ()
            |> String.split_on_char ' '
            |> List.map int_of_string
            |> PolF.create
        in
        assert (deg = PolF.deg f);
        f
    in
    let a = read_pol () in
    let b = read_pol () in
    let c = PolF.(a + b) in

    let print_pol f =
        let deg = PolF.deg f in
        Printf.printf "%d\n" deg;
        match deg with
        | -1 -> Printf.printf "0\n"
        | _ -> Printf.printf "%s\n" (PolF.to_string f)
    in
    print_pol c

let () = solve_add_task ()