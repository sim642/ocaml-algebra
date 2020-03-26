open Lib.Common

let read_vector n =
    let vector =
        read_line ()
        |> String.split_on_char ' '
        |> List.map int_of_string
    in
    assert (List.length vector = n);
    vector

let print_vector vector =
    vector
    |> List.map string_of_int
    |> String.concat " "
    |> print_endline

let solve_grs_decode_task () =
    let p = read_int () in
    let module F = Zn (struct let n = p end) in

    let (n, k) = match read_vector 2 with
        | [n; k] -> (n, k)
        | _ -> failwith "read_vector lied"
    in

    let alpha = read_vector n in
    let v = read_vector n in
    let y = read_vector n in

    let module CParam =
    struct
        module F = F
        let n = n
        let k = k
        let alpha = alpha
        let v = v
    end
    in
    let module C = Grs.Code (CParam) in
    let c = C.decode y in
    print_vector c


let () = solve_grs_decode_task ()