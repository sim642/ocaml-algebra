open Common

type table = int array array

module type CayleyTables =
sig
    val add_table: table
    val mul_table: table
end

module Cayley (CT: CayleyTables) =
struct
    type t = int

    let to_string = string_of_int

    let findi a arr =
        let rec check i = if arr.(i) = a then i else check (i + 1) in
        check 0

    let (+) a b = CT.add_table.(a).(b)
    let zero = 0
    let neg a = findi zero CT.add_table.(a)
    let ( * ) a b = CT.mul_table.(a).(b)
    let one = 1
    let inv a = findi one CT.mul_table.(a)
end

(* OCaml'i standardteek on üsna vaene... *)
let file_lines filename =
    let channel = open_in filename in 
    let lines = ref [] in
    (try
        while true; do
            lines := input_line channel :: !lines
        done
    with End_of_file ->
        close_in channel);
    List.rev !lines

let table_of_lines lines =
    let row_of_line line =
        line
        |> String.split_on_char ' '
        |> List.map int_of_string
        |> Array.of_list
    in
    List.map row_of_line lines
    |> Array.of_list


module F = Cayley (struct
    let add_table = file_lines "liitm.txt" |> table_of_lines
    let mul_table = file_lines "korrut.txt" |> table_of_lines
end)

let () = solve_quot_rem_task (module F)