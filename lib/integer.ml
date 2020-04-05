type t = int

let to_string = string_of_int

let (+) = Stdlib.(+)
let zero = 0
let neg a = -a
let ( * ) = Stdlib.( * )
let one = 1

let inv = function
    | 1 -> 1
    | -1 -> -1
    | x -> failwith @@ Printf.sprintf "%d not invertible in Z" x

let quot_rem a b =
    let r = (a mod b + b) mod b in (* non-negative remainder *)
    let q = (a - r) / b in
    (q, r)
