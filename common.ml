module type Ring =
sig
    type t
    val to_string: t -> string

    val (+): t -> t -> t
    val zero: t
    val neg: t -> t
    val ( * ): t -> t -> t
    val one: t
end

module type Field =
sig
    include Ring
    val inv: t -> t
end

module Z =
struct
    type t = int

    let to_string = string_of_int

    let (+) = Pervasives.(+)
    let zero = 0
    let neg a = -a
    let ( * ) = Pervasives.( * )
    let one = 1

    let inv = function
        | 1 -> 1
        | -1 -> -1
        | x -> failwith @@ Printf.sprintf "%d not invertible in Z" x

    let quot_rem a b = (a / b, (a mod b + b) mod b) (* positiivne jääk *)
end

module Pol (F: Field) =
struct
    type t = F.t list

    let create f =
        let rec drop_while p = function
            | [] -> []
            | x :: xs -> if p x then drop_while p xs else x :: xs
        in
        List.rev f |> drop_while ((=) F.zero) |> List.rev
    
    let to_string f = List.fold_right (fun a acc -> F.to_string a ^ " " ^ acc) f ""

    let rec (+) f g = create @@ match f, g with
        | f, [] | [], f -> f
        | a :: f, b :: g -> F.(a + b) :: (f + g)
    let zero = []
    let neg f = create @@ List.map F.neg f
    let ( * ) f g = create @@ List.fold_right (fun a acc -> List.map (F.( * ) a) g + (F.zero :: acc)) f zero
    let one = [F.one]

    let deg f = List.length f - 1

    (* loengukonspekti teoreemist 9.11 *)
    let rec quot_rem f g =
        let n = deg f in
        let m = deg g in
        if n < m then
            (zero, f)
        else
            let an = List.nth f n in
            let bm = List.nth g m in
            let lead = List.init (n - m) (fun _ -> F.zero) @ [F.(inv bm * an)] in
            (* let lead = Core_kernel.List.init (n - m) (fun _ -> F.zero) @ [F.(inv bm * an)] in *)
            let g1 = g * lead in
            let f1 = f + neg g1 in
            let (q1, r) = quot_rem f1 g in
            (lead + q1, r)
end


let solve_quot_rem_task (module F: Field with type t = int) =
    let module PolF = Pol (F) in
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