module type S =
sig
    type e
    include Ring.S with type t = e list

    val create: e list -> t
    val deg: t -> int
end

module Make (F: Field.S) =
struct
    type e = F.t
    type t = e list

    let create f =
        let rec drop_while p = function
            | [] -> []
            | x :: xs -> if p x then drop_while p xs else x :: xs
        in
        List.rev f |> drop_while ((=) F.zero) |> List.rev

    let to_string f = String.concat " " (List.map F.to_string f)

    let rec (+) f g = create @@ match f, g with
        | f, [] | [], f -> f
        | a :: f, b :: g -> F.(a + b) :: (f + g)
    let zero = []
    let neg f = create @@ List.map F.neg f
    let ( * ) f g = create @@ List.fold_right (fun a acc -> List.map (F.( * ) a) g + (F.zero :: acc)) f zero
    let one = [F.one]

    let deg f = List.length f - 1

    (* from Algebra I lecture notes theorem 9.11 *)
    let rec quot_rem f g =
        let n = deg f in
        let m = deg g in
        if n < m then
            (zero, f)
        else
            let an = List.nth f n in
            let bm = List.nth g m in
            let lead = List.init (n - m) (fun _ -> F.zero) @ [F.(inv bm * an)] in
            let g1 = g * lead in
            let f1 = f + neg g1 in
            let (q1, r) = quot_rem f1 g in
            (lead + q1, r)

    let eval f x = List.fold_right (fun a acc -> F.(a + x * acc)) f F.zero

    let deriv = function
        | [] -> []
        | f ->
            let mul = Common.repeat F.(+) F.zero in
            create @@ List.tl (List.mapi mul f)
end


module IntIO (PolR: S with type e = int) =
struct
    let read_pol () =
        let deg = read_int () in
        let f =
            read_line ()
            |> String.split_on_char ' '
            |> List.map int_of_string
            |> PolR.create
        in
        assert (deg = PolR.deg f);
        f

    let print_pol f =
        let deg = PolR.deg f in
        Printf.printf "%d\n" deg;
        match deg with
        | -1 -> Printf.printf "0\n"
        | _ -> Printf.printf "%s\n" (PolR.to_string f)
end