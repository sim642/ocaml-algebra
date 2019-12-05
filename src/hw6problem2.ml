open Common

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
    |> Printf.printf "%s\n"

let solve_grs_decode_task () =
    let p = read_int () in
    let module F = Zn (struct let n = p end) in
    let module PolF = Pol (F) in

    let rec pow a = function
        | 0 -> F.one
        | n ->
            let r = pow a (n - 1) in
            F.(a * r)
    in

    let module PolFIO = IntPolIO (PolF) in

    let [n; k] = read_vector 2 in
    let d = n - k + 1 in

    let alpha = read_vector n in
    let v = read_vector n in
    let y = read_vector n in

    let s = List.init (d - 2 + 1) (fun l ->
            alpha
            |> List.map (fun alphai -> pow alphai l)
            |> List.map2 F.( * ) v
            |> List.map2 F.( * ) y
            |> List.fold_left F.(+) F.zero
        )
    in
    (* PolFIO.print_pol s; *)

    (* copied from EuclideanAlgorithm *)
    let extended_gcd a b =
        let open PolF in
        let rec egcd r rr t tt =
            if deg rr < (d - 1) / 2 then
                (rr, tt)
            else
                let (q, rrr) = quot_rem r rr in
                let ttt = t + neg (q * tt) in
                egcd rr rrr tt ttt
        in
        egcd a b zero one
    in

    let (r, t) = extended_gcd (List.init (d - 1) (fun _ -> F.zero) @ [F.one]) s in
    (* PolFIO.print_pol r;
    PolFIO.print_pol t; *)

    let c = List.hd t in
    let c_inv = F.inv c in
    (* Printf.printf "%d %d\n" c c_inv; *)
    let lambda = PolF.([c_inv] * t) in
    let gamma = PolF.([c_inv] * r) in
    (* PolFIO.print_pol lambda;
    PolFIO.print_pol gamma; *)

    let rec eval f x = match f with
        | [] -> F.zero
        | a :: f -> F.(a + x * eval f x)
    in

    let deriv f = PolF.create @@ List.tl (List.mapi (fun i a -> F.(i * a)) f) in
    let lambda_deriv = deriv lambda in

    let e = List.map2 (fun alphaj vj ->
            (* Printf.printf "alphaj: %d\n" alphaj; *)
            let alphaj_inv = F.inv alphaj in
            (* Printf.printf "vj: %d\n" vj; *)
            let vj_inv = F.inv vj in
            (* Printf.printf "stuff\n"; *)
            if eval lambda alphaj_inv = F.zero then
                F.((neg (alphaj * vj_inv)) * (eval gamma alphaj_inv) * inv (eval lambda_deriv alphaj_inv))
            else
                F.zero
        ) alpha v in
    (* print_vector e; *)

    let c = List.map2 (fun yi ei -> F.(yi + neg ei)) y e in
    print_vector c


let () = solve_grs_decode_task ()