open Common

module type GRSParam =
sig
    module F: Field
    val n: int
    val k: int
    val alpha: F.t list
    val v: F.t list
end

module GRSCode (Param: GRSParam) =
struct
    include Param

    module PolF = Pol (F)
    let d = n - k + 1

    let rec pow a = function
        | 0 -> F.one
        | n ->
            let r = pow a (n - 1) in
            F.(a * r)

    let rec mul a = function
        | 0 -> F.zero
        | n ->
            let r = mul a (n - 1) in
            F.(a + r)

    let decode (y: F.t list): F.t list =
        let s = List.init (d - 2 + 1) (fun l ->
                alpha
                |> List.map (fun alphai -> pow alphai l)
                |> List.map2 F.( * ) v
                |> List.map2 F.( * ) y
                |> List.fold_left F.(+) F.zero
            )
        in

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

        let c = List.hd t in
        let c_inv = F.inv c in
        let lambda = PolF.([c_inv] * t) in
        let gamma = PolF.([c_inv] * r) in

        let rec eval f x = match f with
            | [] -> F.zero
            | a :: f -> F.(a + x * eval f x)
        in

        let deriv f = PolF.create @@ List.tl (List.mapi (fun i a -> mul a i) f) in
        let lambda_deriv = deriv lambda in

        let e = List.map2 (fun alphaj vj ->
                let alphaj_inv = F.inv alphaj in
                let vj_inv = F.inv vj in
                if eval lambda alphaj_inv = F.zero then
                    F.((neg (alphaj * vj_inv)) * (eval gamma alphaj_inv) * inv (eval lambda_deriv alphaj_inv))
                else
                    F.zero
            ) alpha v in

        let c = List.map2 (fun yi ei -> F.(yi + neg ei)) y e in
        c
end

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

    let [n; k] = read_vector 2 in

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
    let module C = GRSCode (CParam) in
    let c = C.decode y in
    print_vector c


let () = solve_grs_decode_task ()