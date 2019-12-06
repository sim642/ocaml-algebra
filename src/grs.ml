open Common

module type Param =
sig
    module F: Field
    val n: int
    val k: int
    val alpha: F.t list
    val v: F.t list
end

module Code (Param: Param) =
struct
    include Param

    module PolF = Pol (F)
    let d = n - k + 1

    let h =
        let pow = repeat F.( * ) F.one in
        List.init (d - 2 + 1) (fun l ->
                alpha
                |> List.map (pow l)
                |> List.map2 F.( * ) v
            )

    let syndrome y = PolF.create @@ List.map (fun h_row ->
            h_row
            |> List.map2 F.( * ) y
            |> List.fold_left F.(+) F.zero
        ) h

    let euclidean_key_equation s =
        (* copied & modified from EuclideanAlgorithm *)
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

        let a = List.init (d - 1) (fun _ -> F.zero) @ [F.one] in
        let (r, t) = extended_gcd a s in

        let c_inv = F.inv (List.hd t) in
        let lambda = PolF.([c_inv] * t) in
        let gamma = PolF.([c_inv] * r) in
        (lambda, gamma)

    let forney lambda gamma =
        let lambda_eval = PolF.eval lambda in
        let gamma_eval = PolF.eval gamma in
        let lambda'_eval = PolF.(eval (deriv lambda)) in

        List.map2 (fun alpha_j v_j ->
                let open F in
                let alpha_j_inv = inv alpha_j in
                if lambda_eval alpha_j_inv = zero then
                    neg alpha_j * inv v_j * gamma_eval alpha_j_inv * inv (lambda'_eval alpha_j_inv)
                else
                    zero
            ) alpha v

    let decode (y: F.t list): F.t list =
        let s = syndrome y in
        let (lambda, gamma) = euclidean_key_equation s in

        let e = forney lambda gamma in
        let c = List.map2 (fun y_i e_i -> F.(y_i + neg e_i)) y e in
        c
end