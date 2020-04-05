(* for HackerRank's suddenly outdated OCaml... *)
(* module List =
struct
    include List

    (* copied from OCaml 4.06 List.ml *)
    let init len f =
        let rec init_aux i n f =
            if i >= n then
                []
            else
                let r = f i in
                r :: init_aux (i+1) n f
        in
        init_aux 0 len f
end *)

let rec repeat f a n x = match n with
    | 0 -> a
    | n -> f x (repeat f a (n - 1) x)
