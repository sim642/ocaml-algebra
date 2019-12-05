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

    let quot_rem a b = (a / b, (a mod b + b) mod b) (* non-negative remainder *)
end

module Pol (F: Field) =
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
end

module type EuclideanRing =
sig
    include Ring
    val quot_rem: t -> t -> t * t
end

module EuclideanAlgorithm (R: EuclideanRing) =
struct
    open R

    (* https://en.wikipedia.org/wiki/Polynomial_greatest_common_divisor#B%C3%A9zout's_identity_and_extended_GCD_algorithm *)
    let extended_gcd a b =
        let rec egcd r rr s ss t tt =
            if rr = zero then
                (r, s, t)
            else
                let (q, rrr) = quot_rem r rr in
                let sss = s + neg (q * ss) in
                let ttt = t + neg (q * tt) in
                egcd rr rrr ss sss tt ttt
        in
        egcd a b one zero zero one

    let gcd a b = let (d, _, _) = extended_gcd a b in d
    let lcm a b = fst @@ quot_rem (a * b) (gcd a b)
end

module ZEuclid = EuclideanAlgorithm (Z)

module Zn (Arg: sig val n: int end) =
struct
    type t = int
    let n = Arg.n

    let create a = snd @@ Z.quot_rem a n

    let to_string a = string_of_int a

    let (+) a b = create (a + b)
    let zero = 0
    let neg a = create (-a)
    let ( * ) a b = create (a * b)
    let one = 1
    let inv a =
        (* https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers *)
        let (d, _, v) = ZEuclid.extended_gcd n a in
        assert (d = 1);
        create v
end

module type PolRing =
sig
    type e
    include Ring with type t = e list

    val create: e list -> t
    val deg: t -> int
end

module type IntPolRing = PolRing with type e = int

module IntPolIO (PolR: IntPolRing) =
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