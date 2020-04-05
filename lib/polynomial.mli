module type S =
sig
    type e
    include Euclidean.S with type t = e list

    val create: e list -> t
    val deg: t -> int
    val eval: t -> e -> e
    val deriv: t -> t
end

module Make (F: Field.S): S with type e = F.t

module IntIO (PolR: S with type e = int):
sig
    val read_pol: unit -> PolR.t
    val print_pol: PolR.t -> unit
end