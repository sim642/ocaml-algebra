module Make (R: Euclidean.S): Field.S with type t = R.t * R.t

module Integer: Field.S with type t = int * int