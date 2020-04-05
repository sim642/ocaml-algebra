module type S =
sig
    include Ring.S

    val quot_rem: t -> t -> t * t
end

module Algorithm (R: S):
sig
    val extended_gcd: R.t -> R.t -> R.t * R.t * R.t

    val gcd: R.t -> R.t -> R.t
    val lcm: R.t -> R.t -> R.t
end