module type S =
sig
    include Ring.S

    val inv: t -> t
end
