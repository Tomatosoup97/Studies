type ordering

module type ORDER =
sig
    type t
    val compare: t -> t -> ordering
end

