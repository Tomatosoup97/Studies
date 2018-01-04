type ordering = LT | EQ | GT

module type ORDER =
sig
    type t
    val compare: t -> t -> ordering
end

module StringOrder: ORDER with type t = string =

struct
    type t = string
    let compare s1 s2 = if s1<s2 then LT else
                        if s1>s2 then GT else EQ
end

