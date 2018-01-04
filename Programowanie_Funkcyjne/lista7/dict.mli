module type Dictionary =
sig
    type key
    type 'a t

    exception DuplicatedKey of key

    val empty: unit -> 'a t

    val lookup: 'a t -> key -> 'a option

    val insert: 'a t -> key * 'a -> 'a t

    val delete: 'a t -> key -> 'a t

    val update: 'a t -> key * 'a -> 'a t
end

