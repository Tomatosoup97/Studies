(* Mateusz Urbańczyk Lista 7 *)

module Dictionary (Key: ORDER) : DICTIONARY with type key = Key.t =
struct
    type key = Key.t

    exception DuplicatedKey of key
    exception KeyNotFound of key

    let empty = fun () -> []

    let rec lookup xs key =
        match xs with
              [] -> None
            | (k, x)::xs' -> (match Key.compare key k with
                  LT -> lookup xs' key
                | EQ -> Some x
                | GT -> failwith "Not found"
                )

    let rec insert xs (key, value) =
        match xs with
              [] -> [(key, value)]
            | (k, x)::xs' ->
                    (match Key.compare key k with
                          LT -> (k, x)::(insert xs' (key, value))
                        | EQ -> raise (DuplicatedKey key)
                        | GT -> (k, x)::(key, value)::xs'
                    )

    let rec delete xs key =
        match xs with
            [] -> raise (KeyNotFound key)
        | (k, x)::xs' ->
            (match Key.compare key k with
                (* TODO *)
                  LT -> ()
                |
            )
end

