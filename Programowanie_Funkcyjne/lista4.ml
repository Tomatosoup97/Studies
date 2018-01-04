(* Mateusz Urbańczyk - lista 4 *)

type 'a tree = L of 'a | N of 'a tree * 'a tree

exception Load of string

let rec store (t: 'a tree) : 'a option list = match t with
    | L x -> [Some x]
    | N (lt, rt) -> (store lt) @ (store rt) @ [None]

let test_store_1 = store (L 2)
let test_store_2 = store (N (L 3, L 4))
let test_store_3 = store (N (L 3, (N (L 5, L 10))))
let test_store_4 = store (N ((N (L 5, L 10)), L 3))


let load (xs: 'a option list)  =
    let rec load_aux xs stack = match xs with
        | [] -> (match stack with
            | [x] -> x
            | stack' -> raise @@ Load "Invalid tree")
        | (Some x)::xs' -> load_aux xs' ((L x)::stack)
        | None::xs' -> (match stack with
            | h::h'::stack' -> load_aux xs' ((N (h', h))::stack')
            | stack' -> raise @@ Load "Invalid tree")
    in load_aux xs []


let test_load_1 = load [Some 2]
let test_load_2 = load [Some 3; Some 4; None]
let test_load_3 = load [Some 3; Some 5; Some 10; None; None]
let test_load_4 = load [Some 5; Some 10; None; Some 3; None]

