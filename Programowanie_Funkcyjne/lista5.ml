(* Mateusz Urbańczyk lista 5 *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist)
type 'a lBT = LEmpty | LNode of 'a * ('a lBT Lazy.t) * ('a lBT Lazy.t)


let rec ltake = function
    (0, _) -> []
    | (_, LNil) -> []
    | (n, LCons(x,xf)) -> x::ltake(n-1, xf())


let rec toLazyList = function
    [] -> LNil
    | x::xs -> LCons(x, function () -> toLazyList xs)


let rec (@$) ll1 ll2 = match ll1 with
    | LNil -> ll2
    | LCons (x, xf) -> LCons (x, fun () -> (xf()) @$ ll2)


(* Zad 1. *)

let rec repeatBeg x n tail = match n with
    | 0 -> tail()
    | n when n > 0 -> LCons (x, fun () -> repeatBeg x (n-1) tail)
    | n -> failwith "Negative number"


let rec lrepeat (f: int -> int) (xs: 'a llist) =
    let rec aux n xs = match xs with
        | LNil -> LNil
        | LCons (x, xf) -> repeatBeg x (f n) (fun () -> (aux (n+1) (xf())))
    in aux 0 xs


let rec lfrom k = LCons (k, function () -> lfrom (k+1))

let test_lrepeat0 = ltake (15, lrepeat (fun x -> x+1) (toLazyList [1;2;3;4;5]))
let test_lrepeat1 = lrepeat (fun x -> x+1) (toLazyList [])
let test_lrepeat2 = ltake (15, lrepeat (fun x -> x+1) (lfrom 1))


(* Zad 2. *)


let rec remove_indexes xs ll =
    let rec aux ll n = match ll with
        | LNil -> LNil
        | LCons (x, xf) -> if List.mem n xs
                           then aux (xf()) (n+1)
                           else LCons(x, fun () -> aux (xf()) (n+1))
    in aux ll 0

let test_rem_indexes0 = ltake (10, remove_indexes [1; 4; 7; 2] (toLazyList []))
let test_rem_indexes1 = ltake (10, remove_indexes [1; 4; 7; 2] (toLazyList [0;1;2;3;4;5;6;7]))
let test_rem_indexes2 = ltake (10, remove_indexes [1; 4; 7; 2] (toLazyList [10;11;12;13;14;15;16;17]))
let test_rem_indexes3 = ltake (10, remove_indexes [1; 4; 7; 2] (lfrom 10))


(* Zad 3. *)


let rec lfilter f = function
      LNil -> LNil
    | LCons (x, xf) -> if f x
                       then LCons (x, function () -> lfilter f (xf()))
                       else lfilter f (xf())


let splitLL (ll: int llist) = (lfilter ((<=) 0) ll, lfilter (fun x -> x < 0) ll)


let split_res0 = fun () -> splitLL (lfrom (-5))

let test_split0_pos = ltake (10, fst (splitLL (lfrom (-15))))
let test_split0_neg = ltake (10, snd (splitLL (lfrom (-15))))

let test_split1_neg = ltake (10, fst (splitLL (toLazyList [-1;2;-3;4;-5;6;-7;8])))
let test_split1_pos = ltake (10, snd (splitLL (toLazyList [-1;2;-3;4;-5;6;-7;8])))


(* Zad 4. *)


let rec insert (tree: 'a lBT) x = match tree with
    | LEmpty -> LNode (x, lazy(LEmpty), lazy(LEmpty))
    | LNode (y, lt, rt) ->
        if x = y then tree
        else if x < y then LNode (y, lazy(insert (Lazy.force lt) x), rt)
        else LNode (y, lt, lazy(insert (Lazy.force rt) x))

let toLBST xs = List.fold_left insert LEmpty xs


