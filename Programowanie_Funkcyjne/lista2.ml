(* Mateusz Urbańczyk lista 2 *)

(* zad. 1 *)

let rec fib n =
    if n < 2 then n else
    fib(n-1) + fib(n-2)

let test_fib = List.map fib [0;1;2;3;4;5;6;7;8;9;10]
(* let test_fib_big = fib 42 *)

let fib_tail n =
    let rec aux (n, a, b) =
        if n < 1 then a else aux (n-1, b, a+b)
    in aux (n, 0, 1)

let test_fib_tail = List.map fib_tail [0;1;2;3;4;5;6;7;8;9;10]
(* let test_fib_tail_big = fib_tail 42 *)

(* zad. 3 *)

let reverse xs =
    let rec aux acc = function
        | [] -> acc
        | x::xs' -> aux (x::acc) xs'
    in aux [] xs

let test_reverse0 = reverse [1;2;3;4;5]
let test_reverse1 = reverse [1]
let test_reverse2 = reverse []

let rec (<--) xs n =
    let rec aux xs acc n = match xs with
        | [] -> ([n], acc)
        | x::xs' when x >= n -> (n :: xs, acc)
        | x::xs' -> aux xs' (x :: acc) n
    in let (xs, acc) = aux xs [] n
    in (reverse acc) @ xs

let test_insert0 = [1;3;5;5;7] <-- 2
let test_insert1 = [1;3;5;5;7] <-- 5
let test_insert2 = [1;7] <-- 10
let test_insert3 = [42] <-- 0
let test_insert4 = [] <-- 42

(* --- *)

let rec _take n xs = match xs with
    | [] -> ([], [])
    | xs when n <= 0 -> ([], xs)
    | x::xs' ->
        let (taken, dropped) = _take (n-1) xs'
        in (x :: taken, dropped)

(* zad. 4 *)

let take n xs =
    let (taken, dropped) = _take n xs in
    if n >= 0 then taken else dropped

let test_take0 = take 2 [1;2;3;4;5;6]
let test_take1 = take (-2) [1;2;3;4;5;6]
let test_take2 = take 8 [1;2;3;4;5;6]

(* zad. 5 *)

let drop n xs = let (taken, dropped) = _take n xs in dropped

let test_drop0 = drop 2 [1;2;3;4;5;6]
let test_drop1 = drop (-2) [1;2;3;4;5;6]
let test_drop2 = drop 8 [1;2;3;4;5;6]

