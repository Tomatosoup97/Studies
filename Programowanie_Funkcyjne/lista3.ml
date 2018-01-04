(* Mateusz Urbańczyk lista 3 *)

(* Zad. 1 *)
(* a *)

let rec czyIstnieje1 p xs = match xs with
    | [] -> false
    | x::xs -> p(x) || czyIstnieje1 p xs

let testCzyIstnieje1_1 = czyIstnieje1 (function  x -> x=2) [1;2;3;5]
let testCzyIstnieje1_2 = czyIstnieje1 (function  x -> x=2) [1;3;5]
let testCzyIstnieje1_3 = czyIstnieje1 (function  x -> x=2) []


(* b *)

let czyIstnieje2 p = List.fold_left (fun acc x -> acc || p(x)) false

let testCzyIstnieje2_1 = czyIstnieje2 (function  x -> x=2) [1;2;3;5]
let testCzyIstnieje2_2 = czyIstnieje2 (function  x -> x=2) [1;3;5]
let testCzyIstnieje2_3 = czyIstnieje2 (function  x -> x=2) []


(* c *)

let czyIstnieje3 p xs = List.fold_right (fun x acc -> acc || p(x)) xs false

let testCzyIstnieje3_1 = czyIstnieje3 (function  x -> x=2) [1;2;3;5]
let testCzyIstnieje3_2 = czyIstnieje3 (function  x -> x=2) [1;3;5]
let testCzyIstnieje3_3 = czyIstnieje3 (function  x -> x=2) []


(* Zad. 2 *)


let filter f xs = List.fold_right (fun x ys -> if f(x) then x::ys else ys) xs []

let testFilter1 = filter (fun x -> x > 3) [1; 2; 3; 4; 5]
let testFilter1 = filter (fun x -> x > 3) []


(* Zad. 3 *)

(* a *)

let usunl p xs =
    let rec aux xs acc = match xs with
        | [] -> acc
        | x::xs when p(x) -> List.rev acc @ xs
        | x::xs -> aux xs (x::acc)
    in aux xs []


let testUsunl1 = usunl (function x -> x=2) [1;2;3;2;5]
let testUsunl2 = usunl (function x -> x=2) []


(* b *)

let usunl_tail p xs =
    let rec aux xs acc = match xs with
        | [] -> acc
        | x::xs when p(x) -> List.rev_append acc xs
        | x::xs -> aux xs (x::acc)
    in aux xs []


let testUsunlTail1 = usunl_tail (function x -> x=2) [1;2;3;2;5]
let testUsunlTail1 = usunl_tail (function x -> x=22) [1;2;3;2;5]
let testUsunlTail2 = usunl_tail (function x -> x=2) []

