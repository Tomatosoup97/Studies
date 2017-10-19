(* Mateusz Urbańczyk *)

(* zad. 1 *)

let f (x, y) = (x + 42, y + 42)
let g (x, y) = (x +. y) = 42.17
let h (a, b) = if List.hd a = List.hd (List.tl a) && b = 0 then a else List.tl a


(* zad. 2 *)

let rec last_element list =
    if list = [] || List.tl list = [] then List.hd list
    else last_element (List.tl list)

let ends list = (List.hd list, last_element list)

let result = ends [1; 2; 3]
let result2 = ends [1; 2; 3; 4; 5; 6; 7]
(* let result3 = ends [] *)
let result4 = ends [1; 42]
let result5 = ends [1]


(* zad. 3 *)

let secondEl list = List.hd (List.tl list)

let rec isSorted list =
  if list = [] || List.tl list = [] then true
  else if List.hd list <= secondEl list then isSorted (List.tl list)
  else false

let r1 = isSorted [1; 5; 17]
let r2 = isSorted []
let r3 = isSorted [1]
let r4 = isSorted [6; 5; 2]
let r5 = isSorted [5; 5; 5]
let r4 = isSorted [1; 2; 3; 4; 3]


(* zad. 4 *)

let power_ints (n, exp) = int_of_float (float_of_int n ** (float_of_int exp))

let rec powers (num, exp) =
  if exp <= 0 then [1]
  else powers (num, exp-1) @ [power_ints (num, exp)]

let r1 = powers (2, 3)
let r2 = powers (3, 0)
let r3 = powers (3, 1)
let r4 = powers (3, -10)


(* zad. 5 *)

let rec split (xs, el) =
    if xs = [] then ([], [])
    else let (lt, gt) = split(List.tl xs, el) and x = List.hd xs in
    if List.hd xs <= el then (x :: lt, gt) else (lt, x :: gt)

let split_r1 = split (['a'; 's'; 'h'; 'g'], 'g')
let split_r2 = split ([5; 4; 10; 42; 17], 15)
let split_r3 = split ([], 1)
let split_r4 = split ([1], 1)


(* zad. 6 *)

let rec take (xs, n) =
    if n == 0 || xs = [] then ([], xs) else
    let x = List.hd xs and tail = List.tl xs in
    let (taken, dropped) = take (tail, n-1) in
    (x :: taken, dropped)


let test_take0 = take ([1; 2; 3], 1)
let test_take1 = take ([1; 2; 3], 2)
let test_take2 = take ([1; 2; 3], 0)
let test_take3 = take ([], 0)


let rec segments (xs, size) =
    if xs = [] || size = 0 then [] else
    let (first_seg, xs') = take(xs, size) in
    first_seg :: segments(xs', size)

let test_segments0 = segments([1;2;3;4;5;6;7;8;9], 2)
let test_segments1 = segments([1;2;3;4;5;6;7;8;9], 3)
let test_segments4 = segments([1], 0)
let test_segments3 = segments([], 2)


(* zad. 7 *)

let rec swap (xs, p) = let (xs', ys') = take (xs, p) in ys' @ xs'

let test_swap0 = swap(["a"; "b"; "5"; "6"], 2)
let test_swap1 = swap(["a"; "b"; "5"; "6"], 1)
let test_swap2 = swap(["a"; "b"; "5"; "6"], 0)
let test_swap2 = swap([], 2)

