(* Mateusz Urbańczyk Lista 6 *)

(* Library to load before use:
 *
 * #load "str.cma";;
 *
 * *)

(* Zad 1. *)

let zgadnij = fun () ->
    let rand = Random.int 101 in
    let read_try = fun () ->
        let () = print_newline () and () = print_string "Podaj liczbe: "in
        let user_int = read_int() in
        if user_int == rand then
            let () = print_endline "Zgadles. Brawo!" in false
        else if user_int < rand then let () = print_string "moja jest wieksza" in true
        else let () = print_string "moja jest mniejsza" in true
    in
    while read_try() do ()
    done

(* Zad 2. *)

let sortuj_plik = fun () ->
    let () = print_string "Pass file name: " and filename = read_line () in
    let file = open_in filename in
    let num_count = int_of_string @@ input_line file in
    let vector = Array.make num_count 0.0 in
    begin
        let map_line_to_ints s  =
            List.map float_of_string (
                List.filter (fun x -> compare x "" != 0) (Str.split (Str.regexp "[\t\n ]") s)
            ) in
        let rec set_ints xs n = match xs with
            | [] -> n
            | x::xs' -> let () = vector.(n) <- x in set_ints xs' (n+1) in
        let read_nums = fun n ->
            let line = input_line file in
            let ints_list = map_line_to_ints line in
            set_ints ints_list n in
        let rec input_data n =
            let new_n = read_nums n in input_data new_n
        in try
            ignore @@ input_data 0;
            close_in file
        with End_of_file ->
            let rec write_vector oc vector n =
                if n >= num_count then () else
                let str = (string_of_float vector.(n) ^ " ") in
                let () = output oc str 0 (String.length str) in
                write_vector oc vector (n+1) in
            let cmp x y =
                if (x == y) then 0
                else if (x < y) then -1
                else 1 in
            let () = Array.sort cmp vector in
            let () = print_string "Pass file name: " and write_filename = read_line () in
            let write_file = open_out write_filename in
            let () = write_vector write_file vector 0 in
            let () = output write_file "\n" 0 1 in
            let () = close_out write_file in ()
    end

