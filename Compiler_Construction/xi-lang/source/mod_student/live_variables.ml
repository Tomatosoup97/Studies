open Xi_lib
open Iface
open Ir
open Ir_utils
open Analysis (* <--- tu mogą być pomocne komentarze *)
open Analysis_domain

module Make() = struct

    module Implementation(M:sig val cfg: ControlFlowGraph.t end) = struct
        open M

        (*
         * Zwróćmy tablicę gdzie każdy wierzchołek jest zainicjalizowany na
         * konstruktor Simple (typ BlockKnowledge) gdzie na wejściu/wyjściu
         * bloku mamy pusty zbiór rejestrów.
         *
         * Wierzchołki oznaczające basic-bloki powinny ostatecznie być opisane
         * konstruktorem Complex, ale początkowo dla wygody możemy ustawić je na Simple.
         * Ważne aby funkcja przeliczająca wiedzę dla bloku podstawowego ostatecznie
         * opisał blok za pomocą konstruktora Complex. 
         *)
        let initialize_table () =
            let table = Hashtbl.create 513 in
            let kw = Knowledge.make ~pre:RegSet.empty ~post:RegSet.empty in
            let blk_kw = BlockKnowledge.make_simple kw in 
            let set v = 
                Hashtbl.replace table v blk_kw
            in
            List.iter set @@ ControlFlowGraph.labels cfg;
            table


        let result : LiveVariables.table = initialize_table ()

        let rec compute_fixpoint () =
            failwith "Not yet implemented"

        let analyse () =
            compute_fixpoint ();
            result

    end

    let analyse cfg = 
        let module Instance = Implementation(struct let cfg = cfg end) in
        Instance.analyse ()


end
