open Xi_lib
open Ir
open Ir_utils

module Make() = struct

  module Implementation(M:sig
      val cfg: ControlFlowGraph.t
      val initial: Analysis_domain.ConstantFolding.domain
    end) = struct

    open M

    let analyse () = 
      failwith "not yet implemented"
  end



  (* Skontruuj wartość ekstremalną *)
  let make_initial n =
    failwith "not yet implemented"

  let analyse proc : Xi_lib.Analysis_domain.ConstantFolding.table =
    let initial = make_initial @@ Ir.formal_parameters_of_procedure proc in
    let cfg = Ir.cfg_of_procedure proc in
    let module Instance = Implementation(struct let cfg = cfg let initial = initial end) in 
    let result = Instance.analyse () in
    result

end