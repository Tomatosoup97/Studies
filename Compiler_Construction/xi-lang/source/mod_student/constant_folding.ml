open Xi_lib
open Ir

module Make(T:Iface.COMPILER_TOOLBOX) = struct

  module Implementation(M:sig
    val cfg : ControlFlowGraph.t 
    val proc : procedure
   end) = struct

    open M

    let cfa = T.ConstantFoldingAnalysis.analyse proc

    let rewrite () = 
      Logger.extra_debug begin fun () ->
        Logger.dump_constant_folding "before-optimization" cfg cfa;
      end;
      failwith "not yet implemeneted"

  end


  let fold_constants proc = 
    let module Instance = Implementation(struct
      let proc = proc
      let cfg = cfg_of_procedure proc
    end) in
    Instance.rewrite ()

end