(* vim: set softtabstop=2 sw=2: *)
open Xi_lib
open Iface
open Ir
open Ir_utils
open Analysis
open Analysis_domain

let (<.>) f g x = f(g(x))

module Make() = struct

    module Implementation(M:sig val cfg: ControlFlowGraph.t end) = struct
        open M

        let initialize_table () =
            let table = Hashtbl.create 513 in
            let kw = Knowledge.make ~pre:RegSet.empty ~post:RegSet.empty in
            let blk_kw = BlockKnowledge.make_simple kw in
            let set v = Hashtbl.replace table v blk_kw in
            List.iter set @@ ControlFlowGraph.labels cfg;
            table

        let result : LiveVariables.table = initialize_table ()

        let flow_func node input ~kill ~gen = RegSet.union
          ((RegSet.diff input <.> RegSet.of_list <.> kill) node)
          ((RegSet.of_list <.> gen) node)

        let transfer_terminator t input =
          let t_regs = flow_func t input ~kill:defined_registers_terminator
                                         ~gen:used_registers_terminator
          in Knowledge.make ~pre:t_regs ~post:input

        let transfer_instrs instrs initial_kw =
          let transfer_instr instr (prev_regs, res) =
            let new_regs = flow_func instr prev_regs ~kill:defined_registers_instr
                                                     ~gen:used_registers_instr
            in let instr_kw = Knowledge.make ~pre:new_regs ~post:prev_regs in
            new_regs, (instr_kw, instr) :: res
          in
          List.fold_right transfer_instr instrs (Knowledge.pre initial_kw, [])

        let transfer_basic_block_aux l post_regs =
          let t = ControlFlowGraph.terminator cfg l in
          let instrs = ControlFlowGraph.block cfg l in
          let t_kw = transfer_terminator t post_regs in
          let output, instr_kws = transfer_instrs instrs t_kw in
          let block = Knowledge.make ~pre:output ~post:post_regs in
          BlockKnowledge.make_complex ~block ~body:instr_kws ~terminator:(t_kw, t)

        let transfer_basic_block (Label l_val as l) blk_kw post_regs =
          let (Label exit_lbl_val) = ControlFlowGraph.exit_label cfg in
          let (Label entry_lbl_val) = ControlFlowGraph.entry_label cfg in
          if l_val == exit_lbl_val || l_val == entry_lbl_val
          then blk_kw
          else transfer_basic_block_aux l post_regs

        let analyse_cfg_block l =
          (* rtype: bool indicating whether something changed *)
          let blk_kw = Hashtbl.find result l in
          let previous_pre = BlockKnowledge.pre blk_kw in
          let post_regs =
            ((List.fold_left RegSet.union RegSet.empty) <.>
             (List.map (BlockKnowledge.pre <.> Hashtbl.find result))
            ) (ControlFlowGraph.successors cfg l)
          in
          let new_blk_kw = transfer_basic_block l blk_kw post_regs in
          Hashtbl.replace result l new_blk_kw;
          (RegSet.compare previous_pre (BlockKnowledge.pre new_blk_kw)) <> 0

        let rec compute_fixpoint_loop = function
          | [] -> ()
          | l :: labels ->
            if analyse_cfg_block l
            then compute_fixpoint_loop (labels @ (ControlFlowGraph.predecessors cfg l))
            else compute_fixpoint_loop labels

        let compute_fixpoint () =
          compute_fixpoint_loop (ControlFlowGraph.labels cfg)

        let analyse () =
            compute_fixpoint ();
            result

    end

    let analyse cfg =
        let module Instance = Implementation(struct let cfg = cfg end) in
        Instance.analyse ()

end
