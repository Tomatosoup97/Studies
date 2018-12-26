(* vim: set softtabstop=2 sw=2: *)
open Xi_lib
open Xi_lib.Measure
open Ir

let logf fmt = Logger.make_logf __MODULE__ fmt

module Make(Toolbox:Iface.COMPILER_TOOLBOX) = struct

  module Implementation(M:sig
    val cfg: ControlFlowGraph.t
    val proc: procedure
    end) = struct

    open M

    module Coalescencing = Toolbox.RegisterCoalescing

    let available_registers = Toolbox.RegistersDescription.available_registers

    let number_of_available_registers = List.length available_registers

    (* ------------------------------------------------------------------------
     *  Hashtable with colors
     *)

    (* wstępnie pokolorowane wierzchołki *)
    let base_register2color_assignment : (reg, int) Hashtbl.t = Hashtbl.create 13

    (* vertexes coloring *)
    let register2color_assignment : (reg, int) Hashtbl.t = Hashtbl.create 13

    (* pomocnicza tablica -- odwzorowuje kolor na rejestr sprzętowy *)
    let color2register_assignment : (int, reg) Hashtbl.t = Hashtbl.create 13

    (* ------------------------------------------------------------------------
     *  Initial coloring
     *)

    let initialize_colors () =
      let color i hard =
        Hashtbl.replace color2register_assignment i hard;
        Hashtbl.replace base_register2color_assignment hard i;
      in
      List.iteri color available_registers

    (* ------------------------------------------------------------------------
     *  Interference graph building
     *)

    let build_infg () =
      logf "building interference graph";
      let lva = Toolbox.LiveVariablesAnalysis.analyse cfg in
      Logger.extra_debug begin fun () ->
        Logger.dump_live_variables "before-inf-build" cfg lva;
      end;
      let infg = Toolbox.InterferenceGraphAnalysis.analyse cfg lva in
      Logger.extra_debug begin fun () ->
        Logger.dump_interference_graph "before-simplify" infg
      end;
      infg

    (* ------------------------------------------------------------------------
     *  Pomocnicze funkcje
     *)

    let log_str_list xs =
      Logger.extra_debug begin fun () ->
        logf "%s" (String.concat " " xs)
      end

    let log_regs_list xs title =
      log_str_list ([title; ": ["] @ (List.map Ir_utils.string_of_reg xs) @ ["]"])

    let loop name f =
      let rec iter i =
        logf "Starting iteration %s %u" name i;
        let r, should_restart = measure "iteration" f in
        if should_restart then
          iter (succ i)
        else
          r
      in
      iter 0

    (* ------------------------------------------------------------------------
     *  Spilling
     *)

    let compute_spill_costs infg =
      Logger.extra_debug begin fun () ->
        logf "Computing dominators"
      end;
      let dom = Toolbox.DominatorsAnalysis.analyse cfg in
      Logger.extra_debug begin fun () ->
        logf "Computing natural-loops"
      end;
      let nloops = Toolbox.NaturalLoopsAnalysis.analyse cfg dom in
      Logger.extra_debug begin fun () ->
        logf "Computing spill-costs"
      end;
      let spill_costs = Toolbox.SpillCostsAnalysis.analyse cfg nloops in
      Logger.extra_debug begin fun () ->
          Logger.dump_spill_costs spill_costs;
      end;
      spill_costs

    let spill actual_spills =
      measure "spill" (fun () -> Toolbox.Spilling.spill proc actual_spills);
      actual_spills <> []

    (* ------------------------------------------------------------------------
     * Simplify phase
     *)

    let graph_vertexes_list g = RegGraph.fold_vertex List.cons g []

    let reg_deg infg = function
        | (REG_Tmp _) as v -> RegGraph.out_degree infg v
        | _ -> max_int

    let min_in_graph infg compare =
        match graph_vertexes_list infg with
          | [] -> None
          | v :: vs ->
            let rec aux v_min = function
              | [] -> Some v_min
              | v :: vs ->
                if compare infg v v_min
                then aux v vs
                else aux v_min vs
            in aux v vs

    let calc_ratio infg scosts v =
      let cost = Hashtbl.find scosts v in
      float_of_int cost /. float_of_int (RegGraph.out_degree infg v)

    let cmp_regs_vals f r1 r2 =
      match f r1 = f r2, r1, r2 with
        | true, REG_Tmp i1, REG_Tmp i2 -> i1 < i2
        | _ -> f r1 < f r2

    let select_spill_candidate infg scosts =
        let vs = graph_vertexes_list infg in
        let compare_costs infg v1 v2 =
          cmp_regs_vals (calc_ratio infg scosts) v1 v2
        in
        match min_in_graph infg compare_costs with
          | Some v -> v
          | None -> failwith "No available candidates for spill!"

    let simplify scosts infg =
        let d_infg = fun () ->
          let _ = RegGraph.fold_vertex
            (fun v acc -> (log_regs_list (RegGraph.succ infg v) (Ir_utils.string_of_reg v)); acc)
            infg []
          in ()
        in
        logf "Run simplify";
        let nodes = Stack.create () in
        let compare_degrees infg v1 v2 = cmp_regs_vals (reg_deg infg) v1 v2 in
        let rec simplify_loop = fun () ->
          match min_in_graph infg compare_degrees with
            | Some (REG_Tmp i as min_v)  ->
              let v_neighbours = RegGraph.succ infg min_v in
              let min_v_deg = RegGraph.out_degree infg min_v in
              if min_v_deg < number_of_available_registers
              then (
                log_str_list ["Stacking register"; Ir_utils.string_of_reg min_v; "of degree"; string_of_int min_v_deg];
                Stack.push (min_v, v_neighbours) nodes;
                RegGraph.remove_vertex infg min_v;
                simplify_loop ()
              )
              else (
                let spill_candidate = select_spill_candidate infg scosts in
                let v_neighbours = RegGraph.succ infg spill_candidate in
                log_str_list (["Local spill costs:"] @ (RegGraph.fold_vertex (fun v acc -> (String.concat ":" [Ir_utils.string_of_reg v; string_of_float (calc_ratio infg scosts v); string_of_int (RegGraph.out_degree infg v)]) :: acc) infg []));
                log_str_list ["Potential spill"; (Ir_utils.string_of_reg spill_candidate); "of degree"; (string_of_int (RegGraph.out_degree infg spill_candidate))];
                Stack.push (spill_candidate, v_neighbours) nodes;
                RegGraph.remove_vertex infg spill_candidate;
                simplify_loop ()
              )
            | _ ->
                logf "No more temporary registers in interference-graph";
                nodes
        in simplify_loop ()

    (* ------------------------------------------------------------------------
     *  Select phase
     *)

    let fst_seq_num = function
      (* Given an sorted array return first sequential element that is not
       * in the list. Example: [0, 1, 2, 4] -> 3 *)
      | [] -> 0
      | x :: xs ->
        let rec aux prev = (function
          | [] -> prev + 1
          | x :: xs -> if x <= (prev + 1) then aux x xs else prev + 1)
        in if x == 0 then aux x xs else 0

    let select_color infg v v_nbs =
      log_str_list ["Picking color for"; Ir_utils.string_of_reg v];
      let get_color v = Hashtbl.find register2color_assignment v in
      let ncolors = List.sort compare (List.map get_color v_nbs) in
      log_str_list (["Neighbours have colors:"] @ List.map string_of_int ncolors);
      let color = fst_seq_num ncolors in
      log_str_list ["Selected color is"; string_of_int color];
      Hashtbl.add register2color_assignment v color;
      color

    let select potential_spills infg =
      logf "Run select";
      let rec aux actual_spills =
        if Stack.is_empty potential_spills
        then (
          log_regs_list actual_spills "Actual spills";
          actual_spills
        ) else
          let v, v_nbs = Stack.pop potential_spills in
          RegGraph.add_vertex infg v;
          List.iter (RegGraph.add_edge infg v) v_nbs;
          if select_color infg v v_nbs >= number_of_available_registers
          then (
            log_str_list ["Spilling register"; Ir_utils.string_of_reg v];
            aux (actual_spills @ [v])
          ) else aux actual_spills
      in aux []

    (* ------------------------------------------------------------------------
     *  build-coalesce loop
     *)

    let build_coalescence () =
      let infg = measure "build" (fun () -> build_infg ()) in
      let changed = measure "coalescence" (fun () ->  Coalescencing.coalesce proc infg available_registers) in
      infg, changed

    let build_coalescence_loop () =
      loop "build-coalescence" build_coalescence

    (* ------------------------------------------------------------------------
     *  Single pass
     *)

    let single_pass () =
      logf "Run single_pass";
      let init () = begin
          Hashtbl.reset register2color_assignment;
          Hashtbl.replace_seq register2color_assignment @@ Hashtbl.to_seq base_register2color_assignment;
      end in
      Logger.extra_debug begin fun () ->
        Logger.dump_ir_proc "begin-loop" proc
      end;
      let init = measure "init" init in
      let infg = measure "build-coalescence " build_coalescence_loop in
      let spill_costs = measure "spillcosts" (fun () -> compute_spill_costs infg) in
      let simplify_stack = simplify spill_costs infg in
      match select simplify_stack infg with
        | [] -> (), false
        | xs -> (), spill xs

    (* ------------------------------------------------------------------------
     *  Budowanie mapowania rejestrów
     *)

    let build_register_assignment () =
      logf "Run build_register_assignment";
      let register_assignment : (reg, reg) Hashtbl.t = Hashtbl.create 513 in
      let f reg c =
        let hard_reg = Hashtbl.find color2register_assignment c in
        Hashtbl.add register_assignment reg hard_reg;
      in
      Hashtbl.iter f register2color_assignment;
      register_assignment

    (* ------------------------------------------------------------------------
     *  Main
     *)

    let regalloc () =
      logf "Starting register-allocation";
      initialize_colors ();
      loop "main-loop" single_pass;
      build_register_assignment ()

  end

  let regalloc proc =
    let module Instance = Implementation(struct
      let cfg = cfg_of_procedure proc
      let proc = proc
      let available_registers = Toolbox.RegistersDescription.available_registers
      end)
    in
    Instance.regalloc ()

end

