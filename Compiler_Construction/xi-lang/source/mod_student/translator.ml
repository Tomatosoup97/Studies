(* vim: set softtabstop=2 sw=2: *)
open Xi_lib
open Ir

let i32_0 = Int32.of_int 0
let i32_1 = Int32.of_int 1

let uncurry f = fun (x, y) -> f x y

(* --------------------------------------------------- *)

module Make() = struct

  module Environment = struct

    type env = Env of
      { procmap: procid Ast.IdMap.t
      ; varmap: reg Ast.IdMap.t
      }

    let empty =
      let procmap = Ast.IdMap.empty in
      let varmap = Ast.IdMap.empty in
      Env {procmap; varmap}


    let add_proc id procid (Env {procmap; varmap}) =
      let procmap = Ast.IdMap.add id procid procmap in
      Env {procmap; varmap}

    let add_var id reg (Env {procmap; varmap}) =
      let varmap = Ast.IdMap.add id reg varmap in
      Env {procmap; varmap}

    let lookup_proc id (Env {procmap; _}) =
      try
        Ast.IdMap.find id procmap
      with Not_found ->
        failwith @@ Format.sprintf "Unknown procedure identifier: %s" (Ast.string_of_identifier id)

    let lookup_var id (Env {varmap; _}) =
      try
        Ast.IdMap.find id varmap
      with Not_found ->
        failwith @@ Format.sprintf "Unknown variable identifier: %s" (Ast.string_of_identifier id)

  end


(* --------------------------------------------------- *)
  module Scanner = struct

    let mangle_id id =
      Format.sprintf "_I_%s" (Ast.string_of_identifier id)

    let rec mangle_texpr = function
      | Ast.TEXPR_Int _ -> "i"
      | Ast.TEXPR_Bool _ -> "b"
      | Ast.TEXPR_Array {sub;_} -> "a" ^ mangle_texpr sub

    let mangle_var_declaration v = mangle_texpr @@ Ast.type_expression_of_var_declaration v

    let mangle_formal_parameters xs = String.concat "" (List.map mangle_var_declaration xs)

    let mangle_return_types xs = String.concat "" (List.map mangle_texpr xs)

    let scan_global_declaration (env, symbols) = function
      | Ast.GDECL_Function {id; formal_parameters; return_types; _} ->
        let name = Format.sprintf "%s_%s_%s"
          (mangle_id id) (mangle_formal_parameters formal_parameters) (mangle_return_types return_types)
          in

        Environment.add_proc id (Procid name) env, Procid name :: symbols

    let scan_module env (Ast.ModuleDefinition {global_declarations; _}) =
      List.fold_left scan_global_declaration (env, []) global_declarations

  end
(* --------------------------------------------------- *)

  module type SContext = sig

    val cfg : ControlFlowGraph.t

    val node2type: (Ast.node_tag, Types.normal_type) Hashtbl.t

    val allocate_register: unit -> reg
  end

(* --------------------------------------------------- *)
  module Translator(M:SContext) = struct
    open M

    (* append instruction to basic block *)
    let append_instruction l i =
      let block = ControlFlowGraph.block cfg l in
      ControlFlowGraph.set_block cfg l (block @ [i])

    (* set terminator to unconditional jump *)
    let set_jump l_from l_to =
      ControlFlowGraph.set_terminator cfg l_from @@ T_Jump l_to;
      ControlFlowGraph.connect cfg l_from l_to

    (* set terminator to return-from-procedure *)
    let set_return l_from xs =
      ControlFlowGraph.set_terminator cfg l_from @@ T_Return xs;
      ControlFlowGraph.connect cfg l_from (ControlFlowGraph.exit_label cfg)

    (* set terminator to conditional jump *)
    let set_branch cond a b l_from l_to1 l_to2 =
      ControlFlowGraph.set_terminator cfg l_from @@ T_Branch (cond, a, b, l_to1, l_to2);
      ControlFlowGraph.connect cfg l_from l_to1;
      ControlFlowGraph.connect cfg l_from l_to2

    let allocate_block () = ControlFlowGraph.allocate_block cfg

    let typechecker_error = fun () -> failwith "Invalid type, typechecker should have failed!"

    let binop_instr res_reg lhs rhs = function
      | Ast.BINOP_Sub -> I_Sub (res_reg, lhs, rhs)
      | Ast.BINOP_Mult -> I_Mul (res_reg, lhs, rhs)
      | Ast.BINOP_Div -> I_Div (res_reg, lhs, rhs)
      | Ast.BINOP_Rem -> I_Rem (res_reg, lhs, rhs)
      | _ -> failwith "Unknown BINOP operator"

    let unop_instr res_reg sub = function
      | Ast.UNOP_Not -> I_Not (res_reg, sub)
      | Ast.UNOP_Neg -> I_Neg (res_reg, sub)

    let relop_instr = function
      | Ast.RELOP_Eq -> COND_Eq
      | Ast.RELOP_Ne -> COND_Ne
      | Ast.RELOP_Lt -> COND_Lt
      | Ast.RELOP_Gt -> COND_Gt
      | Ast.RELOP_Ge -> COND_Ge
      | Ast.RELOP_Le -> COND_Le

    (* --------------------------------------------------- *)
    let rec translate_expression env current_bb = function
      | Ast.EXPR_Char {value; _} ->
          current_bb, E_Int (Int32.of_int @@ Char.code value)

      | Ast.EXPR_Int {value; _} ->
          current_bb, E_Int value

      | Ast.EXPR_Bool {value; _} ->
          current_bb, E_Int (if value then i32_1 else i32_0)

      | Ast.EXPR_String {value; _} ->
          let str_reg = allocate_register () in
          let str_len = Int32.of_int (String.length value) in
          append_instruction current_bb @@ I_NewArray (str_reg, E_Int str_len);
          String.iteri (fun i -> fun c ->
            append_instruction current_bb @@ I_StoreArray
            (E_Reg str_reg,
             E_Int (Int32.of_int i),
             E_Int (Int32.of_int @@ Char.code c));
          ) value;
          current_bb, E_Reg str_reg

      | Ast.EXPR_Id {id; _} ->
          current_bb, E_Reg (Environment.lookup_var id env)

      | Ast.EXPR_Relation {lhs; rhs; op; _} as bin_expr ->
          (* TODO: recheck this *)
          let r = allocate_register () in
          let else_bb = allocate_block () in
          let bb' = translate_condition env current_bb else_bb bin_expr in
          append_instruction bb' @@ I_Move (r, E_Int i32_1);
          append_instruction else_bb @@ I_Move (r, E_Int i32_0);
          bb', E_Reg r

      | Ast.EXPR_Binop {op=Ast.BINOP_Or; _}
      | Ast.EXPR_Binop {op=Ast.BINOP_And; _} as bin_expr ->
          let r = allocate_register () in
          let bb_res = allocate_block () in
          let bb_else = allocate_block () in
          let bb_then = translate_condition env current_bb bb_else bin_expr in
          append_instruction bb_then @@ I_Move (r, E_Int i32_1);
          append_instruction bb_else @@ I_Move (r, E_Int i32_0);
          set_jump bb_then bb_res;
          set_jump bb_else bb_res;
          bb_res, E_Reg r

      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_Add; tag; _} ->
          let res_reg = allocate_register () in
          let current_bb, lhs_res = translate_expression env current_bb lhs in
          let current_bb, rhs_res = translate_expression env current_bb rhs in
          (match Hashtbl.find node2type tag with
            | TP_Int -> append_instruction current_bb @@
                        I_Add (res_reg, lhs_res, rhs_res)
            | TP_Array t -> append_instruction current_bb @@
                            I_Concat (res_reg, lhs_res, rhs_res)
            | _ -> typechecker_error ()
          );
          current_bb, E_Reg res_reg

      | Ast.EXPR_Binop {lhs; rhs; op; _} ->
          let res_reg = allocate_register () in
          let current_bb, lhs_res = translate_expression env current_bb lhs in
          let current_bb, rhs_res = translate_expression env current_bb rhs in
          append_instruction current_bb @@ binop_instr res_reg lhs_res rhs_res op;
          current_bb, E_Reg res_reg

      | Ast.EXPR_Unop {op; sub; _} ->
          let res_reg = allocate_register () in
          let current_bb, sub_res = translate_expression env current_bb sub in
          append_instruction current_bb @@ unop_instr res_reg sub_res op;
          current_bb, E_Reg res_reg

      | Ast.EXPR_Index {expr; index; _} ->
          let res_reg = allocate_register () in
          let current_bb, xs = translate_expression env current_bb expr in
          let current_bb, index = translate_expression env current_bb index in
          append_instruction current_bb @@ I_LoadArray (res_reg, xs, index);
          current_bb, E_Reg res_reg

      | Ast.EXPR_Struct {elements; _} ->
          let res_reg = allocate_register () in
          let struct_len = Int32.of_int (List.length elements) in
          append_instruction current_bb @@ I_NewArray (res_reg, E_Int struct_len);
          List.iteri (fun i -> fun elem ->
            let bb', res = translate_expression env current_bb elem in
            append_instruction current_bb @@ I_StoreArray
            (E_Reg res_reg,
             E_Int (Int32.of_int i),
             res);
          ) elements;
          current_bb, E_Reg res_reg

      | Ast.EXPR_Length {arg; _} ->
          let res_reg = allocate_register () in
          let current_bb, arg_res = translate_expression env current_bb arg in
          append_instruction current_bb @@ I_Length (res_reg, arg_res);
          current_bb, E_Reg res_reg

      | Ast.EXPR_Call call ->
          (match translate_call env current_bb 1 call with
            | bb, [res] -> bb, E_Reg res
            | _ -> typechecker_error ()
          )

    and translate_call env current_bb num_of_results (Call {callee; arguments; _}) =
        let n_elem_list n = (Array.make n 0 |> Array.to_list) in
        let res_regs =
          List.map (fun _ -> allocate_register ())  (n_elem_list num_of_results)
        in let procid = Environment.lookup_proc callee env in
        let current_bb, args_res = List.fold_left (fun (bb, args) -> fun arg ->
            let bb', res = translate_expression env bb arg in
            (bb', args @ [res])
        ) (current_bb,  []) arguments
        in append_instruction current_bb @@ I_Call (res_regs, procid, args_res, []);
        current_bb, res_regs

    (* --------------------------------------------------- *)
    and translate_condition env current_bb else_bb = function
      | Ast.EXPR_Bool {value=true; _} ->
        current_bb

      | Ast.EXPR_Bool {value=false; _} ->
        set_jump current_bb else_bb;
        allocate_block ()

      | Ast.EXPR_Relation {lhs; op; rhs; _} ->
          (* TODO: I don't trust this yet *)
          let r1 = allocate_register () in
          let r2 = allocate_register () in
          let bb, lhs_res = translate_expression env current_bb lhs in
          let bb', rhs_res = translate_expression env bb rhs in
          let cond = relop_instr op in
          let bb_then = allocate_block () in
          set_branch cond lhs_res rhs_res current_bb bb_then else_bb;
          bb_then

      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_Or; _} ->
          (* TODO: does it make sense? *)
          (* TODO: are bb_l and bb_right_then necessary? *)
          (* This seems like it works *)
          let bb_after = allocate_block () in
          let bb_right = allocate_block () in
          let bb_l, lhs_res = translate_expression env current_bb lhs in
          set_branch COND_Eq lhs_res (E_Int i32_1) bb_l bb_after bb_right;
          let bb_right_then, rhs_res = translate_expression env bb_right rhs in
          set_branch COND_Eq rhs_res (E_Int i32_1) bb_right_then bb_after else_bb;
          bb_after

      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_And; _} ->
          (* TODO: does it make sense? *)
          (* TODO: are bb_l and bb_right_then necessary? *)
          let bb_right = allocate_block () in
          let bb_l, lhs_res = translate_expression env current_bb lhs in
          set_branch COND_Ne lhs_res (E_Int i32_0) bb_l bb_right else_bb;
          let bb_right_then, rhs_res = translate_expression env bb_right rhs in
          let bb_after = allocate_block () in
          set_branch COND_Ne rhs_res (E_Int i32_0) bb_right_then bb_after else_bb;
          bb_after

      | e ->
        let current_bb, res = translate_expression env current_bb e in
        let next_bb = allocate_block () in
        set_branch COND_Ne res (E_Int i32_0) current_bb next_bb else_bb;
        next_bb

    (* --------------------------------------------------- *)
    let rec translate_statement env current_bb = function
      | Ast.STMT_Assign {lhs; rhs; _} ->
          (match lhs with
            | Ast.LVALUE_Id {id; _} ->
                let var = Environment.lookup_var id env in
                let current_bb, rhs_res = translate_expression env current_bb rhs in
                append_instruction current_bb @@ I_Move (var, rhs_res)

            | Ast.LVALUE_Index {sub; index; _} ->
                let current_bb, xs = translate_expression env current_bb sub in
                let current_bb, index = translate_expression env current_bb index in
                let current_bb, rhs_res = translate_expression env current_bb rhs in
                append_instruction current_bb @@ I_StoreArray (xs, index, rhs_res)
          );
          env, current_bb

      | Ast.STMT_VarDecl {var; init; _} ->
          (* TODO: we allocate new array and then... what? we should use it somewhere *)
          let get_var_type = fun (Ast.VarDecl {tp; _}) -> tp in
          let rec translate_array_decl var_type =
            (match var_type with
              | Ast.TEXPR_Array {sub; dim=None; _} -> translate_array_decl sub
              | Ast.TEXPR_Array {sub; dim=Some dim; _} ->
                  let arr = allocate_register () in
                  let current_bb, len = translate_expression env current_bb dim in
                  append_instruction current_bb @@ I_NewArray (arr, len);
                  translate_array_decl sub
              | _ -> ()
            )
          in
          translate_array_decl (get_var_type var);
          (match init with
            | Some init ->
                let current_bb, init_res = translate_expression env current_bb init in
                let ext_env, var_reg = bind_var_declaration env var in
                append_instruction current_bb @@ I_Move (var_reg, init_res);
                ext_env, current_bb
            | None ->
                let ext_env, var_reg = bind_var_declaration env var in
                ext_env, current_bb
          )

      | Ast.STMT_If { cond; then_branch; else_branch; _} ->
          let bb_else = allocate_block () in
          let bb_merge = allocate_block() in
          let bb_then = translate_condition env current_bb bb_else cond in
          set_jump bb_else bb_merge;
          set_jump bb_then bb_merge;
          let _, bb_then = translate_statement env bb_then then_branch in
          (match else_branch with
            | Some else_branch ->
                let _, _ = translate_statement env bb_else else_branch in ()
            | None -> ()
          );
          env, bb_merge

      | Ast.STMT_Block body ->
          translate_block env current_bb body

      | Ast.STMT_Return {values; _} ->
          let transl_values (current_bb, xs) value =
            let current_bb', x = translate_expression env current_bb value in
            (current_bb', xs @ [x])
          in
          let current_bb, results = List.fold_left transl_values (current_bb, []) values in
          set_return current_bb results;
          env, current_bb

      | Ast.STMT_Call call ->
          let bb', _ = translate_call env current_bb 0 call in
          env, bb'

      | Ast.STMT_While {cond; body; _} ->
          let bb_after = allocate_block () in
          let bb_cond = allocate_block () in
          set_jump current_bb bb_cond;
          let bb_then = translate_condition env bb_cond bb_after cond in
          let _, bb_body = translate_statement env bb_then body in
          set_jump bb_body bb_cond;
          env, bb_after

      | Ast.STMT_MultiVarDecl {vars; init; _} ->
          (* TODO: Refactor this fold_left *)
          let res_count = List.length vars in
          let bb', init_regs = translate_call env current_bb res_count init in
          let bb', env' = List.fold_left2 (
            fun (bb, env as acc) -> fun init_reg -> fun vardecl ->
              (match vardecl with
                | Some vardecl ->
                  let env', var_reg = bind_var_declaration env vardecl in
                  append_instruction bb @@ I_Move (var_reg, E_Reg init_reg);
                  bb, env'
                | None -> acc
              )
            ) (current_bb, env) init_regs vars
          in env', bb'

    and translate_block env current_bb (Ast.STMTBlock {body; _}) =
        let env', res_bb = List.fold_left
          (uncurry translate_statement) (env, current_bb) body
        in env', res_bb

    and bind_var_declaration env vardecl =
      let r = allocate_register () in
      let env = Environment.add_var (Ast.identifier_of_var_declaration vardecl) r env in
      env, r

    let bind_formal_parameters env xs =
      let f env x = fst (bind_var_declaration env x) in
      List.fold_left f env xs

  let translate_global_definition env = function
    | Ast.GDECL_Function {id; body=Some body; formal_parameters;_} ->
      let procid = Environment.lookup_proc id env in
      let frame_size = 0 in
      let env = bind_formal_parameters env formal_parameters in
      let formal_parameters = List.length formal_parameters in
      let proc = Procedure {procid; cfg; frame_size; allocate_register; formal_parameters} in
      let first_bb = allocate_block () in
      let _, last_bb = translate_block env first_bb body in
      ControlFlowGraph.connect cfg  last_bb (ControlFlowGraph.exit_label cfg);
      ControlFlowGraph.connect cfg  (ControlFlowGraph.entry_label cfg) first_bb;
      [proc]

    | _ ->
      []

  end

  let make_allocate_register () =
    let counter = ref 0 in
    fun () ->
      let i = !counter in
      incr counter;
      REG_Tmp i


    let translate_global_definition node2type env gdef =
      let cfg = ControlFlowGraph.create () in
      let module T = Translator(struct
        let cfg = cfg
        let node2type = node2type
        let allocate_register = make_allocate_register ()
      end) in
      T.translate_global_definition env gdef

    let translate_module node2type env (Ast.ModuleDefinition {global_declarations; _}) =
      List.flatten @@ List.map (translate_global_definition node2type env) global_declarations

    let translate_module mdef node2type =
      let env = Environment.empty in
      let env, symbols = Scanner.scan_module env mdef in
      let procedures = translate_module node2type env mdef in
      Program {procedures; symbols}
  end

