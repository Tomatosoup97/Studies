(* vim: set softtabstop=2 sw=2: *)
open Xi_lib
open Ir

let i32_0 = Int32.of_int 0
let i32_1 = Int32.of_int 1

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

    let i32_0 = Int32.of_int 0
    let i32_1 = Int32.of_int 1

    let binop_instr res_reg lhs rhs = function
      | Ast.BINOP_Sub -> I_Sub (res_reg, lhs, rhs)
      | Ast.BINOP_Mult -> I_Mul (res_reg, lhs, rhs)
      | Ast.BINOP_Div -> I_Div (res_reg, lhs, rhs)
      | Ast.BINOP_Rem -> I_Rem (res_reg, lhs, rhs)
      | _ -> failwith "Unknown BINOP operator"

    let unop_instr res_reg sub = function
      | Ast.UNOP_Not -> I_Not (res_reg, sub)
      | Ast.UNOP_Neg -> I_Neg (res_reg, sub)

    (* --------------------------------------------------- *)
    let rec translate_expression env current_bb = function
      | Ast.EXPR_Char {value; _} ->
          current_bb, E_Int (Int32.of_int @@ Char.code value)

      | Ast.EXPR_Bool {value; _} ->
          current_bb, E_Int (Int32.of_int (if value then 1 else 0))

      | Ast.EXPR_String {value; _} ->
          let str_reg = allocate_register () in
          let str_len = Int32.of_int (String.length value) in
          append_instruction current_bb @@ I_NewArray (str_reg, E_Int str_len);
          (* TODO: do I need to insert one char by one with I_StoreArray? *)
          current_bb, E_Reg str_reg

      | Ast.EXPR_Id {id; _} ->
          current_bb, E_Reg (Environment.lookup_var id env)

      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_Or; _}
      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_And; _} ->
          (* translate_condition? *)
          failwith "not yet implemented"

      | Ast.EXPR_Binop {lhs; rhs; op=Ast.BINOP_Add; tag; _} ->
          let res_reg = allocate_register () in
          let current_bb, lhs_res = translate_expression env current_bb lhs in
          let current_bb, rhs_res = translate_expression env current_bb rhs in
          (match Hashtbl.find node2type tag with
            | TP_Int -> append_instruction current_bb @@
                        I_Add (res_reg, lhs_res, rhs_res)
            | TP_Array t -> append_instruction current_bb @@
                            I_Concat (res_reg, lhs_res, rhs_res)
            | _ -> failwith "Cannot add given type, typechecker should have failed!"
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
          (* TODO
           * for each element translate_expression and append result to xs
           * allocate len(elements) array
           * for each x in xs StoreArray?
           *)
          failwith "not yet implemented"

      | Ast.EXPR_Length {arg; _} ->
          let res_reg = allocate_register () in
          let current_bb, arg_res = translate_expression env current_bb arg in
          append_instruction current_bb @@ I_Length (res_reg, arg_res);
          current_bb, E_Reg res_reg

      | _ ->
          failwith "not yet implemented"

    (* --------------------------------------------------- *)
    and translate_condition env current_bb else_bb = function
      | Ast.EXPR_Bool {value=true; _} ->
        current_bb

      | Ast.EXPR_Bool {value=false; _} ->
        set_jump current_bb else_bb;
        allocate_block ()

      (* Zaimplementuj dodatkowe przypadki *)

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
                (* TODO: recheck *)
                let var = Environment.lookup_var id env in
                let current_bb, rhs_res = translate_expression env current_bb rhs in
                append_instruction current_bb @@ I_Move (var, rhs_res)

            | Ast.LVALUE_Index {sub; index; _} ->
                let current_bb, xs = translate_expression env current_bb sub in
                let current_bb, index = translate_expression env current_bb index in
                let current_bb, rhs_res = translate_expression env current_bb rhs in
                append_instruction current_bb @@ I_StoreArray (xs, index, rhs_res)
          );
          current_bb

      | Ast.STMT_VarDecl {var; init; _} ->
          (* TODO case when var is array *)
          (* TODO: what to do with ext_env? nothing? *)
          (*
          (match Hashtbl.find node2type tag with
            | TP_Int -> ();
            | TP_Array -> append_instruction current_bb @@ I_Move (var_reg, init_res)
          )
          *)
          (match init with
            | Some init ->
                let current_bb, init_res = translate_expression env current_bb init in
                let ext_env, var_reg = bind_var_declaration env var in
                append_instruction current_bb @@ I_Move (var_reg, init_res);
                current_bb
            | None ->
                (* TODO: We do nothing here? *)
                let ext_env, var_reg = bind_var_declaration env var in
                current_bb
          )

      | Ast.STMT_If { cond; then_branch; else_branch; _} ->
          let bb_else = allocate_block () in
          let bb_then = translate_condition env current_bb bb_else cond in
          let bb_then' = translate_statement env bb_then then_branch in
          let bb_merge = allocate_block() in
          (match else_branch with
            | Some else_branch ->
                let bb_else' = translate_statement env bb_else else_branch in
                set_jump bb_else' bb_merge
            | None ->
                set_jump bb_else bb_merge
          );
          set_jump bb_then' bb_merge;
          bb_merge

      | Ast.STMT_Block body ->
          let _, current_bb = translate_block env current_bb body in
          current_bb

      | _ ->
        failwith "not yet implemented"

    and translate_block env current_bb (Ast.STMTBlock {body; _}) =
      failwith "not yet implemented"

    (* TODO: it was let, not and, does it mean that we shouldn't use it in
    * translate statement? *)
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

