(* vim: set softtabstop=2 sw=2: *)
open Xi_lib
open Ast
open Types

module Make() = struct

  (* Logger - usage same as Format.printf *)
  let logf fmt = Logger.make_logf __MODULE__ fmt

  module Check () = struct

    module ErrorReporter = Typechecker_errors.MakeOneShotErrorReporter ()

    (* Symbol table *)
    let node2type_map = Hashtbl.create 513

    (* --------------------------------------------------- *)
    (* helper functions *)

    (* --------------------------------------------------- *)
    (* _infer_expression wrapper filling node2type_map hashtable *)
    let rec infer_expression env e =
      let tp = _infer_expression env e in
      Hashtbl.replace node2type_map (tag_of_expression e) tp;
      logf "%s: inferred type %s"
        (string_of_location
        (location_of_expression e))
        (string_of_normal_type tp);
      tp

    (* --------------------------------------------------- *)
    (* Bottom-up strategy *)
    and _infer_expression env = function
      | EXPR_Id {id; loc; _} ->
          (match TypingEnvironment.lookup id env with
            | Some (ENVTP_Var t) -> t
            | Some _ ->
                ErrorReporter.report_identifier_is_not_variable ~loc ~id
            | None ->
                ErrorReporter.report_unknown_identifier ~loc ~id
          )

      | EXPR_Int _ -> TP_Int

      | EXPR_Char _ -> TP_Int

      | EXPR_Bool _ -> TP_Bool

      | EXPR_Index {expr;index;loc; _} ->
          check_expression env TP_Int index;
          (match infer_expression env expr with
            | (TP_Array t) -> t
            | _ as t -> ErrorReporter.report_expected_array ~loc ~actual:t
          )

      | EXPR_Call call ->
          check_function_call env call

      | EXPR_Length {arg;loc;_} ->
          (match infer_expression env arg with
            | TP_Array _ -> TP_Int
            | _ as actual -> ErrorReporter.report_expected_array ~loc ~actual
          )

      | EXPR_Relation {lhs; rhs; op=RELOP_Ge; _}
      | EXPR_Relation {lhs; rhs; op=RELOP_Gt; _}
      | EXPR_Relation {lhs; rhs; op=RELOP_Lt; _}
      | EXPR_Relation {lhs; rhs; op=RELOP_Le; _}  ->
          let t = TP_Int in
          check_expression env t lhs;
          check_expression env t rhs;
          TP_Bool

      | EXPR_Relation {lhs; rhs; op=RELOP_Eq; _}
      | EXPR_Relation {lhs; rhs; op=RELOP_Ne; _} ->
          let lhs_type = infer_expression env lhs in
          check_expression env lhs_type rhs;
          TP_Bool

      | EXPR_Binop {loc; lhs; rhs; op=BINOP_Add; _} ->
        (match infer_expression env lhs with
          | (TP_Array _) as tp
          | (TP_Int as tp) ->
            check_expression env tp rhs;
            tp
          | _ ->
            let descr = "operator + expects integer or array" in
            ErrorReporter.report_other_error ~loc ~descr
        )

      | EXPR_Binop {lhs; rhs; op=BINOP_And;_}
      | EXPR_Binop {lhs; rhs; op=BINOP_Or;_} ->
          let t = TP_Bool in
          check_expression env t lhs;
          check_expression env t rhs;
          t

      | EXPR_Binop {lhs; rhs; op=BINOP_Sub;_}
      | EXPR_Binop {lhs; rhs; op=BINOP_Rem;_}
      | EXPR_Binop {lhs; rhs; op=BINOP_Mult;_}
      | EXPR_Binop {lhs; rhs; op=BINOP_Div;_} ->
          let t = TP_Int in
          check_expression env t lhs;
          check_expression env t rhs;
          t

      | EXPR_Unop {op=UNOP_Neg; sub; _} ->
          check_expression env TP_Bool sub;
          TP_Bool

      | EXPR_Unop {op=UNOP_Not; sub; _} ->
          check_expression env TP_Bool sub;
          TP_Bool

      | EXPR_String _ ->
          TP_Array TP_Int

      | EXPR_Struct {elements=[]; loc; _} ->
          ErrorReporter.report_cannot_infer ~loc

      | EXPR_Struct {elements=x::xs; _} ->
          let t = infer_expression env x in
          List.iter (check_expression env t) xs;
          TP_Array t

    and infer_var_decl env (VarDecl {loc; id; tp}) =
        let t = infer_type_expr env tp in
        let ext_env, is_new = TypingEnvironment.add id (ENVTP_Var t) env in
        if is_new then t, ext_env
                  else ErrorReporter.report_shadows_previous_definition ~loc ~id

    and infer_var_decl_dim env (VarDecl {loc; id; tp}) =
        (* TODO: This duplicates the code above! *)
        let has_dim, t = infer_type_expr_dim env tp in
        let ext_env, is_new = TypingEnvironment.add id (ENVTP_Var t) env in
        if is_new then loc, has_dim, t, ext_env
                  else ErrorReporter.report_shadows_previous_definition ~loc ~id

    and infer_type_exprs env = List.map (infer_type_expr env)

    and infer_type_expr env = function
      | TEXPR_Int _ -> TP_Int
      | TEXPR_Bool _ -> TP_Bool
      | TEXPR_Array {dim; sub; loc} ->
          (match dim with
            | Some dim ->
                ErrorReporter.report_array_initialization_forbidden ~loc
            | None ->
                TP_Array (infer_type_expr env sub)
          )

    and infer_type_expr_dim env = function
      (* TODO: This duplicates the code above *)
      | TEXPR_Int _ -> false, TP_Int
      | TEXPR_Bool _ -> false, TP_Bool
      | TEXPR_Array {dim; sub; _} ->
          (match dim with
            | Some dim ->
                check_expression env TP_Int dim;
                true, TP_Array (infer_type_expr env sub)
            | None ->
                false, TP_Array (infer_type_expr env sub)
          )

    and check_function_args env loc formal_params actual_params =
      let check = fun arg -> fun t -> check_expression env t arg in
      let expected = List.length formal_params in
      let actual = List.length actual_params in
      if expected <> actual then
        ErrorReporter.report_bad_number_of_arguments  ~loc ~actual ~expected
      else List.iter2 check actual_params formal_params

    and check_function_call env (Call { loc ; callee; arguments; _}) =
        (match TypingEnvironment.lookup callee env with
          | Some (ENVTP_Fn (param_ts, [res_t])) ->
              check_function_args env loc param_ts arguments;
              let check = fun arg -> fun t -> check_expression env t arg in
              List.iter2 check arguments param_ts;
              res_t
          | Some (ENVTP_Fn (_, t::ts)) ->
              ErrorReporter.report_expected_function_returning_one_value  ~loc ~id:callee
          | Some _ ->
              ErrorReporter.report_identifier_is_not_callable ~loc ~id:callee
          | None ->
              ErrorReporter.report_unknown_identifier ~loc ~id:callee
        )

    (* --------------------------------------------------- *)
    (* Top-down strategy: zapisz w node2type_map oczekiwanie a następnie
     * sprawdź czy nie zachodzi specjalny przypadek. *)
    and check_expression env expected e =
      logf "%s: checking expression against %s"
        (string_of_location (location_of_expression e))
        (string_of_normal_type expected);
      Hashtbl.replace node2type_map (tag_of_expression e) expected;

      match e, expected with
      | EXPR_Struct {elements; _}, TP_Array tp ->
        List.iter (check_expression env tp) elements

      (* Bottom-up strategy fallback *)
      | _ ->
        let actual = infer_expression env e in
        if actual <> expected then
          ErrorReporter.report_type_mismatch
            ~loc:(location_of_expression e)
            ~actual
            ~expected

    (* --------------------------------------------------- *)
    (* Procedure checking helper function *)

    let check_procedure_call env (Call { loc ; callee; arguments; _}) : unit =
      (match TypingEnvironment.lookup callee env with
        | Some (ENVTP_Fn (param_ts, [])) ->
            check_function_args env loc param_ts arguments;
            ()
        | Some (ENVTP_Fn (_, ts)) ->
            ErrorReporter.report_procedure_cannot_return_value ~loc
        | Some _ ->
            ErrorReporter.report_identifier_is_not_callable ~loc ~id:callee
        | None ->
            ErrorReporter.report_unknown_identifier ~loc ~id:callee
      )

    let check_multi_ret_call env (Call { loc ; callee; arguments; _}) =
      (match TypingEnvironment.lookup callee env with
        | Some (ENVTP_Fn (_, [])) ->
            ErrorReporter.report_expected_function_returning_many_values
            ~loc ~id:callee ~expected:42 ~actual:0 (* TODO: expected *)
        | Some (ENVTP_Fn (_, [t])) ->
            ErrorReporter.report_expected_function_returning_many_values
            ~loc ~id:callee ~expected:42 ~actual:1 (* TODO: expected *)
        | Some (ENVTP_Fn (param_ts, ret_ts)) ->
            check_function_args env loc param_ts arguments;
            callee, ret_ts
        | Some _ ->
            ErrorReporter.report_identifier_is_not_callable ~loc ~id:callee
        | None ->
            ErrorReporter.report_unknown_identifier ~loc ~id:callee
      )

    (* --------------------------------------------------- *)
    (* Left-value *)

    let infer_lvalue env = function
      | LVALUE_Id {id;loc;_} ->
          (* TODO: this basically duplicates EXPR_Id *)
          (match TypingEnvironment.lookup id env with
            | Some (ENVTP_Var t) -> t
            | Some _ ->
                ErrorReporter.report_identifier_is_not_variable ~loc ~id
            | None ->
                ErrorReporter.report_unknown_identifier ~loc ~id
          )
      | LVALUE_Index {index; sub; loc} ->
          check_expression env TP_Int index;
          (match infer_expression env sub with
            | (TP_Array t) -> t
            | _ as t -> ErrorReporter.report_expected_array ~loc ~actual:t
          )

    (* --------------------------------------------------- *)
    (* Statements *)

    let rec check_statement env = function
      | STMT_Assign {lhs; rhs; _} ->
        let lhs_tp = infer_lvalue env lhs in
        check_expression env lhs_tp rhs;
        env, RT_Unit

      | STMT_MultiVarDecl {vars; init; loc} ->
          (* TODO: refactor this monster *)
          let callee, ret_ts = check_multi_ret_call env init in
          let _var_decl_foldr = fun vd -> fun (ts, env) ->
            (match vd with
              | Some vd -> let t, ext_env = infer_var_decl env vd in
                           (Some t) :: ts, ext_env
              | None -> None :: ts, env
            )
          in let inf_ts, ext_env = List.fold_right _var_decl_foldr vars ([], env) in
          let check_type = fun ret_t -> fun inf_t ->
            (match inf_t with
              | Some inf_t ->
                if ret_t <> inf_t then ErrorReporter.report_type_mismatch
                                       ~loc ~actual:inf_t ~expected:ret_t
              | None -> ()
            )
          in let expected = List.length ret_ts in
          let actual = List.length inf_ts in
          if expected <> actual then
            ErrorReporter.report_expected_function_returning_many_values
              ~loc ~id:callee ~actual ~expected
          else List.iter2 check_type ret_ts inf_ts;
          ext_env, RT_Unit

      | STMT_Block body ->
        check_statement_block env body

      | STMT_Call call ->
        check_procedure_call env call;
        env, RT_Unit

      | STMT_If {cond;then_branch;else_branch; _} ->
          check_expression env TP_Bool cond;
          let _, t_res = check_statement env then_branch in
          (match else_branch with
            | Some else_branch ->
                let _, f_res = check_statement env else_branch in
                env, if t_res == f_res then t_res else RT_Unit
            | None -> env, RT_Unit
          )

      | STMT_Return {values;loc} ->
          let val_ts = List.map (infer_expression env) values in
          (match TypingEnvironment.get_return env with
            | None -> ()
            | Some ret_ts ->
              let val_ts_len = List.length val_ts in
              let ret_ts_len = List.length ret_ts in
              if val_ts_len <> ret_ts_len then
                ErrorReporter.report_bad_number_of_return_values
                  ~loc ~expected:ret_ts_len ~actual:val_ts_len
              else
                let cmp_types = fun t1 -> fun t2 ->
                  if t1 <> t2 then
                    ErrorReporter.report_type_mismatch
                      ~loc:loc ~actual:t1 ~expected:t2
                  else ()
                in List.iter2 cmp_types val_ts ret_ts;
          );
          env, RT_Void

      | STMT_VarDecl {var; init} ->
          (* TODO: This dim is damn ugly *)
          let loc, has_dim, t, ext_env = infer_var_decl_dim env var in
          (match init with
            | Some e ->
                if has_dim then
                  ErrorReporter.report_array_initialization_forbidden ~loc
                else check_expression env t e;
            | None -> ());
          ext_env, RT_Unit

      | STMT_While {cond; body; loc} ->
          check_expression env TP_Bool cond;
          let _, _  = check_statement env body in
          env, RT_Unit

    and check_statement_block env (STMTBlock {body; _}) =
          check_statement_block_aux env body

    and check_statement_block_aux env = function
          | [] -> env, RT_Unit
          | [s] -> check_statement env s
          | s :: ss ->
              let newEnv, s_res = check_statement env s in
              let loc = (location_of_statement s) in
              (match s_res with
                | RT_Unit -> check_statement_block_aux newEnv ss
                (* TODO: think about more verbose error *)
                | _ -> ErrorReporter.report_cannot_infer ~loc
              )

    (* --------------------------------------------------- *)
    (* Top-level functions *)

    let infer_formal_params env formal_parameters =
      let _var_decl_foldr = fun vd -> fun (ts, env) ->
        let t, ext_env = infer_var_decl env vd in
        t :: ts, ext_env
      in List.fold_right _var_decl_foldr formal_parameters ([], env)

    let check_global_declaration env = function
      | GDECL_Function {formal_parameters; return_types; body; loc; id; _} ->
          let _, ext_env = infer_formal_params env formal_parameters in
          (match TypingEnvironment.lookup_unsafe id env with
            | ENVTP_Fn (_, ret_types) ->
              (match body with
                | Some body ->
                  let ext_env = TypingEnvironment.set_return env ret_types in
                  (match check_statement_block ext_env body with
                    | _, RT_Void -> ()
                    | _, RT_Unit ->
                        if List.length ret_types <> 0 then
                         ErrorReporter.report_not_all_control_paths_return_value ~loc ~id
                        else ()
                  )
                | None -> ()
              )
            | _ -> ErrorReporter.report_identifier_is_not_callable ~loc ~id
          )

    let scan_global_declaration env = function
      | GDECL_Function {id; formal_parameters; return_types; loc; _} ->
          let param_types, _ = infer_formal_params env formal_parameters in
          let infered_rtypes = infer_type_exprs env return_types in
          let func_type = ENVTP_Fn (param_types, infered_rtypes) in
          let ext_env, is_new = TypingEnvironment.add id func_type env in
          if is_new then ext_env
                    else ErrorReporter.report_shadows_previous_definition ~loc ~id

    let scan_module env (ModuleDefinition {global_declarations; _}) =
      List.fold_left scan_global_declaration env global_declarations

    let check_module env (ModuleDefinition {global_declarations; _}) =
      List.iter (check_global_declaration env) global_declarations

    (* --------------------------------------------------- *)
    let process_module env mdef =
      (* Scan module to gather global declarations *)
      let env = scan_module env mdef in
      (* Verify global declarations *)
      check_module env mdef

    let computation mdef =
      let env = TypingEnvironment.empty in
      process_module env mdef;
      node2type_map
  end

  (* --------------------------------------------------- *)
  (* Entrypoint *)
  let check_module mdef =
    (* Create and run typechecker instance *)
    let module M = Check() in
    M.ErrorReporter.wrap M.computation mdef

end
