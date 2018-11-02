%start <Xi_lib.Ast.module_definition> file

%{
open Xi_lib
open Ast
open Parser_utils

(* Tags generator *)
let mkTag =
    let i = ref 0 in
    fun () ->
        let tag = !i in
        incr i;
        NodeTag tag

%}

%token <int32> INT
%token <string> IDENTIFIER
%token <bool> BOOL
%token INT_T
%token BOOL_T
%token PLUS
%token MINUS
%token MULT
%token HIGH_MULT
%token DIV
%token MOD
%token BIN_AND
%token BIN_OR
%token BIN_NEG
%token ASSIGN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token ARRAY
%token IF
%token ELSE
%token LENGTH
%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
%token COMMA
%token COLON
%token SEMICOLON
%token WHILE
%token RET
%token APOSTROPHE
%token QUOT_MARK
%token EOF

%%

file:
    | global_declaration* EOF
    { ModuleDefinition { global_declarations=$1 } }

global_declaration:
    | identifier formal_parameters type_expression* statement_block?
    { GDECL_Function {
        loc=mkLocation $startpos;
        id=$1;
        formal_parameters=$2;
        return_types=$3;
        body=$4
    } }

formal_parameters:
    | LPAREN vds=var_declarations RPAREN
    { vds }

var_declarations:
    | { [] }
    | var_declaration { [$1] }
    | vd=var_declaration COMMA vds=var_declarations { vd :: vds }

var_declaration:
    | identifier type_expression
    { VarDecl {
        loc=mkLocation $startpos;
        id=$1;
        tp=$2
    } }

type_expression:
    | COLON t=type_node LBRACKET dim=expression? RBRACKET
    { TEXPR_Array {
            loc=mkLocation $startpos;
            sub=t;
            dim=dim
    } }
    | COLON t=type_node { t }

type_node:
    | INT_T { TEXPR_Int { loc=mkLocation $startpos } }
    | BOOL_T { TEXPR_Bool { loc=mkLocation $startpos } }

statement_block:
    | LBRACE sts=statement* RBRACE
    { STMTBlock { loc=mkLocation $startpos; body=sts } }

statement:
    | dangling_if_stmt      { $1 }
    | no_dangling_if_stmt   { $1 }

(* TODO: the grammar below looks ugly *)
dangling_if_stmt:
    | IF LPAREN cond=expression RPAREN tE=simple_statement
    { STMT_If {
        loc=mkLocation $startpos;
        cond=cond;
        then_branch=tE;
        else_branch=None
    } }
    | IF LPAREN cond=expression RPAREN tE=dangling_if_stmt
    { STMT_If {
        loc=mkLocation $startpos;
        cond=cond;
        then_branch=tE;
        else_branch=None
    } }
    | IF LPAREN cond=expression RPAREN tE=no_dangling_if_stmt ELSE fE=dangling_if_stmt
    { STMT_If {
        loc=mkLocation $startpos;
        cond=cond;
        then_branch=tE;
        else_branch=Some fE
    } }
    | WHILE LPAREN cond=expression RPAREN body=dangling_if_stmt
    { STMT_While {
        loc=mkLocation $startpos;
        cond=cond;
        body=body
    } }

no_dangling_if_stmt:
    | simple_statement { $1 }
    | IF LPAREN cond=expression RPAREN tE=no_dangling_if_stmt ELSE fE=no_dangling_if_stmt
    { STMT_If {
        loc=mkLocation $startpos;
        cond=cond;
        then_branch=tE;
        else_branch=Some fE
    } }
    | WHILE LPAREN cond=expression RPAREN body=no_dangling_if_stmt
    { STMT_While {
        loc=mkLocation $startpos;
        cond=cond;
        body=body
    } }

simple_statement:
    | function_call { STMT_Call $1 }
    | vd=var_declaration init=assign_expr? SEMICOLON?
    { STMT_VarDecl { var=vd; init=init } }

assign_expr:
    | ASSIGN e=expression  { e }

function_call:
    | identifier expression*
    { Call {
        tag=mkTag ();
        loc=mkLocation $startpos;
        callee=$1;
        arguments=$2
    } }

expression:
    (* TODO: only Int so far *)
    | INT
    { EXPR_Int {
        tag=mkTag ();
        loc=mkLocation $startpos;
        value=$1
    } }

identifier:
    | IDENTIFIER
    { Identifier $1 }


(*
   ** przykład użycia mkLocation

    use_declaration:
        | USE suffix(identifier, opt(SEMICOLON))
        { GDECL_Use {loc=mkLocation $startpos; id=$2} }

   ** przykład użycia mkTag

    atomic_expression:
        | identifier
        { EXPR_Id {loc=mkLocation $startpos; id=$1; tag=mkTag ()} }
*)

