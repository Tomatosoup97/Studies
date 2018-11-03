%start <Xi_lib.Ast.module_definition> file

%{
open Xi_lib
open Ast
open Parser_utils

let mkTag =
    let i = ref 0 in
    fun () ->
        let tag = !i in
        incr i;
        NodeTag tag

let mkBinOp = fun stp -> fun op -> fun lhs -> fun rhs -> EXPR_Binop {
    tag=mkTag (); loc=mkLocation stp; op=op; lhs=lhs; rhs=rhs
}

let mkRelOp = fun stp -> fun op -> fun lhs -> fun rhs -> EXPR_Relation {
    tag=mkTag (); loc=mkLocation stp; op=op; lhs=lhs; rhs=rhs
}

let mkUnaryOp = fun stp -> fun op -> fun sub -> EXPR_Unop {
    tag=mkTag (); loc=mkLocation stp; op=op; sub=sub
}

let mkIfStmt = fun stp -> fun cond -> fun tE -> fun fE -> STMT_If {
    loc=mkLocation stp;
    cond=cond;
    then_branch=tE;
    else_branch=fE
}

let mkWhileStmt = fun stp -> fun cond -> fun body -> STMT_While {
    loc=mkLocation stp;
    cond=cond;
    body=body
}

%}

%token <int32> INT
%token <string> IDENTIFIER
%token <string> STRING
%token <char> CHAR
%token <bool> BOOL
%token INT_T
%token BOOL_T
%token PLUS
%token MINUS
%token MULT
%token DIV
%token MOD
%token BIN_AND
%token BIN_OR
%token NEG
%token ASSIGN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACKET
%token RBRACKET
%token UNDERSCORE
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
%token EOF

%%

(* TODO: standardize | {} *)
file:
    | global_declaration* EOF
    { ModuleDefinition { global_declarations=$1 } }

global_declaration:
    | identifier formal_parameters type_expressions statement_block?
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

type_expressions:
    | { [] }
    | type_expression { [$1] }
    | t=type_expression COMMA ts=type_expressions { t :: ts }

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

(* TODO: "Each statement in a block may be terminated by a semicolon"
 * Right now every statement can be terminated by a semicolon - not only in a block
 * *)
statement:
    | dangling_if_stmt SEMICOLON?       { $1 }
    | no_dangling_if_stmt SEMICOLON?    { $1 }

dangling_if_stmt:
    | IF LPAREN cond=expression RPAREN tE=simple_statement
    { mkIfStmt $startpos cond tE None }
    | IF LPAREN cond=expression RPAREN tE=dangling_if_stmt
    { mkIfStmt $startpos cond tE None }
    | IF LPAREN cond=expression RPAREN tE=no_dangling_if_stmt ELSE fE=dangling_if_stmt
    { mkIfStmt $startpos cond tE (Some fE) }
    | WHILE LPAREN cond=expression RPAREN body=dangling_if_stmt
    { mkWhileStmt $startpos cond body }

no_dangling_if_stmt:
    | simple_statement { $1 }
    | IF LPAREN cond=expression RPAREN tE=no_dangling_if_stmt ELSE fE=no_dangling_if_stmt
    { mkIfStmt $startpos cond tE (Some fE) }
    | WHILE LPAREN cond=expression RPAREN body=no_dangling_if_stmt
    { mkWhileStmt $startpos cond body }

simple_statement:
    | function_call { STMT_Call $1 }
    | vd=var_declaration init=assign_expr?
    { STMT_VarDecl { var=vd; init=init } }
    | lvalue assign_expr
    { STMT_Assign { loc=mkLocation $startpos; lhs=$1; rhs=$2 } }
(* TODO *)
(*    | RET exprs=expressions
    { STMT_Return { loc=mkLocation $startpos; values=exprs } } *)
    | multi_var_decl { $1 }
    | statement_block { STMT_Block $1 }

function_call:
    | id=identifier LPAREN exprs=expressions RPAREN
    { Call {
        tag=mkTag ();
        loc=mkLocation $startpos;
        callee=id;
        arguments=exprs
    } }

assign_expr:
    | ASSIGN e=expression  { e }

lvalue:
    | identifier { LVALUE_Id { loc=mkLocation $startpos; id=$1 } }
    | e=expression LBRACKET index=expression RBRACKET
    { LVALUE_Index { loc=mkLocation $startpos; sub=e; index=index } }

multi_var_decl:
    | vds=nullable_var_declarations ASSIGN call=function_call
    { STMT_MultiVarDecl { loc=mkLocation $startpos; vars=vds; init=call } }

(* TODO: Can i assume that there are at least two of nullable_var_decls?
 * otherwise results in shift/reduce conflict
 * *)
nullable_var_declarations:
    | vd1=var_declaration COMMA vd2=var_declaration { [Some vd1; Some vd2] }
    | UNDERSCORE COMMA vds=nullable_var_declarations { None :: vds }
    | vd=var_declaration COMMA vds=nullable_var_declarations { (Some vd) :: vds }

(* TODO: maybe rename xs -> x_list ? *)
expressions:
    | { [] }
    | expression { [$1] }
    | e=expression COMMA exprs=expressions { e :: exprs }

expression:
    | exprA { $1 }

exprA:
    | exprB { $1 }
    | exprB BIN_OR exprA { mkBinOp $startpos BINOP_Or $1 $3 }

exprB:
    | exprC { $1 }
    | exprC BIN_AND exprB { mkBinOp $startpos BINOP_And $1 $3 }

exprC:
    | exprD { $1 }
    | exprD eq_op exprC { mkRelOp $startpos $2 $1 $3 }

exprD:
    | exprE { $1 }
    | exprE comparison_op exprD { mkRelOp $startpos $2 $1 $3 }

exprE:
    | exprF { $1 }
    | exprF weak_bin_op exprE { mkBinOp $startpos $2 $1 $3 }

exprF:
    | exprG { $1 }
    | exprG strong_bin_op exprF { mkBinOp $startpos $2 $1 $3 }

exprG:
    | exprH { $1 }
(*    | unary_op exprH { mkUnaryOp $startpos $1 $2 } *)

exprH:
    | identifier
    { EXPR_Id { tag=mkTag (); loc=mkLocation $startpos; id=$1 } }

    | INT
    { EXPR_Int { tag=mkTag (); loc=mkLocation $startpos; value=$1 } }

    | CHAR
    { EXPR_Char { tag=mkTag (); loc=mkLocation $startpos; value=$1 } }

    | STRING
    { EXPR_String { tag=mkTag (); loc=mkLocation $startpos; value=$1 } }

    | BOOL
    { EXPR_Bool { tag=mkTag (); loc=mkLocation $startpos; value=$1 } }

    | LENGTH LPAREN e=expression RPAREN
    { EXPR_Length { tag=mkTag (); loc=mkLocation $startpos; arg=e } }

    | function_call { EXPR_Call $1 }

    (*
    | e=expression LBRACKET index=expression RBRACKET
    { EXPR_Index { tag=mkTag (); loc=mkLocation $startpos; expr=e; index=index } }
    *)

    (* TODO: conflict here *)
    (*
    | LBRACE els=expressions RBRACE
    { EXPR_Struct { tag=mkTag (); loc=mkLocation $startpos; elements=els } }
    *)

eq_op:
    | EQ        { RELOP_Eq }
    | NEQ       { RELOP_Ne }

comparison_op:
    | LT        { RELOP_Lt }
    | LTE       { RELOP_Le }
    | GT        { RELOP_Gt }
    | GTE       { RELOP_Ge }

weak_bin_op:
    | PLUS      { BINOP_Add }
    | MINUS     { BINOP_Sub }

strong_bin_op:
    | MULT      { BINOP_Mult }
    | DIV       { BINOP_Div }
    | MOD       { BINOP_Rem }

(* Which is NOT and which is NEG ? *)
unary_op:
  | NEG         { UNOP_Neg }
  | MINUS       { UNOP_Not }

identifier:
    | IDENTIFIER
    { Identifier $1 }

