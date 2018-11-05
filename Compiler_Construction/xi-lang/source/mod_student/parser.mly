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

let mkIdExpr = fun stp -> fun id -> EXPR_Id {
    tag=mkTag (); loc=mkLocation stp; id=id
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
%token NOT
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
    | identifier formal_parameters type_expressions_list statement_block?
    { GDECL_Function {
        loc=mkLocation $startpos;
        id=$1;
        formal_parameters=$2;
        return_types=$3;
        body=$4
    } }

formal_parameters:
    | LPAREN var_declarations RPAREN { $2 }

var_declarations:
    | { [] }
    | var_declaration   { [$1] }
    | vd=var_declaration COMMA vds=var_declarations { vd :: vds }

var_declaration:
    | identifier COLON type_expression
    { VarDecl {
        loc=mkLocation $startpos;
        id=$1;
        tp=$3
    } }

type_expressions_list:
    | { [] }
    | COLON ts=type_expressions { ts }

type_expressions:
    | type_expression { [$1] }
    | t=type_expression COMMA ts=type_expressions { t :: ts }

type_expression:
    | type_node { $1 }
    | type_expression LBRACKET expression? RBRACKET
    { TEXPR_Array {
            loc=mkLocation $startpos;
            sub=$1;
            dim=$3
    } }

type_node:
    | INT_T     { TEXPR_Int { loc=mkLocation $startpos } }
    | BOOL_T    { TEXPR_Bool { loc=mkLocation $startpos } }

statement_block:
    | LBRACE sts=statements_list RBRACE
    { STMTBlock { loc=mkLocation $startpos; body=sts } }

statements_list:
    |                                   { [] }
    | return_statement SEMICOLON?       { [$1] }
    | st=statement SEMICOLON? sts=statements_list  { st :: sts }

statement:
    | IF cond=expression tE=statement
    { mkIfStmt $startpos cond tE None }
    | IF cond=expression tE=stmt2 ELSE fE=statement
    { mkIfStmt $startpos cond tE (Some fE) }
    | WHILE cond=expression body=statement
    { mkWhileStmt $startpos cond body }
    | simple_statement { $1 }

stmt2:
    | simple_statement { $1 }
    | IF cond=expression tE=stmt2 ELSE fE=stmt2
    { mkIfStmt $startpos cond tE (Some fE) }

%inline parens_expr:
    | expression { $1 }
    | LPAREN expression RPAREN { $2 }

simple_statement:
    | function_call
    { STMT_Call $1 }
    | var_declaration assign_expr?
    { STMT_VarDecl { var=$1; init=$2 } }
    | lvalue assign_expr
    { STMT_Assign { loc=mkLocation $startpos; lhs=$1; rhs=$2 } }
    | multi_var_decl { $1 }
    | statement_block
    { STMT_Block $1 }

%inline return_statement:
    | RET exprs=expressions
    { STMT_Return { loc=mkLocation $startpos; values=exprs } }

function_call:
    | id=identifier LPAREN exprs=expressions RPAREN
    { Call {
        tag=mkTag ();
        loc=mkLocation $startpos;
        callee=id;
        arguments=exprs
    } }

%inline assign_expr:
    | ASSIGN e=expression { e }

lvalue:
    | identifier
    { LVALUE_Id { loc=mkLocation $startpos; id=$1 } }
    | array_expr LBRACKET expression RBRACKET
    { LVALUE_Index { loc=mkLocation $startpos; sub=$1; index=$3 } }

array_expr:
    | identifier { mkIdExpr $startpos $1 }
    | e=array_expr LBRACKET index=expression RBRACKET
    { EXPR_Index { tag=mkTag (); loc=mkLocation $startpos; expr=e; index=index } }

multi_var_decl:
    | vds=nullable_var_declarations ASSIGN call=function_call
    { STMT_MultiVarDecl { loc=mkLocation $startpos; vars=vds; init=call } }

nullable_var_declarations:
    | maybe_var_decl COMMA maybe_var_decl     { [$1; $3] }
    | vd=maybe_var_decl COMMA vds=nullable_var_declarations { vd :: vds }

maybe_var_decl:
    | UNDERSCORE        { None }
    | var_declaration   { Some $1 }

expressions:
    | { [] }
    | expression { [$1] }
    | e=expression COMMA exprs=expressions { e :: exprs }

expression:
    | exprA { $1 }

exprA:
    | exprB { $1 }
    | e1=exprA BIN_OR e2=exprB { mkBinOp $startpos BINOP_Or e1 e2 }

exprB:
    | exprC { $1 }
    | e1=exprB BIN_AND e2=exprC { mkBinOp $startpos BINOP_And e1 e2 }

exprC:
    | exprD { $1 }
    | exprC eq_op exprD { mkRelOp $startpos $2 $1 $3 }

exprD:
    | exprE { $1 }
    | e1=exprD op=comparison_op e2=exprE { mkRelOp $startpos op e1 e2 }

exprE:
    | exprF { $1 }
    | e1=exprE op=weak_bin_op e2=exprF { mkBinOp $startpos op e1 e2 }

exprF:
    | exprG { $1 }
    | e1=exprF op=strong_bin_op e2=exprG { mkBinOp $startpos op e1 e2 }

exprG:
    | exprH { $1 }
    | unary_op exprH { mkUnaryOp $startpos $1 $2 }

exprH:
    | identifier
    { mkIdExpr $startpos $1 }

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

    | e=exprH LBRACKET index=expression RBRACKET
    { EXPR_Index { tag=mkTag (); loc=mkLocation $startpos; expr=e; index=index } }

    | LBRACE els=expressions RBRACE
    { EXPR_Struct { tag=mkTag (); loc=mkLocation $startpos; elements=els } }

    | LPAREN e=expression RPAREN { e }

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

unary_op:
  | NOT         { UNOP_Not } (* ! *)
  | MINUS       { UNOP_Neg } (* - *)

identifier:
    | IDENTIFIER
    { Identifier $1 }

