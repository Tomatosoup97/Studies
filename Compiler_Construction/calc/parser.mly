%token <int> INT
%token PLUS MINUS TIMES DIV
%token EOL
(* Priorities *)
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */


%start <int> main

%%

main:
| e = expr EOL                  { e }

(* Task 5 *)
(*
expr:
| i = INT                       { i }
| e1 = expr PLUS e2 = expr      { e1 + e2 }
| e1 = expr MINUS e2 = expr     { e1 - e2 }
| e1 = expr TIMES e2 = expr     { e1 * e2 }
| e1 = expr DIV e2 = expr       { e1 / e2 }
*)

(* Task 6 *)
expr:
| i = INT                        { i }
| e1 = expr op=op e2 = expr      { op e1 e2 }

%inline op:
| PLUS      { ( + ) }
| MINUS     { ( - ) }
| TIMES     { ( * ) }
| DIV       { ( / ) }

(* Task 7 *)
(*
expr:
| add_expr { $1 }

add_expr:
| e1 = add_expr PLUS e2 = mult_expr     { e1 + e2 }
| e1 = add_expr MINUS e2 = mult_expr    { e1 - e2 }
| e = mult_expr                         { e }

mult_expr:
| e1 = mult_expr TIMES e2 = term        { e1 * e2 }
| e1 = mult_expr DIV e2 = term          { e1 / e2 }
| t = term                              { t }

term:
| i = INT                               { i }
*)

