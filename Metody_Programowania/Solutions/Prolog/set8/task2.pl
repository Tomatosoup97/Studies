
expression(Expr) -->
    simple_expression(SimpleExpr),
    expression_aux(SimpleExpr, Expr).

expression_aux(Acc, Expr) -->
    "*", !, simple_expression(SimpleExpr),
    { Acc1 = (Acc, '*', SimpleExpr) },
    expression_aux(Acc1, Expr).

expression_aux(Acc, Acc) --> [].

simple_expression(a) --> "a", !.

simple_expression(b) --> "b", !.

simple_expression(Expr) --> "(", expression(Expr), ")".

