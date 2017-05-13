{-# LANGUAGE Safe #-}
module Tests(tests) where

import DataTypes

tests :: [Test]
tests =
  [ Test "inc" (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "calc" (SrcString "42 + 17") (Eval [] (Value 59))
  , Test "binary_op_mod" (SrcString "42 mod 10") (Eval [] (Value 2))
  , Test "binary_op_div" (SrcString "3 div 2") (Eval [] (Value 1))
  , Test "operators_precedence" (SrcString "30 + 3 * 4") (Eval [] (Value 42))
  , Test "let" (SrcString "let x = 12 in x + 30") (Eval [] (Value 42))
  , Test "let_should_override_input" (SrcString "input x in let x = 1 in x + 1") (Eval [42] (Value 2))
  , Test "nested_let" (SrcString "let x = 1 in let y = 2 in x + y") (Eval [] (Value 3))
  , Test "if" (SrcString "if true then 1 else 0") (Eval [] (Value 1))
  , Test "let_with_if" (SrcString "let x = if true then 1 else 0 in x + 1") (Eval [] (Value 2))
  , Test "comparison_op" (SrcString "if 1 > 2 then 1 else 0") (Eval [] (Value 0))
  , Test "logical_op" (SrcString "if true and false then 1 else 0") (Eval [] (Value 0))
  , Test "logical_op2" (SrcString "input x in if x <> 2 or x = 0 then 1 else 0") (Eval [2] (Value 0))
  , Test "unary_op" (SrcString "input x in -x") (Eval [42] (Value (-42)))
  , Test "unary_op_not" (SrcString "if not true then 1 else 0 ") (Eval [] (Value 0))
  , Test "bool_var" (SrcString "let x = true in if x then 1 else 0") (Eval [] (Value 1))
  -- Test Type checker
  -- =================
  -- , Test "undefVar" (SrcString "x")                TypeError
  ]
