{-# LANGUAGE Safe #-}
module Tests(tests) where

import DataTypes

tests :: [Test]
tests =

  -- Test Interpreter
  -- =================

  [ Test "inc"                  (SrcString "input x in x + 1")                       (Eval [42] (Value 43))
  , Test "binary_op_add"        (SrcString "42 + 17")                                (Eval [] (Value 59))
  , Test "negative_num"         (SrcString "10 - 100")                               (Eval [] (Value (-90)))
  , Test "binary_op_mod"        (SrcString "42 mod 10")                              (Eval [] (Value 2))
  , Test "binary_op_div"        (SrcString "3 div 2")                                (Eval [] (Value 1))
  , Test "operators_precedence" (SrcString "30 + 3 * 4")                             (Eval [] (Value 42))
  , Test "parentheses1"         (SrcString "(30 + 3) * 4")                           (Eval [] (Value 132))
  , Test "parentheses2"         (SrcString "input x y in (x - 2) * y + x")           (Eval [4,5] (Value 14))
  , Test "should_not_overflow"  (SrcString "input x in x * x")                       (Eval [9999999999] (Value 99999999980000000001))
  -- , Test "zero_division"        (SrcString "input x in x div 0")                     (Eval [42] RuntimeError )

  , Test "let"                  (SrcString "let x = 12 in x + 30")                   (Eval [] (Value 42))
  , Test "let_override"         (SrcString "input x in let x = 1 in x + 1")          (Eval [42] (Value 2))
  , Test "nested_let"           (SrcString "let x = 1 in let y = 2 in x + y")        (Eval [] (Value 3))

  , Test "if_true"              (SrcString "if true then 1 else 0")                  (Eval [] (Value 1))
  , Test "if_false"             (SrcString "if false then 1 else 0")                 (Eval [] (Value 0))
  , Test "let_with_if"          (SrcString "let x = if true then 1 else 0 in x + 1") (Eval [] (Value 2))
  , Test "comparison_gt"        (SrcString "if 1 > 2 then 1 else 0")                 (Eval [] (Value 0))
  , Test "comparison_gte"       (SrcString "if 2 >= 2 then 1 else 0")                (Eval [] (Value 1))
  , Test "comparison_neq2"      (SrcString "if 2 <> 2 then 1 else 0")                (Eval [] (Value 0))
  , Test "logical_op"           (SrcString "if true and false then 1 else 0")        (Eval [] (Value 0))
  , Test "logical_op2"          (SrcString "input x in if x <> 2 or x = 0 then 1 else 0") (Eval [2] (Value 0))

  , Test "unary_op"             (SrcString "input x in -x")                          (Eval [42] (Value (-42)))
  , Test "unary_op_not"         (SrcString "if not true then 1 else 0 ")             (Eval [] (Value 0))

  , Test "boolean_var"          (SrcString "let x = true in if x then 1 else 0")     (Eval [] (Value 1))
  , Test "boolean_var2"         (SrcString "let x = 1 <> 2 in if x then 1 else 0")   (Eval [] (Value 1))

  , Test "comment"              (SrcString "input x (* comment *) in x")             (Eval [42] (Value (42)))

  -- Test Type checker
  -- =================

  , Test "undefVar"         (SrcString "x")                                         TypeError
  , Test "minusBool"        (SrcString "if -true then 1 else 0")                    TypeError
  , Test "mod_bool"         (SrcString "let x = true in if x then x mod 2 else 0")  TypeError

  , Test "notInteger"       (SrcString "if not 1 then 1 else 0")                    TypeError
  , Test "orInteger"        (SrcString "if 1 or 2 then 1 else 0")                   TypeError
  , Test "and_op_numbers"   (SrcString "if 1 and 2 then 1 else 0")                  TypeError
  , Test "if_result"        (SrcString "if true then true else false")              TypeError

  , Test "let_bool"         (SrcString "let x = true in x + 1")                     TypeError
  , Test "int_input"        (SrcString "input x in if x then 1 else 0")             TypeError

  , Test "compare_bool_gt"  (SrcString "if true > false then 1 else 0")             TypeError
  , Test "compare_bool_gte" (SrcString "if true >= false then 1 else 0")            TypeError

  , Test "complex_check"    (SrcFile "programs/program_typeerror.pp4")              TypeError

  -- Integration tests with more complex pp4 programs
  -- =================

  , Test "complex_program"  (SrcFile "programs/program.pp4") (Eval [1, 2, 3] (Value (5)))
  , Test "max_of_three"     (SrcFile "programs/max_of_three.pp4") (Eval [17, 42, 20] (Value (42)))
  ]
