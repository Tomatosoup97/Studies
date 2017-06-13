{-# LANGUAGE Safe #-}
module Tests(tests) where

import DataTypes

tests :: [Test]
tests =

  -- Test PP4 Interpreter

  [ Test "inc"                  (SrcString "input x in x + 1")                       (Eval [42] (Value 43))
  , Test "binary_op_add"        (SrcString "42 + 17")                                (Eval [] (Value 59))
  , Test "negative_num"         (SrcString "10 - 100")                               (Eval [] (Value (-90)))
  , Test "binary_op_mod"        (SrcString "42 mod 10")                              (Eval [] (Value 2))
  , Test "binary_op_div"        (SrcString "3 div 2")                                (Eval [] (Value 1))
  , Test "operators_precedence" (SrcString "30 + 3 * 4")                             (Eval [] (Value 42))
  , Test "parentheses1"         (SrcString "(30 + 3) * 4")                           (Eval [] (Value 132))
  , Test "parentheses2"         (SrcString "input x y in (x - 2) * y + x")           (Eval [4,5] (Value 14))
  , Test "should_not_overflow"  (SrcString "input x in x * x")                       (Eval [9999999999] (Value 99999999980000000001))

  , Test "runTimeErrBasic"      (SrcString "input x in x div 0")                     (Eval [42] RuntimeError )
  , Test "runTimeErrNested"     (SrcString "(1 div 0) + 1")                          (Eval [] RuntimeError )
  , Test "runtimeErrUnary"      (SrcString "-(10 div 0)")                            (Eval [] RuntimeError)
  , Test "runTimeErrIf"         (SrcString "if (1 div 0) <> 1 then 1 else 0")        (Eval [] RuntimeError )
  , Test "runTimeErrLet"        (SrcString "let x = 1 div 0 in 0")                   (Eval [] RuntimeError )
  , Test "runTimeBasicMod"      (SrcString "1 + (1 div 0)")                          (Eval [] RuntimeError )
  , Test "runTimeLetMod"        (SrcString "let x = 1 mod 0 in 0")                   (Eval [] RuntimeError )

  , Test "let"                  (SrcString "let x = 12 in x + 30")                   (Eval [] (Value 42))
  , Test "let_override"         (SrcString "input x in let x = 1 in x + 1")          (Eval [42] (Value 2))
  , Test "nested_let"           (SrcString "let x = 1 in let y = 2 in x + y")        (Eval [] (Value 3))
  , Test "let_if_boolean"       (SrcString "let x = if true then true else false in 1") (Eval [] (Value 1))

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
  , Test "varsOverlap"          (SrcString "let x = 5 in let x = true in if x then 1 else 0") (Eval [] (Value 1))

  , Test "comment"              (SrcString "input x (* comment *) in x")             (Eval [42] (Value (42)))

  -- Test PP4 Type checker

  , Test "mod_bool"             (SrcString "let x = true in if x then x mod 2 else 0") TypeError
  , Test "undefVar"             (SrcString "x")                                      TypeError
  , Test "minusBool"            (SrcString "if -true then 1 else 0")                 TypeError

  , Test "notInteger"           (SrcString "if not 1 then 1 else 0")                 TypeError
  , Test "orInteger"            (SrcString "if 1 or 2 then 1 else 0")                TypeError
  , Test "and_op_numbers"       (SrcString "if 1 and 2 then 1 else 0")               TypeError
  , Test "if_result"            (SrcString "if true then true else false")           TypeError

  , Test "let_bool"             (SrcString "let x = true in x + 1")                  TypeError
  , Test "int_input"            (SrcString "input x in if x then 1 else 0")          TypeError

  , Test "compare_bool_gt"      (SrcString "if true > false then 1 else 0")          TypeError
  , Test "compare_bool_gte"     (SrcString "if true >= false then 1 else 0")         TypeError

  , Test "complex_check"        (SrcFile "programs/program_typeerror.pp6")           TypeError

  -- Integration tests with more complex pp4 programs

  , Test "complex_program"      (SrcFile "programs/program.pp6") (Eval [1, 2, 3] (Value (5)))
  , Test "max_of_three"         (SrcFile "programs/max_of_three.pp6") (Eval [17, 42, 20] (Value (42)))

  -- PP5 Tests
  -- =================

  , Test "nil"                  (SrcString "let x = []: int list in 1")                                (Eval [] (Value (1)))
  , Test "cons"                 (SrcString "let x = 1 :: []: int list in 1")                           (Eval [] (Value (1)))
  , Test "cons_long"            (SrcString "let x = 1 :: 2 :: 3 :: 4 :: []: int list in 1")            (Eval [] (Value (1)))
  , Test "cons_bool"            (SrcString "let x = (true :: []: bool list) in 1")                     (Eval [] (Value (1)))
  , Test "listSyntSugar"        (SrcString "let x = [1, 2, 3, 4]: int list in 1")                      (Eval [] (Value (1)))
  , Test "list_bools"           (SrcString "let x = [true, false]: bool list in 1")                    (Eval [] (Value (1)))

  , Test "unit"                 (SrcString "let x = () in 1")                                          (Eval [] (Value (1)))
  , Test "tuple"                (SrcString "let x = (1, 2) in 1")                                      (Eval [] (Value (1)))
  , Test "fst"                  (SrcString "let x = (1, 2) in fst x")                                  (Eval [] (Value (1)))
  , Test "fst_mix"              (SrcString "let x = (1, true) in fst x")                               (Eval [] (Value (1)))
  , Test "snd"                  (SrcString "let x = (1, 2) in snd x")                                  (Eval [] (Value (2)))
  , Test "fst_complex"          (SrcString "if fst (true, false) then 1 else 0")                       (Eval [] (Value (1)))
  , Test "expr_in_pair"         (SrcString "fst (10 + 2 * 3, 1)")                                      (Eval [] (Value (16)))
  , Test "snd_complex"          (SrcString "let x = snd (1, 2) in x + 2")                              (Eval [] (Value (4)))
  , Test "nested_tuples"        (SrcString "fst (fst ((1, 2), (3, 4)))")                               (Eval [] (Value (1)))
  , Test "deeply_nested"        (SrcString "snd (fst (snd (((1, 2), (3, 4)), ((5, 6), (7, 8)))))")     (Eval [] (Value (6)))

  , Test "matchNil"             (SrcString "match []:int list with [] -> 1 | x :: xs -> 0")            (Eval [] (Value (1)))
  , Test "matchCons"            (SrcString "match 1 :: []: int list with [] -> 0 | x :: xs -> 1")      (Eval [] (Value (1)))
  , Test "matchUseVars"         (SrcString "match 9 :: []: int list with [] -> 0 | x :: xs -> x")      (Eval [] (Value (9)))
  , Test "matchList"            (SrcString "match [1, 2]: int list with [] -> 1 | x :: xs -> x")       (Eval [] (Value (1)))
  , Test "matchFst"             (SrcString "match fst([1]: int list, [0]: int list) with [] -> 0 | x :: xs -> x") (Eval [] (Value (1)))

  , Test "runTimeFst"           (SrcString "fst (1 div 0, 1)")                                         (Eval [] RuntimeError )
  , Test "runTimeGreedy"        (SrcString "fst (1, 1 div 0)")                                         (Eval [] RuntimeError )
  , Test "runTimeMatch"         (SrcString "match [0, 1]: int list with [] -> 0 | x :: xs -> x div 0") (Eval [] RuntimeError )
  , Test "runTimeMatch2"        (SrcString "match [1 div 0, 1]: int list with [] -> 0 | x :: xs -> x") (Eval [] RuntimeError )
  , Test "runTimeFunction"      (SrcString "fun zeroDiv(x: int) : int = x div 0 in zeroDiv(1)")        (Eval [] RuntimeError )

  -- Test PP5 type checker

  , Test "mult_lists"           (SrcString "let x = [1]: int in let y = [2]: int list in x + y")       TypeError
  , Test "fstList"              (SrcString "fst [1, 2]: int list")                                     TypeError
  , Test "cons_not_list"        (SrcString "let x = 1 :: 2 :: [] : int list in x")                     TypeError
  , Test "list_diff_types"      (SrcString "let x = [1, true]: int list in 1")                         TypeError
  , Test "list_diff_types2"     (SrcString "let x = [1, 2, 3, 4, true]:int list in 1")                 TypeError
  , Test "add_unit"             (SrcString "() + 1")                                                   TypeError
  , Test "add_tuple"            (SrcString "(1, 2) + (3, 4)")                                          TypeError
  , Test "snd_add_bool"         (SrcString "let x = snd (true, false) in x + 2")                       TypeError
  , Test "match_pair"           (SrcString "match (1, 2) with [] -> e1 | x :: xs -> x")                TypeError
  , Test "match_bool"           (SrcString "match true with [] -> e1 | x :: xs -> x")                  TypeError

  , Test "func_invalid_arg"     (SrcFile "programs/func_invalid_arg.pp6")                              TypeError
  , Test "invalid_res_type"     (SrcFile "programs/invalid_res_type.pp6")                              TypeError

  -- Programs

  , Test "fib"                  (SrcFile "programs/fib.pp6")                                           (Eval [6] (Value (8)))
  , Test "head"                 (SrcFile "programs/head.pp6")                                          (Eval [3] (Value (5)))
  , Test "unitFunc"             (SrcFile "programs/unit_func.pp6")                                     (Eval [42] (Value (42)))

  -- PP6 Tests
  -- =================

  -- HOF = High Order Function

  , Test "HOF"                  (SrcString "fun add (x: int) : int = x + 5 in let x = add in 2")       (Eval [] (Value (2)))
  , Test "HOF_usage"            (SrcString "fun add (x: int) : int = x + 5 in let x = add in x(2)")    (Eval [] (Value (7)))
  , Test "lambda"               (SrcString "let x = fn(x: int) -> x + 5 in x(5)")                      (Eval [] (Value (10)))
  , Test "lambda2"              (SrcString "(fn(x: int) -> x + 5) 11")                                 (Eval [] (Value (16)))
  , Test "nestedLambda"         (SrcString "(fn(x: int) -> (fn(y: int) ->  x + y)) 5 3")               (Eval [] (Value (8)))
  , Test "pairOfLambdas"        (SrcString "fst ((fn(x: int) -> x + 2), (fn(x: int) -> x + 4)) 2")     (Eval [] (Value (4)))
  , Test "lambdaRunTime"        (SrcString "let foo = fn(x: int) -> x div 0 in foo(10)")               (Eval [] RuntimeError )
  , Test "ifLambda"             (SrcString "input x in (if x <> 0 then fn(x:int) -> x + 5 else fn(x:int) -> x + 2) x") (Eval [1] (Value 6))

  -- From file
  , Test "closures"             (SrcFile "programs/closures.pp6")                                      (Eval [5] (Value (16)))
  , Test "lambdaInFunc"         (SrcFile "programs/lambdaInFunc.pp6")                                  (Eval [10] (Value (15)))
  , Test "lambdaAsArg"          (SrcFile "programs/lambdaAsArg.pp6")                                   (Eval [3] (Value (11)))
  , Test "listOfLambdas"        (SrcFile "programs/listOfLambdas.pp6")                                 (Eval [1] (Value (6)))

  -- Test PP6 type checker

  , Test "HOF_add"              (SrcString "fun id (x : int) : int = x in let x = id in x + x")        TypeError
  , Test "lambdaUnboundVar"     (SrcString "let foo = fn(x: int) -> x + z in foo(10)")                 TypeError
  , Test "lambdaInvArg"         (SrcString "let foo = fn(x: bool) -> 1 in foo(1)")                     TypeError
  , Test "nameCollision"        (SrcString "fun f(x : int): int = x input x in let f = x in f f")      TypeError
  , Test "returnLambda"         (SrcString "fn(x:int) -> x + 1")                                       TypeError
  , Test "lambdaArgInvType"     (SrcFile   "programs/lambdaArgInvType.pp6")                            TypeError
  ]
