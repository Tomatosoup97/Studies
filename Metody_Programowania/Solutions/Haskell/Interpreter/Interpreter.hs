{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import DataTypes


data Primitive
    = Num Integer
    | Boolean Bool
    deriving (Show)


data Error p
    = String p
    deriving (Show)


type Environment = Map.Map Var Primitive

-- Wrapper around Data.Map lookup
lookupEnv :: Environment -> String -> Primitive
lookupEnv env x = case Map.lookup x env of
    Just n -> n
    Nothing -> error "Unbound variable"

extendEnv :: Environment -> Var -> Primitive -> Environment
extendEnv env var value = Map.insert var value env

-- Type checker
-- ==============

-- is_binary_op :: Environment -> BinaryOperator -> Expr e1 -> Expr e2 ->
--                 TypeCheckResult
-- is_binary_op env op expr1 expr2 =
--     is_arithmetic_op(op)

-- infer_type :: Environment -> Expr p -> Either (Error p) Primitive
-- infer_type env expr = case expr of
--     -- ENum p n -> Num n
--     -- EBool p b -> Boolean b
--     EVar p var -> lookupEnv env var
--     EBinary p op expr1 expr2 -> is_binary_op env op expr1 expr2
--     EUnary p op expr -> is_unary_op env op expr
--     ELet p var varExpr expr -> is_let_expr env var varExpr expr
--     EIf p condition trueExpr falseExpr ->
--         is_conditional_expr env condition trueExpr falseExpr


-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int
typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck var expr = Ok

-- Interpreter
-- ==============

binary_op :: Environment -> BinaryOperator -> Expr e1 -> Expr e2 -> Primitive
binary_op env op expr1 expr2 = case op of
    -- Arithmetic operators
    BAdd -> Num (expr1_val + expr2_val)
    BSub -> Num (expr1_val - expr2_val)
    BMul -> Num (expr1_val * expr2_val)
    BDiv -> case expr2_val of
        0 -> error "Zero division"
        _ -> Num (expr1_val `div` expr2_val)
    BMod -> Num (expr1_val `mod` expr2_val)
    -- Comparison operators
    BEq -> Boolean (expr1_val == expr2_val)
    BNeq -> Boolean (expr1_val /= expr2_val)
    BLt -> Boolean (expr1_val < expr2_val)
    BGt -> Boolean (expr1_val > expr2_val)
    BLe -> Boolean (expr1_val <= expr2_val)
    BGe -> Boolean (expr1_val >= expr2_val)
    -- Logical operators
    BAnd -> Boolean (expr1_val_b && expr2_val_b)
    BOr -> Boolean (expr1_val_b || expr2_val_b)
    where
    Boolean expr1_val_b = interpret env expr1
    Boolean expr2_val_b = interpret env expr2
    Num expr1_val = interpret env expr1
    Num expr2_val = interpret env expr2


unary_op :: Environment -> UnaryOperator -> Expr e -> Primitive
unary_op env op expr = case op of
    UNeg -> Num (-expr_val) where Num expr_val = interpret env expr
    UNot -> Boolean (not expr_val) where Boolean expr_val = interpret env expr


let_expr :: Environment -> Var -> Expr e1 -> Expr e2 -> Primitive
let_expr env var var_expr expr = interpret extended_env expr
    where var_value = interpret env var_expr
          extended_env = extendEnv env var var_value


conditional_expr :: Environment -> Expr cond -> Expr trueExpr ->
                    Expr falseExpr -> Primitive
conditional_expr env condition_expr trueExpr falseExpr =
    if condition then
        interpret env trueExpr
    else interpret env falseExpr
    where Boolean condition = interpret env condition_expr


-- Main function interpreting AST
interpret :: Environment -> Expr p -> Primitive
interpret env expr = case expr of
    ENum p n -> Num n
    EBool p b -> Boolean b
    EVar p var -> lookupEnv env var
    EBinary p op expr1 expr2 -> binary_op env op expr1 expr2
    EUnary p op expr -> unary_op env op expr
    ELet p var varExpr expr -> let_expr env var varExpr expr
    EIf p condition trueExpr falseExpr ->
        conditional_expr env condition trueExpr falseExpr


-- Transform input to internal environment
integerToPrimitive :: (Var, Integer) -> (Var, Primitive)
integerToPrimitive (var, n) = (var, Num n)


inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = (Map.fromList . map integerToPrimitive) input


eval :: [(Var, Integer)] -> Expr p -> EvalResult
eval input expr = case result of
    Num n -> Value n
    Boolean b -> RuntimeError
    where env = inputToEnvironment input
          result = interpret env expr
