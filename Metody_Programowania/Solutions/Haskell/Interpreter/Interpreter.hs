{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import qualified DataTypes


data Primitive
    = Num Integer
    | Boolean Bool
    deriving (Show)


data Type
    = Integer
    | Bool
    deriving (Show)


typeErrorMsg :: String -> String
typeErrorMsg str = "Unsupported operand type(s) for " ++ str


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Show)


type Environment = Map.Map Var Primitive


lookupEnv :: Environment -> String -> Maybe Primitive
lookupEnv env x = Map.lookup x env


extendEnv :: Environment -> Var -> Primitive -> Environment
extendEnv env var value = Map.insert var value env


-- Type checker
-- =================================================================


is_logical_op :: BinaryOperator -> Bool
is_logical_op op = any (op ==) [BAnd, BOr]

is_comparison_op :: BinaryOperator -> Bool
is_comparison_op op = any (op ==) [BEq, BNeq, BLt, BGt, BLe, BGe]

is_arithmetic_op :: BinaryOperator -> Bool
is_arithmetic_op op = any (op ==) [BAdd, BSub, BMul, BDiv, BMod]


is_binary_expr :: Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Type
is_binary_expr env op expr1 expr2
    | is_arithmetic_op op = case infer_type env expr1 of
        Right Integer -> case infer_type env expr2 of
            Right Integer -> Right Integer
            _ -> Left $ TypeError (getData expr2) (typeErrorMsg $ show op)
        _ -> Left $ TypeError (getData expr1) (typeErrorMsg $ show op)

    | is_comparison_op op  = case infer_type env expr1 of
        Right Integer -> case infer_type env expr2 of
            Right Integer -> Right Bool
            _ -> Left $ TypeError (getData expr2) (typeErrorMsg $ show op)
        _ -> Left $ TypeError (getData expr1) (typeErrorMsg $ show op)

    | is_logical_op op = case infer_type env expr1 of
        Right Bool -> case infer_type env expr2 of
            Right Bool -> Right Bool
            _ -> Left $ TypeError (getData expr2) (typeErrorMsg $ show op)
        _ -> Left $ TypeError (getData expr1) (typeErrorMsg $ show op)


is_unary_expr :: Environment -> UnaryOperator -> Expr p -> Either (Error p) Type
is_unary_expr env op expr = case op of
    UNot -> case expr_type of
        Right Integer -> Left $ TypeError pos (typeErrorMsg $ show op)
        Right Bool -> Right Bool
    UNeg -> case expr_type of
        Right Integer -> Right Integer
        Right Bool -> Left $ TypeError pos (typeErrorMsg $ show op)
    where expr_type = infer_type env expr
          pos = getData expr


is_conditional_expr :: Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Type
is_conditional_expr env condition ifTrueExpr elseExpr = case infer_type env condition of
    Right Bool -> case infer_type env ifTrueExpr of
        Right _ -> case infer_type env elseExpr of
            Right Integer -> Right Integer
            Right Bool -> Right Bool
            Left (TypeError p msg) -> Left $ TypeError p msg
        Left (TypeError p msg) -> Left $ TypeError p msg
    Right Integer -> Left $ TypeError (getData condition) (typeErrorMsg $ "condition")
    Left (TypeError p msg) -> Left $ TypeError p msg


is_let_expr :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Type
is_let_expr env var varExpr expr = case infer_type env varExpr of
    Right Bool -> case infer_type extended_env expr of
        Right Bool -> Right Bool
        Right Integer -> Right Integer
        Left (TypeError p msg) -> Left $ TypeError p msg
        where extended_env = extendEnv env var (Boolean True)
    Right Integer -> case infer_type extended_env expr of
        Right Bool -> Right Bool
        Right Integer -> Right Integer
        Left (TypeError p msg) -> Left $ TypeError p msg
        where extended_env = extendEnv env var (Num 1)
    Left (TypeError p msg) -> Left $ TypeError p msg


infer_type :: Environment -> Expr p -> Either (Error p) Type
infer_type env expr = case expr of
    ENum p n -> Right Integer
    EBool p b -> Right Bool
    EVar p var -> case (lookupEnv env var) of
        Just val -> case val of
            Boolean b -> Right Bool
            Num b -> Right Integer
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EBinary p op expr1 expr2 -> is_binary_expr env op expr1 expr2
    EUnary p op expr -> is_unary_expr env op expr
    ELet p var varExpr expr -> is_let_expr env var varExpr expr
    EIf p condition ifTrueExpr elseExpr ->
        is_conditional_expr env condition ifTrueExpr elseExpr


varToEnvTuple :: Var -> (Var, Primitive)
varToEnvTuple var = (var, Num 1)

varsToEnv :: [Var] -> Environment
varsToEnv vars = Map.fromList . map varToEnvTuple $ vars


typecheck :: [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck vars expr = case infer_type env expr of
    Right Integer -> DataTypes.Ok
    Right Bool -> DataTypes.Error p "Expression should evaluate to integer!"
    Left (TypeError p msg) -> DataTypes.Error p msg
    where env = varsToEnv vars
          p = getData expr


-- Interpreter
-- =================================================================


binary_op :: Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Primitive
binary_op env op expr1 expr2 = case op of
    -- Arithmetic operators
    BAdd -> Right $ Num $ expr1_val + expr2_val
    BSub -> Right $ Num $ expr1_val - expr2_val
    BMul -> Right $ Num $ expr1_val * expr2_val
    BDiv -> case expr2_val of
        0 -> Left $ ZeroDivisionError (getData expr2)
        _ -> Right $ Num $ expr1_val `div` expr2_val
    BMod -> Right $ Num $ expr1_val `mod` expr2_val
    -- Comparison operators
    BEq -> Right $ Boolean $ expr1_val == expr2_val
    BNeq -> Right $ Boolean $ expr1_val /= expr2_val
    BLt -> Right $ Boolean $ expr1_val < expr2_val
    BGt -> Right $ Boolean $ expr1_val > expr2_val
    BLe -> Right $ Boolean $ expr1_val <= expr2_val
    BGe -> Right $ Boolean $ expr1_val >= expr2_val
    -- Logical operators
    BAnd -> Right $ Boolean $ expr1_val_b && expr2_val_b
    BOr -> Right $ Boolean $ expr1_val_b || expr2_val_b
    where
    Right (Boolean expr1_val_b) = interpret env expr1
    Right (Boolean expr2_val_b) = interpret env expr2
    Right (Num expr1_val) = interpret env expr1
    Right (Num expr2_val) = interpret env expr2


unary_op :: Environment -> UnaryOperator -> Expr p -> Either (Error p) Primitive
unary_op env op expr = case op of
    UNeg -> Right $ Num (-expr_val)
        where Right (Num expr_val) = interpret env expr
    UNot -> Right $ Boolean (not expr_val)
        where Right (Boolean expr_val) = interpret env expr


let_expr :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Primitive
let_expr env var var_expr expr = interpret extended_env expr
    where Right var_value = interpret env var_expr
          extended_env = extendEnv env var var_value


conditional_expr :: Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Primitive
conditional_expr env condition_expr trueExpr falseExpr =
    if condition then
        interpret env trueExpr
    else interpret env falseExpr
    where Right (Boolean condition) = interpret env condition_expr


-- Main function interpreting AST
interpret :: Environment -> Expr p -> Either (Error p) Primitive
interpret env expr = case expr of
    ENum p n -> Right $ Num n
    EBool p b -> Right $ Boolean b
    EVar p var -> Right value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> case binary_op env op expr1 expr2 of
        Right (Boolean b) -> Right $ Boolean b
        Right (Num n) -> Right $ Num n
        Left (ZeroDivisionError p) -> Left $ ZeroDivisionError p
    EUnary p op expr -> unary_op env op expr
    ELet p var varExpr expr -> let_expr env var varExpr expr
    EIf p condition trueExpr falseExpr ->
        conditional_expr env condition trueExpr falseExpr


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, Primitive)
inputTupleToEnvTuple (var, n) = (var, Num n)

inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


eval :: [(Var, Integer)] -> Expr p -> DataTypes.EvalResult
eval input expr = case result of
    Right (Num n) -> DataTypes.Value n
    Right (Boolean _) -> DataTypes.RuntimeError
    Left _ -> DataTypes.RuntimeError
    where env = inputToEnvironment input
          result = interpret env expr
