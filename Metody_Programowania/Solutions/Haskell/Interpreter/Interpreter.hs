{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import Control.Monad
import qualified DataTypes


data Primitive
    = TInt Integer
    | TBool Bool
    deriving (Eq, Show)


data Type
    = Integer
    | Bool
    deriving (Eq, Show)


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Eq, Show)


type Environment = Map.Map Var Primitive


lookupEnv :: Environment -> String -> Maybe Primitive
lookupEnv env x = Map.lookup x env


extendEnv :: Environment -> Var -> Primitive -> Environment
extendEnv env var value = Map.insert var value env


-- Type checker
-- =================================================================


typeErrorMsg :: String -> String
typeErrorMsg str = "Unsupported operand type(s) for " ++ str


compareTypes :: Expr p -> Type -> Type -> Either (Error p) Type
compareTypes e t1 t2
    | t1 == t2 = Right t2
    | otherwise = Left $ TypeError (getData e) (typeErrorMsg $ show e ++ ". Expected " ++ show t1)


-- Convert type to dummy primitive (used in type inference environment)
typeToDummyPrimitive :: Type -> Primitive
typeToDummyPrimitive t = case t of
    Integer -> TInt 1
    Bool -> TBool True


primitiveToType :: Primitive -> Type
primitiveToType p = case p of
    TBool _ -> Bool
    TInt _ -> Integer


is_logical_op :: BinaryOperator -> Bool
is_logical_op op = any (op ==) [BAnd, BOr]

is_comparison_op :: BinaryOperator -> Bool
is_comparison_op op = any (op ==) [BEq, BNeq, BLt, BGt, BLe, BGe]

is_arithmetic_op :: BinaryOperator -> Bool
is_arithmetic_op op = any (op ==) [BAdd, BSub, BMul, BDiv, BMod]


binaryOpTypes :: BinaryOperator -> (Type, Type)
binaryOpTypes op
    | is_logical_op op = (Bool, Bool)
    | is_comparison_op op = (Integer, Bool)
    | is_arithmetic_op op = (Integer, Integer)


unaryOpTypes :: UnaryOperator -> (Type, Type)
unaryOpTypes UNot = (Bool, Bool)
unaryOpTypes UNeg = (Integer, Integer)


infer_binary_expr_type :: Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Type
infer_binary_expr_type env op e1 e2 =
    infer_type env e1 >>= compareTypes e1 expType >>
    infer_type env e2 >>= compareTypes e2 expType >>
    return resType
    where (expType, resType) = binaryOpTypes op


infer_unary_expr_type :: Environment -> UnaryOperator -> Expr p -> Either (Error p) Type
infer_unary_expr_type env op e =
    infer_type env e >>= compareTypes e expType >> return resType
    where (expType, resType) = unaryOpTypes op


infer_cond_expr_type :: Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Type
infer_cond_expr_type env cond tE fE =
    infer_type env cond >>= compareTypes cond Bool >>
    infer_type env tE >>= (\t ->
        infer_type env fE >>= compareTypes fE t >> return t
    )


infer_let_expr_type :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Type
infer_let_expr_type env var varExpr expr =
    infer_type env varExpr >>= (\t ->
        let extEnv = (extendEnv env var (typeToDummyPrimitive t)) in
        infer_type extEnv expr >>= (\tE -> return tE)
    )


infer_type :: Environment -> Expr p -> Either (Error p) Type
infer_type env e = case e of
    ENum p n -> Right Integer
    EBool p b -> Right Bool
    EVar p var -> case (lookupEnv env var) of
        Just val -> Right $ primitiveToType val
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EBinary p op e1 e2 -> infer_binary_expr_type env op e1 e2
    EUnary p op e -> infer_unary_expr_type env op e
    ELet p var varExpr e -> infer_let_expr_type env var varExpr e
    EIf p cond tE fE -> infer_cond_expr_type env cond tE fE


-- Convert variable list to environment

varToEnvTuple :: Var -> (Var, Primitive)
varToEnvTuple var = (var, TInt 1)

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
binary_op env op e1 e2 = case op of
    -- Arithmetic operators
    BAdd -> Right $ TInt $ e1_val + e2_val
    BSub -> Right $ TInt $ e1_val - e2_val
    BMul -> Right $ TInt $ e1_val * e2_val
    BDiv -> case e2_val of
        0 -> Left $ ZeroDivisionError (getData e2)
        _ -> Right $ TInt $ e1_val `div` e2_val
    BMod -> case e2_val of
        0 -> Left $ ZeroDivisionError (getData e2)
        _ -> Right $ TInt $ e1_val `mod` e2_val
    -- Comparison operators
    BEq -> Right $ TBool $ e1_val == e2_val
    BNeq -> Right $ TBool $ e1_val /= e2_val
    BLt -> Right $ TBool $ e1_val < e2_val
    BGt -> Right $ TBool $ e1_val > e2_val
    BLe -> Right $ TBool $ e1_val <= e2_val
    BGe -> Right $ TBool $ e1_val >= e2_val
    -- Logical operators
    BAnd -> Right $ TBool $ e1_val_b && e2_val_b
    BOr -> Right $ TBool $ e1_val_b || e2_val_b
    where
    Right (TBool e1_val_b) = interpret env e1
    Right (TBool e2_val_b) = interpret env e2
    Right (TInt e1_val) = interpret env e1
    Right (TInt e2_val) = interpret env e2


unary_op :: Environment -> UnaryOperator -> Expr p -> Either (Error p) Primitive
unary_op env op expr = case op of
    UNeg -> Right $ TInt (-expr_val)
        where Right (TInt expr_val) = interpret env expr
    UNot -> Right $ TBool (not expr_val)
        where Right (TBool expr_val) = interpret env expr


let_expr :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Primitive
let_expr env var var_expr expr = interpret extended_env expr
    where Right var_value = interpret env var_expr
          extended_env = extendEnv env var var_value


conditional_expr :: Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Primitive
conditional_expr env condE tE fE =
    if condition then interpret env tE
    else interpret env fE
    where Right (TBool condition) = interpret env condE


-- Main function interpreting AST
interpret :: Environment -> Expr p -> Either (Error p) Primitive
interpret env expr = case expr of
    ENum p n -> Right $ TInt n
    EBool p b -> Right $ TBool b
    EVar p var -> Right value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> binary_op env op expr1 expr2
    EUnary p op expr -> unary_op env op expr
    ELet p var varExpr expr -> let_expr env var varExpr expr
    EIf p cond tE fE -> conditional_expr env cond tE fE


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, Primitive)
inputTupleToEnvTuple (var, n) = (var, TInt n)

inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


eval :: [(Var, Integer)] -> Expr p -> DataTypes.EvalResult
eval input expr = case result of
    Right (TInt n) -> DataTypes.Value n
    Left _ -> DataTypes.RuntimeError
    where env = inputToEnvironment input
          result = interpret env expr
