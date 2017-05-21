{-
-- PP4 language AST interpreter with typechecking
-}

{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
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


-- Convert Primitive to Type (while losing data stored in Primitive)
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


-- Get binary op types basing on operator
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
    infer_type env tE >>= \t ->
        infer_type env fE >>= compareTypes fE t >> return t


infer_let_expr_type :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Type
infer_let_expr_type env var varExpr expr =
    infer_type env varExpr >>= \t ->
        let extEnv = (extendEnv env var (typeToDummyPrimitive t)) in
        infer_type extEnv expr >>= \tE -> return tE


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


{-
-- Main function type checking expression before interpreting
-- [Var] - list of already defined variables (Integers)
-}
typecheck :: [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck vars expr = case infer_type env expr of
    Right Integer -> DataTypes.Ok
    Right Bool -> DataTypes.Error p "Expression should evaluate to integer!"
    Left (TypeError p msg) -> DataTypes.Error p msg
    where env = varsToEnv vars
          p = getData expr


-- Interpreter
-- =================================================================


calcBinExpr :: BinaryOperator -> p -> Primitive -> Primitive -> Either (Error p) Primitive
calcBinExpr op p eV1 eV2 = case op of
    -- Arithmetic operators
    BDiv -> if x2 == 0 then Left $ ZeroDivisionError p
            else            Right (TInt (x1 `div` x2))
    BMod -> if x2 == 0 then Left $ ZeroDivisionError p
            else            Right (TInt (x1 `mod` x2))
    BAdd -> Right $ TInt $ x1 + x2
    BSub -> Right $ TInt $ x1 - x2
    BMul -> Right $ TInt $ x1 * x2
    -- Comparison operators
    BEq -> Right $ TBool $ x1 == x2
    BNeq -> Right $ TBool $ x1 /= x2
    BLt -> Right $ TBool $ x1 < x2
    BGt -> Right $ TBool $ x1 > x2
    BLe -> Right $ TBool $ x1 <= x2
    BGe -> Right $ TBool $ x1 >= x2
    -- Logical operators
    BAnd -> Right $ TBool $ x1_b && x2_b
    BOr -> Right $ TBool $ x1_b || x2_b
    where (TInt x1) = eV1
          (TInt x2) = eV2
          (TBool x1_b) = eV1
          (TBool x2_b) = eV2


interpretBinExpr :: Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Primitive
interpretBinExpr env op e1 e2 = do
    x1 <- interpret env e1
    x2 <- interpret env e2
    calcBinExpr op p x1 x2
    where p = (getData e2)


interpretUnaryOp :: Environment -> UnaryOperator -> Expr p -> Either (Error p) Primitive
interpretUnaryOp env op e = case op of
    UNeg -> interpret env e >>= \(TInt x) -> return $ TInt (-x)
    UNot -> interpret env e >>= \(TBool x) -> return $ TBool (not x)


interpretLetExpr :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Primitive
interpretLetExpr env var var_expr expr =
    interpret env var_expr >>= \varValue ->
        let extEnv = extendEnv env var varValue in
        interpret extEnv expr


interpretCondExpr :: Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Primitive
interpretCondExpr env condE tE fE =
    interpret env condE >>= \(TBool condition) ->
        if condition then interpret env tE
        else              interpret env fE

{-
-- Interpreter function
-- Expression result is based on passed environment
-}
interpret :: Environment -> Expr p -> Either (Error p) Primitive
interpret env expr = case expr of
    ENum p n -> Right $ TInt n
    EBool p b -> Right $ TBool b
    EVar p var -> Right value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> interpretBinExpr env op expr1 expr2
    EUnary p op expr -> interpretUnaryOp env op expr
    ELet p var varExpr expr -> interpretLetExpr env var varExpr expr
    EIf p cond tE fE -> interpretCondExpr env cond tE fE


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, Primitive)
inputTupleToEnvTuple (var, n) = (var, TInt n)

inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


{-
-- Main function evaluating expression.
-- Act as a intermediary function between actual interpreter and AST from parser
-}
eval :: [(Var, Integer)] -> Expr p -> DataTypes.EvalResult
eval input expr = case result of
    Right (TInt n) -> DataTypes.Value n
    Left _ -> DataTypes.RuntimeError
    where env = inputToEnvironment input
          result = interpret env expr
