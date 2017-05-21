{-
-- PP4 language AST interpreter with typechecking
-}

{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import qualified DataTypes


data Primitive
    = VInt Integer
    | VBool Bool
    deriving (Eq, Show)


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Eq, Show)


type Environment = Map.Map Var Primitive

-- Function definitions stored in symbol table
type FuncSymTab p = Map.Map FSym (FunctionDef p)


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
    TInt -> VInt 1
    TBool -> VBool True


-- Convert Primitive to Type (while losing data stored in Primitive)
primitiveToType :: Primitive -> Type
primitiveToType p = case p of
    VBool _ -> TBool
    VInt _ -> TInt


is_logical_op :: BinaryOperator -> Bool
is_logical_op op = any (op ==) [BAnd, BOr]

is_comparison_op :: BinaryOperator -> Bool
is_comparison_op op = any (op ==) [BEq, BNeq, BLt, BGt, BLe, BGe]

is_arithmetic_op :: BinaryOperator -> Bool
is_arithmetic_op op = any (op ==) [BAdd, BSub, BMul, BDiv, BMod]


-- Get binary op types basing on operator
binaryOpTypes :: BinaryOperator -> (Type, Type)
binaryOpTypes op
    | is_logical_op op = (TBool, TBool)
    | is_comparison_op op = (TInt, TBool)
    | is_arithmetic_op op = (TInt, TInt)


unaryOpTypes :: UnaryOperator -> (Type, Type)
unaryOpTypes UNot = (TBool, TBool)
unaryOpTypes UNeg = (TInt, TInt)


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
    infer_type env cond >>= compareTypes cond TBool >>
    infer_type env tE >>= \t ->
        infer_type env fE >>= compareTypes fE t >> return t


infer_let_expr_type :: Environment -> Var -> Expr p -> Expr p -> Either (Error p) Type
infer_let_expr_type env var varExpr expr =
    infer_type env varExpr >>= \t ->
        let extEnv = (extendEnv env var (typeToDummyPrimitive t)) in
        infer_type extEnv expr >>= \tE -> return tE


infer_type :: Environment -> Expr p -> Either (Error p) Type
infer_type env e = case e of
    ENum p n -> Right TInt
    EBool p b -> Right TBool
    EVar p var -> case (lookupEnv env var) of
        Just val -> Right $ primitiveToType val
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EBinary p op e1 e2 -> infer_binary_expr_type env op e1 e2
    EUnary p op e -> infer_unary_expr_type env op e
    ELet p var varExpr e -> infer_let_expr_type env var varExpr e
    EIf p cond tE fE -> infer_cond_expr_type env cond tE fE


-- Convert variable list to environment

varToEnvTuple :: Var -> (Var, Primitive)
varToEnvTuple var = (var, VInt 1)

varsToEnv :: [Var] -> Environment
varsToEnv vars = Map.fromList . map varToEnvTuple $ vars


{-
-- Main function type checking expression before interpreting
-- [Var] - list of already defined variables (integers)
-}
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck fs vars expr = case infer_type env expr of
    Right TInt -> DataTypes.Ok
    Right TBool -> DataTypes.Error p "Expression should evaluate to integer!"
    Left (TypeError p msg) -> DataTypes.Error p msg
    where env = varsToEnv vars
          p = getData expr


-- Interpreter
-- =================================================================


calcBinExpr :: BinaryOperator -> p -> Primitive -> Primitive -> Either (Error p) Primitive
calcBinExpr op p eV1 eV2 = case op of
    -- Arithmetic operators
    BDiv -> if x2 == 0 then Left $ ZeroDivisionError p
            else            Right (VInt (x1 `div` x2))
    BMod -> if x2 == 0 then Left $ ZeroDivisionError p
            else            Right (VInt (x1 `mod` x2))
    BAdd -> Right $ VInt $ x1 + x2
    BSub -> Right $ VInt $ x1 - x2
    BMul -> Right $ VInt $ x1 * x2
    -- Comparison operators
    BEq -> Right $ VBool $ x1 == x2
    BNeq -> Right $ VBool $ x1 /= x2
    BLt -> Right $ VBool $ x1 < x2
    BGt -> Right $ VBool $ x1 > x2
    BLe -> Right $ VBool $ x1 <= x2
    BGe -> Right $ VBool $ x1 >= x2
    -- Logical operators
    BAnd -> Right $ VBool $ x1_b && x2_b
    BOr -> Right $ VBool $ x1_b || x2_b
    where (VInt x1) = eV1
          (VInt x2) = eV2
          (VBool x1_b) = eV1
          (VBool x2_b) = eV2


interpretBinExpr :: FuncSymTab p -> Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Primitive
interpretBinExpr fs env op e1 e2 = do
    x1 <- interpret fs env e1
    x2 <- interpret fs env e2
    calcBinExpr op p x1 x2
    where p = (getData e2)


interpretUnaryOp :: FuncSymTab p -> Environment -> UnaryOperator -> Expr p -> Either (Error p) Primitive
interpretUnaryOp fs env op e = case op of
    UNeg -> interpret fs env e >>= \(VInt x) -> return $ VInt (-x)
    UNot -> interpret fs env e >>= \(VBool x) -> return $ VBool (not x)


interpretLetExpr :: FuncSymTab p -> Environment -> Var -> Expr p -> Expr p -> Either (Error p) Primitive
interpretLetExpr fs env var var_expr expr =
    interpret fs env var_expr >>= \varValue ->
        let extEnv = extendEnv env var varValue in
        interpret fs extEnv expr


interpretCondExpr :: FuncSymTab p -> Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Primitive
interpretCondExpr fs env condE tE fE =
    interpret fs env condE >>= \(VBool condition) ->
        if condition then interpret fs env tE
        else              interpret fs env fE

{-
-- Interpreter function
-- Expression result is based on passed environment
-}
interpret :: FuncSymTab p -> Environment -> Expr p -> Either (Error p) Primitive
interpret fs env expr = case expr of
    ENum p n -> Right $ VInt n
    EBool p b -> Right $ VBool b
    EVar p var -> Right value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> interpretBinExpr fs env op expr1 expr2
    EUnary p op expr -> interpretUnaryOp fs env op expr
    ELet p var varExpr expr -> interpretLetExpr fs env var varExpr expr
    EIf p cond tE fE -> interpretCondExpr fs env cond tE fE


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, Primitive)
inputTupleToEnvTuple (var, n) = (var, VInt n)

inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


-- Transform function definitions to symbol table

funcToSymTabRecord :: FunctionDef p -> (FSym, FunctionDef p)
funcToSymTabRecord f = (funcName f, f)

funcDefsToSymTab :: [FunctionDef p] -> FuncSymTab p
funcDefsToSymTab fs = Map.fromList . map funcToSymTabRecord $ fs

{-
-- Main function evaluating expression.
-- Act as a intermediary function between actual interpreter and AST from parser
-}
eval :: [FunctionDef p] -> [(Var, Integer)] -> Expr p -> DataTypes.EvalResult
eval fs input expr = case result of
    Right (VInt n) -> DataTypes.Value n
    Left _ -> DataTypes.RuntimeError
    where env = inputToEnvironment input
          fSymTab = funcDefsToSymTab fs
          result = interpret fSymTab env expr
