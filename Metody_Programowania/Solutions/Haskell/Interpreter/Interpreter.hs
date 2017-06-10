{-
-- PP6 language AST interpreter with typechecking
-}

{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import qualified DataTypes


data Primitive p
    = VUnit
    | VNil Type
    | VInt Integer
    | VBool Bool
    | VPair (Primitive p) (Primitive p)
    | VList [Primitive p]
    | VClosure (Environment p) (FuncRecord p)
    | VLambda (Environment p) (FuncRecord p)


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Eq, Show)


type Environment p = Map.Map Var (Primitive p)

-- Function definitions stored in symbol table

type FuncRecord p = ((Primitive p) -> Either (Error p) (Primitive p))


-- Symbol table for types

type FuncTypeTabRecord p = (Type -> Either (Error p) Type)

type FuncTypeSymTab p = Map.Map Var (FuncTypeTabRecord p)


lookupTypeSymTab :: FuncTypeSymTab p -> Var -> Maybe (FuncTypeTabRecord p)
lookupTypeSymTab fs fsym = Map.lookup fsym fs


lookupEnv :: Environment p -> Var -> Maybe (Primitive p)
lookupEnv env x = Map.lookup x env

extendEnv :: Environment p -> Var -> Primitive p -> (Environment p)
extendEnv env var value = Map.insert var value env


-- Type checker
-- =================================================================


typeErrorMsg :: String -> String
typeErrorMsg str = "Unsupported operand type(s) for " ++ str


typeError :: Expr p -> Type -> Error p
typeError e t = TypeError (getData e) (typeErrorMsg $ show e ++ ". Expected " ++ show t)


compareTypes :: Expr p -> Type -> Type -> Either (Error p) Type
compareTypes e t1 t2
    | t1 == t2 = Right t2
    | otherwise = Left $ typeError e t1


-- Convert type to dummy primitive (used in type inference environment)
typeToPrimitive :: Type -> (Primitive p)
typeToPrimitive t = case t of
    TInt -> VInt 1
    TBool -> VBool True
    TUnit -> VUnit
    TPair t1 t2 -> VPair (typeToPrimitive t1) (typeToPrimitive t2)
    TList t -> VList [typeToPrimitive t]


-- Convert Primitive to Type (while losing data stored in Primitive)
primitiveToType :: (Primitive p) -> Type
primitiveToType p = case p of
    VBool _ -> TBool
    VUnit -> TUnit
    VInt _ -> TInt
    VNil t -> TList t
    VPair p1 p2 -> TPair (primitiveToType p1) (primitiveToType p2)
    VList [p] -> TList (primitiveToType p)


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


infer_binary_expr_type :: Environment p -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Type
infer_binary_expr_type env op e1 e2 =
    infer_type env e1 >>= compareTypes e1 expType >>
    infer_type env e2 >>= compareTypes e2 expType >>
    return resType
    where (expType, resType) = binaryOpTypes op


infer_unary_expr_type :: Environment p -> UnaryOperator -> Expr p -> Either (Error p) Type
infer_unary_expr_type env op e =
    infer_type env e >>= compareTypes e expType >> return resType
    where (expType, resType) = unaryOpTypes op


infer_cond_expr_type :: Environment p -> Expr p -> Expr p -> Expr p -> Either (Error p) Type
infer_cond_expr_type env cond tE fE =
    infer_type env cond >>= compareTypes cond TBool >>
    infer_type env tE >>= \t ->
        infer_type env fE >>= compareTypes fE t


infer_let_expr_type :: Environment p -> Var -> Expr p -> Expr p -> Either (Error p) Type
infer_let_expr_type env var varExpr expr =
    infer_type env varExpr >>= \t ->
        let extEnv = (extendEnv env var (typeToPrimitive t)) in
        infer_type extEnv expr


infer_pair_type :: Environment p -> Expr p -> Expr p -> Either (Error p) Type
infer_pair_type env e1 e2 =
    infer_type env e1 >>= \t1 ->
        infer_type env e2 >>= \t2 -> return (TPair t1 t2)


infer_cons_type :: Environment p -> Expr p -> Expr p -> Either (Error p) Type
infer_cons_type env xE xsE =
    infer_type env xE >>= \xT ->
        infer_type env xsE >>= compareTypes xsE (TList xT)


infer_fst_type :: Environment p -> Expr p -> Either (Error p) Type
infer_fst_type env e =
    infer_type env e >>= \t -> case t of
        (TPair resType _) -> return resType
        _ -> Left $ typeError e t


infer_snd_type :: Environment p -> Expr p -> Either (Error p) Type
infer_snd_type env e =
    infer_type env e >>= \t -> case t of
        (TPair _ resType) -> return resType
        _ -> Left $ typeError e t


infer_match_type :: Environment p -> Expr p -> NilClause p -> ConsClause p -> Either (Error p) Type
infer_match_type env e (nilE) (xVar, xsVar, consE) =
    infer_type env e >>= \t -> case t of
        TList lT ->
            infer_type env nilE >>= \nT ->
                let extEnvTmp = extendEnv env xVar (typeToPrimitive lT) in
                let extEnv = extendEnv extEnvTmp xsVar (typeToPrimitive t) in
                infer_type extEnv consE >>= compareTypes e nT
        _ -> Left $ typeError e t


infer_func_app_type :: Environment p -> Expr p -> Expr p -> Either (Error p) Type
infer_func_app_type env fE argE = case fE of
    EVar p var -> case (lookupEnv env var) of
        Just (VClosure cloEnv func) ->
            infer_type env argE >>= \argT -> func (typeToPrimitive argT) >>= return . primitiveToType
        _ -> invalidFuncApp
    _ -> invalidFuncApp
    where invalidFuncApp =
            Left $ TypeError (getData fE) ("Invalid function application" ++ show fE)


infer_lambda_type :: Environment p -> Var -> Type -> Expr p -> Either (Error p) Type
infer_lambda_type cloEnv var t fE =
    let extCloEnv = extendEnv cloEnv var (typeToPrimitive t)
    in infer_type extCloEnv fE >>= \fT -> return $ TArrow t fT


infer_type :: Environment p -> Expr p -> Either (Error p) Type
infer_type env e = case e of
    ENum p n -> return TInt
    EBool p b -> return TBool
    EVar p var -> case (lookupEnv env var) of
        Just val -> return $ primitiveToType val
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EUnit p -> return TUnit
    EPair p e1 e2 -> infer_pair_type env e1 e2
    ENil p t -> return t
    EFst p e -> infer_fst_type env e
    ESnd p e -> infer_snd_type env e
    ECons p e1 e2 -> infer_cons_type env e1 e2
    EMatchL p e nilC consC -> infer_match_type env e nilC consC
    EBinary p op e1 e2 -> infer_binary_expr_type env op e1 e2
    EUnary p op e -> infer_unary_expr_type env op e
    ELet p var varExpr e -> infer_let_expr_type env var varExpr e
    EIf p cond tE fE -> infer_cond_expr_type env cond tE fE
    EApp p e1 e2 -> infer_func_app_type env e1 e2
    EFn p var t fE -> infer_lambda_type env var t fE


-- Convert variable list to environment

varToEnvTuple :: Var -> (Var, (Primitive p))
varToEnvTuple var = (var, VInt 1)

varsToEnv :: [Var] -> Environment p
varsToEnv vars = Map.fromList . map varToEnvTuple $ vars


-- Convert function defs to environment.
-- It creates Map containing tuples (fsym, \arg -> resType), where function
-- check against argument type and thne return func result
funcsToEnv :: [FunctionDef p] -> Environment p
funcsToEnv fs = Map.fromList $ map toSymTabRecord fs
    where toSymTabRecord =
            (\f -> (funcName f, let emptyEnv = (Map.fromList []) in
                    VClosure emptyEnv
                        (\arg ->  compareTypes (funcBody f) (funcArgType f)
                                               (primitiveToType arg) >>
                                  return (typeToPrimitive $ funcResType f))
            ))


type_check_func :: Environment p -> FunctionDef p -> Either (Error p) Type
type_check_func fSymTab f =
    infer_type env (funcBody f) >>= compareTypes (funcBody f) (funcResType f)
    where varEnv = Map.fromList [(funcArg f, (typeToPrimitive (funcArgType f)))]
          env = Map.union fSymTab varEnv

-- Recursively iterate over function definitions and type check them
type_check_funcs :: [FunctionDef p] -> Environment p -> Either (Error p) Type
type_check_funcs [] _ = Right TInt
type_check_funcs (f:fdefs) fSymTab =
    type_check_func fSymTab f >> type_check_funcs fdefs fSymTab


{-
-- Main function type checking expression before interpretation
-- [Var] - list of already defined variables (integers)
-}
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck fs vars expr =
    case (type_check_funcs fs fSymTab, infer_type env expr) of
        (Left (TypeError p msg), _) -> DataTypes.Error p msg
        (_, Right TInt) -> DataTypes.Ok
        (_, Right _) -> DataTypes.Error p "Expression should evaluate to integer!"
        (_, Left (TypeError p msg)) -> DataTypes.Error p msg
        where varEnv = varsToEnv vars
              fSymTab = funcsToEnv fs
              env = Map.union varEnv fSymTab
              p = getData expr


-- Interpreter
-- =================================================================


calcBinExpr :: BinaryOperator -> p -> Primitive p -> Primitive p -> Either (Error p) (Primitive p)
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


interpretBinExpr :: Environment p -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretBinExpr env op e1 e2 = do
    x1 <- interpret env e1
    x2 <- interpret env e2
    calcBinExpr op p x1 x2
    where p = (getData e2)


interpretUnaryOp :: Environment p -> UnaryOperator -> Expr p -> Either (Error p) (Primitive p)
interpretUnaryOp env op e = case op of
    UNeg -> interpret env e >>= \(VInt x) -> return $ VInt (-x)
    UNot -> interpret env e >>= \(VBool x) -> return $ VBool (not x)


interpretLetExpr :: Environment p -> Var -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretLetExpr env var var_expr expr =
    interpret env var_expr >>= \varValue ->
        let extEnv = extendEnv env var varValue in
        interpret extEnv expr


interpretCondExpr :: Environment p -> Expr p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretCondExpr env condE tE fE =
    interpret env condE >>= \(VBool condition) ->
        if condition then interpret env tE
        else              interpret env fE


interpretCons :: Environment p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretCons env e1 e2 =
    interpret env e1 >>= \x ->
        interpret env e2 >>= \xs -> case xs of
            VList xs -> return $ VList (x:xs)
            VNil _ -> return $ VList [x]
            VInt i -> Left $ ZeroDivisionError (getData e1)


interpretPair :: Environment p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretPair env e1 e2 =
    interpret env e1 >>= \x1 ->
        interpret env e2 >>= \x2 -> return $ VPair x1 x2


interpretMatchExpr :: Environment p -> Expr p -> NilClause p -> ConsClause p -> Either (Error p) (Primitive p)
interpretMatchExpr env e (nilE) (xVar, xsVar, consE) =
    interpret env e >>= \val -> case val of
        VNil _ -> interpret env nilE
        VList ([]) -> interpret env nilE
        VList (x:xs) ->
            let extEnvTmp = extendEnv env xVar x in
            let extEnv = extendEnv extEnvTmp xsVar (VList xs) in
            interpret extEnv consE


interpretFuncApp :: Environment p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretFuncApp env e1 e2 =
    interpret env e1 >>= \f -> case f of
        VClosure _ func -> interpret env e2 >>= func
        VLambda cloEnv func -> interpret env e2 >>= func --TODO: cloEnv ?

{-
-- Interpreter function
-- Expression result is based on passed environment
-}
interpret :: Environment p -> Expr p -> Either (Error p) (Primitive p)
interpret env expr = case expr of
    ENum p n -> return $ VInt n
    EBool p b -> return $ VBool b
    EUnit p -> return VUnit
    ENil p t -> return $ VNil t
    ECons p e1 e2 -> interpretCons env e1 e2
    EPair p e1 e2 -> interpretPair env e1 e2
    EFst p e -> interpret env e >>= \(VPair x1 _) -> return x1
    ESnd p e -> interpret env e >>= \(VPair _ x2) -> return x2
    EVar p var -> return value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> interpretBinExpr env op expr1 expr2
    EUnary p op expr -> interpretUnaryOp env op expr
    ELet p var varExpr expr -> interpretLetExpr env var varExpr expr
    EIf p cond tE fE -> interpretCondExpr env cond tE fE
    EMatchL p e nilC consC -> interpretMatchExpr env e nilC consC
    EApp p e1 e2 -> interpretFuncApp env e1 e2
    EFn p var t fE -> return $ VLambda env lambdaFunc
        where lambdaFunc = \arg -> let extCloEnv = extendEnv env var arg in interpret extCloEnv fE


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, (Primitive p))
inputTupleToEnvTuple (var, n) = (var, VInt n)

inputToEnvironment :: [(Var, Integer)] -> Environment p
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


-- Transform function definitions to symbol table
funcDefsToSymTab :: [FunctionDef p] -> Environment p
funcDefsToSymTab fs =
    let funEnv = Map.fromList $ map
            (\f -> (funcName f,
                    VClosure (Map.fromList [])
                             (\arg -> let env = Map.fromList [(funcArg f, arg)] in
                                      let e = funcBody f in
                                      interpret (Map.union env funEnv) e))
            ) fs
    in funEnv


{-
-- Main function evaluating expression.
-- Act as a intermediary function between actual interpreter and AST from parser
-}
eval :: [FunctionDef p] -> [(Var, Integer)] -> Expr p -> DataTypes.EvalResult
eval fs input expr = case result of
    Right (VInt n) -> DataTypes.Value n
    Left _ -> DataTypes.RuntimeError
    where varEnv = inputToEnvironment input
          fSymTab = funcDefsToSymTab fs
          env = Map.union varEnv fSymTab
          result = interpret env expr
