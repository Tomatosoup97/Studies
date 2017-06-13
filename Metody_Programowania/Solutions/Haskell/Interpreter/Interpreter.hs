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
    | VClosure (FuncRecord p)
    | VLambda (Environment p) (FuncRecord p)


-- Internal type used in type checker
data (IType p)
  = TInt'
  | TBool'
  | TUnit'
  | TPair' (IType p) (IType p)
  | TList' (IType p)
  | TArrow' (IType p) (IType p)
  | TClosure' (FuncTypeRecord p)


instance Eq (IType p) where
    TInt' == TInt' = True
    TBool' == TBool' = True
    TUnit' == TUnit' = True
    TPair' t1 t2 == TPair' t1' t2' = t1 == t1' && t2 == t2'
    TList' t1 == TList' t2 = t1 == t2
    TArrow' t1 t2 == TArrow' t1' t2' = t1 == t1' && t2 == t2'
    TClosure' _ == TClosure' _ = True
    _ == _ = False


instance Show (IType p) where
    show (TClosure' _) = "TClosure"
    show t = show t


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Eq, Show)


type Environment p = Map.Map Var (Primitive p)

type TypeEnvironment p = Map.Map Var (IType p)

-- Function definitions stored in symbol table

type FuncRecord p = ((Primitive p) -> Either (Error p) (Primitive p))

type FuncTypeRecord p = ((IType p) -> Either (Error p) (IType p))


lookupEnv :: Environment p -> Var -> Maybe (Primitive p)
lookupEnv env x = Map.lookup x env

extendEnv :: Environment p -> Var -> Primitive p -> (Environment p)
extendEnv env var value = Map.insert var value env


lookupTypeEnv :: TypeEnvironment p -> Var -> Maybe (IType p)
lookupTypeEnv env x = Map.lookup x env

extendTypeEnv :: TypeEnvironment p -> Var -> IType p -> TypeEnvironment p
extendTypeEnv env var value = Map.insert var value env


-- Type checker
-- =================================================================


typeErrorMsg :: String -> String
typeErrorMsg str = "Unsupported operand type(s) for " ++ str


typeError :: Expr p -> (IType p) -> Error p
typeError e t = TypeError (getData e) (typeErrorMsg $ show e ++ ". Expected " ++ show t)


compareTypes :: Expr p -> (IType p) -> (IType p) -> Either (Error p) (IType p)
compareTypes e t1 t2
    | t1 == t2 = Right t2
    | otherwise = Left $ typeError e t1


-- Convert type to internal type
typeToIType :: Type -> (IType p)
typeToIType t = case t of
    TInt -> TInt'
    TBool -> TBool'
    TUnit -> TUnit'
    TPair t1 t2 -> TPair' (typeToIType t1) (typeToIType t2)
    TList t -> TList' (typeToIType t)
    TArrow t1 t2 -> TArrow' (typeToIType t1) (typeToIType t2)


is_logical_op :: BinaryOperator -> Bool
is_logical_op op = any (op ==) [BAnd, BOr]

is_comparison_op :: BinaryOperator -> Bool
is_comparison_op op = any (op ==) [BEq, BNeq, BLt, BGt, BLe, BGe]

is_arithmetic_op :: BinaryOperator -> Bool
is_arithmetic_op op = any (op ==) [BAdd, BSub, BMul, BDiv, BMod]


-- Get binary op types basing on operator
binaryOpTypes :: BinaryOperator -> ((IType p), (IType p))
binaryOpTypes op
    | is_logical_op op = (TBool', TBool')
    | is_comparison_op op = (TInt', TBool')
    | is_arithmetic_op op = (TInt', TInt')


unaryOpTypes :: UnaryOperator -> ((IType p), (IType p))
unaryOpTypes UNot = (TBool', TBool')
unaryOpTypes UNeg = (TInt', TInt')


infer_binary_expr_type :: TypeEnvironment p -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_binary_expr_type env op e1 e2 = do
    t1 <- infer_type env e1
    compareTypes e1 expType t1
    t2 <- infer_type env e2
    compareTypes e2 expType t2
    return resType
    where (expType, resType) = binaryOpTypes op


infer_unary_expr_type :: TypeEnvironment p -> UnaryOperator -> Expr p -> Either (Error p) (IType p)
infer_unary_expr_type env op e =
    infer_type env e >>= compareTypes e expType >> return resType
    where (expType, resType) = unaryOpTypes op


infer_cond_expr_type :: TypeEnvironment p -> Expr p -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_cond_expr_type env cond tE fE =
    infer_type env cond >>= compareTypes cond TBool' >>
    infer_type env tE >>= \t -> infer_type env fE >>= compareTypes fE t


infer_let_expr_type :: TypeEnvironment p -> Var -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_let_expr_type env var varExpr expr = do
    t <- infer_type env varExpr
    let extEnv = (extendTypeEnv env var t)
    infer_type extEnv expr


infer_pair_type :: TypeEnvironment p -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_pair_type env e1 e2 = do
    t1 <- infer_type env e1
    t2 <- infer_type env e2
    return (TPair' t1 t2)


infer_cons_type :: TypeEnvironment p -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_cons_type env xE xsE = do
    xT <- infer_type env xE
    xsT <- infer_type env xsE
    compareTypes xsE (TList' xT) xsT


infer_fst_type :: TypeEnvironment p -> Expr p -> Either (Error p) (IType p)
infer_fst_type env e =
    infer_type env e >>= \t -> case t of
        (TPair' resType _) -> return resType
        _ -> Left $ typeError e t


infer_snd_type :: TypeEnvironment p -> Expr p -> Either (Error p) (IType p)
infer_snd_type env e =
    infer_type env e >>= \t -> case t of
        (TPair' _ resType) -> return resType
        _ -> Left $ typeError e t


infer_match_type :: TypeEnvironment p -> Expr p -> NilClause p -> ConsClause p -> Either (Error p) (IType p)
infer_match_type env e (nilE) (xVar, xsVar, consE) =
    infer_type env e >>= \t -> case t of
        TList' lT ->
            infer_type env nilE >>= \nT ->
                let extEnvTmp = extendTypeEnv env xVar lT in
                let extEnv = extendTypeEnv extEnvTmp xsVar t in
                infer_type extEnv consE >>= compareTypes e nT
        _ -> Left $ typeError e t


infer_func_app_type :: TypeEnvironment p -> Expr p -> Expr p -> Either (Error p) (IType p)
infer_func_app_type env fE argE =
    infer_type env fE >>= \fT -> case fT of
        (TClosure' func) -> infer_type env argE >>= func
        (TArrow' t1 t2) -> infer_type env argE >>= compareTypes argE t1 >> return t2
        _ -> Left $ TypeError (getData fE) ("Invalid function application" ++ show fE)


infer_lambda_type :: TypeEnvironment p -> Var -> (IType p) -> Expr p -> Either (Error p) (IType p)
infer_lambda_type cloEnv var t fE = do
    let extCloEnv = extendTypeEnv cloEnv var t
    fT <- infer_type extCloEnv fE
    return $ TArrow' t fT


infer_type :: TypeEnvironment p -> Expr p -> Either (Error p) (IType p)
infer_type env e = case e of
    ENum p n -> return TInt'
    EBool p b -> return TBool'
    EVar p var -> case (lookupTypeEnv env var) of
        Just val -> return val
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EUnit p -> return TUnit'
    EPair p e1 e2 -> infer_pair_type env e1 e2
    ENil p t -> return (typeToIType t)
    EFst p e -> infer_fst_type env e
    ESnd p e -> infer_snd_type env e
    ECons p e1 e2 -> infer_cons_type env e1 e2
    EMatchL p e nilC consC -> infer_match_type env e nilC consC
    EBinary p op e1 e2 -> infer_binary_expr_type env op e1 e2
    EUnary p op e -> infer_unary_expr_type env op e
    ELet p var varExpr e -> infer_let_expr_type env var varExpr e
    EIf p cond tE fE -> infer_cond_expr_type env cond tE fE
    EApp p e1 e2 -> infer_func_app_type env e1 e2
    EFn p var t fE -> infer_lambda_type env var (typeToIType t) fE


-- Convert function defs to environment.
-- It creates Map containing tuples (fsym, \arg -> resType), where function
-- check against argument type and thne return func result
funcsToEnv :: [FunctionDef p] -> TypeEnvironment p
funcsToEnv fs = Map.fromList $ map toSymTabRecord fs
    where toSymTabRecord =
            (\f -> (funcName f, TClosure'
                   (\argT -> let funcArgT = typeToIType (funcArgType f)
                                 funcResT = typeToIType (funcResType f) in
                             compareTypes (funcBody f) funcArgT argT >>
                             return funcResT)
            ))


type_check_func :: TypeEnvironment p -> FunctionDef p -> Either (Error p) (IType p)
type_check_func fSymTab f =
    infer_type env (funcBody f) >>= compareTypes (funcBody f) funcResT
    where funcArgT = typeToIType (funcArgType f)
          funcResT = typeToIType (funcResType f)
          varEnv = Map.fromList [(funcArg f, funcArgT)]
          env = Map.union fSymTab varEnv

-- Recursively iterate over function definitions and type check them
type_check_funcs :: [FunctionDef p] -> TypeEnvironment p -> Either (Error p) (IType p)
type_check_funcs [] _ = Right TInt'
type_check_funcs (f:fdefs) fSymTab =
    type_check_func fSymTab f >> type_check_funcs fdefs fSymTab


-- Convert variable list to environment

varsToEnv :: [Var] -> TypeEnvironment p
varsToEnv vars = Map.fromList . map (\var -> (var, TInt')) $ vars

{-
-- Main function type checking expression before interpretation
-- [Var] - list of already defined variables (integers)
-}
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck fs vars expr =
    case (type_check_funcs fs fSymTab, infer_type env expr) of
        (Left (TypeError p msg), _) -> DataTypes.Error p msg
        (_, Right TInt') -> DataTypes.Ok
        (_, Right _) -> DataTypes.Error (getData expr) "Expression should evaluate to integer!"
        (_, Left (TypeError p msg)) -> DataTypes.Error p msg
        where varEnv = varsToEnv vars
              fSymTab = funcsToEnv fs
              env = Map.union varEnv fSymTab


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
interpretLetExpr env var var_expr expr = do
    varValue <- interpret env var_expr
    let extEnv = extendEnv env var varValue
    interpret extEnv expr


interpretCondExpr :: Environment p -> Expr p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretCondExpr env condE tE fE =
    interpret env condE >>= \(VBool condition) ->
        if condition then interpret env tE
        else              interpret env fE


interpretCons :: Environment p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretCons env e1 e2 = do
    x <- interpret env e1
    xs <- interpret env e2
    case xs of
        VList xs -> return $ VList (x:xs)
        VNil _ -> return $ VList [x]
        VInt i -> Left $ ZeroDivisionError (getData e1)


interpretPair :: Environment p -> Expr p -> Expr p -> Either (Error p) (Primitive p)
interpretPair env e1 e2 = do
    x1 <- interpret env e1
    x2 <- interpret env e2
    return $ VPair x1 x2


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
        VClosure func -> interpret env e2 >>= func
        VLambda cloEnv func -> interpret env e2 >>= func

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
inputToEnvironment :: [(Var, Integer)] -> Environment p
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input
    where inputTupleToEnvTuple (var, n) = (var, VInt n)


-- Transform function definitions to symbol table
funcDefsToSymTab :: [FunctionDef p] -> Environment p
funcDefsToSymTab fs =
    let funEnv = Map.fromList $ map
            (\f -> (funcName f,
                    VClosure (\arg -> let env = Map.fromList [(funcArg f, arg)] in
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
