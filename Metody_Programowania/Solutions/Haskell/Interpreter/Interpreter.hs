{-
-- PP4 language AST interpreter with typechecking
-}

{-# LANGUAGE Safe #-}
module Interpreter (typecheck, eval) where

import qualified Data.Map as Map

import AST
import qualified DataTypes


data Primitive
    = VUnit
    | VNil Type
    | VInt Integer
    | VBool Bool
    | VPair Primitive Primitive
    | VList [Primitive]
    deriving (Eq, Show)


data Error p
    = TypeError p DataTypes.ErrorMessage
    | ZeroDivisionError p
    deriving (Eq, Show)


type Environment = Map.Map Var Primitive

-- Function definitions stored in symbol table

type FuncTabRecord p = (Primitive -> Either (Error p) Primitive)

type FuncTypeTabRecord p = (Type -> Either (Error p) Type)

type FuncSymTab p = Map.Map FSym (FuncTabRecord p)

type FuncTypeSymTab p = Map.Map FSym (FuncTypeTabRecord p)


lookupSymTab :: FuncSymTab p -> FSym -> Maybe (FuncTabRecord p)
lookupSymTab fs fsym = Map.lookup fsym fs


lookupTypeSymTab :: FuncTypeSymTab p -> FSym -> Maybe (FuncTypeTabRecord p)
lookupTypeSymTab fs fsym = Map.lookup fsym fs


lookupEnv :: Environment -> Var -> Maybe Primitive
lookupEnv env x = Map.lookup x env


extendEnv :: Environment -> Var -> Primitive -> Environment
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
typeToPrimitive :: Type -> Primitive
typeToPrimitive t = case t of
    TInt -> VInt 1
    TBool -> VBool True
    TUnit -> VUnit
    TPair t1 t2 -> VPair (typeToPrimitive t1) (typeToPrimitive t2)
    TList t -> VList [typeToPrimitive t]

-- Convert Primitive to Type (while losing data stored in Primitive)
primitiveToType :: Primitive -> Type
primitiveToType p = case p of
    VBool _ -> TBool
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


infer_binary_expr_type :: FuncTypeSymTab p -> Environment -> BinaryOperator -> Expr p -> Expr p -> Either (Error p) Type
infer_binary_expr_type fs env op e1 e2 =
    infer_type fs env e1 >>= compareTypes e1 expType >>
    infer_type fs env e2 >>= compareTypes e2 expType >>
    return resType
    where (expType, resType) = binaryOpTypes op


infer_unary_expr_type :: FuncTypeSymTab p -> Environment -> UnaryOperator -> Expr p -> Either (Error p) Type
infer_unary_expr_type fs env op e =
    infer_type fs env e >>= compareTypes e expType >> return resType
    where (expType, resType) = unaryOpTypes op


infer_cond_expr_type :: FuncTypeSymTab p -> Environment -> Expr p -> Expr p -> Expr p -> Either (Error p) Type
infer_cond_expr_type fs env cond tE fE =
    infer_type fs env cond >>= compareTypes cond TBool >>
    infer_type fs env tE >>= \t ->
        infer_type fs env fE >>= compareTypes fE t >> return t


infer_let_expr_type :: FuncTypeSymTab p -> Environment -> Var -> Expr p -> Expr p -> Either (Error p) Type
infer_let_expr_type fs env var varExpr expr =
    infer_type fs env varExpr >>= \t ->
        let extEnv = (extendEnv env var (typeToPrimitive t)) in
        infer_type fs extEnv expr >>= \tE -> return tE


infer_pair_type :: FuncTypeSymTab p -> Environment -> Expr p -> Expr p -> Either (Error p) Type
infer_pair_type fs env e1 e2 =
    infer_type fs env e1 >>= \t1 ->
        infer_type fs env e2 >>= \t2 ->
            compareTypes e2 t1 t2 >> return (TPair t1 t2)


infer_cons_type :: FuncTypeSymTab p -> Environment -> Expr p -> Expr p -> Either (Error p) Type
infer_cons_type fs env e1 e2 =
    infer_type fs env e1 >>= \t1 ->
        infer_type fs env e2 >>= \t2 -> case t2 of
            TList lT -> compareTypes e2 t1 lT >> return (TList t2)
            _ -> Left $ typeError e1 t2


infer_fst_type :: FuncTypeSymTab p -> Environment -> Expr p -> Either (Error p) Type
infer_fst_type fs env e =
    infer_type fs env e >>= \t -> case t of
        (TPair resType _) -> Right resType
        _ -> Left $ typeError e t


infer_snd_type :: FuncTypeSymTab p -> Environment -> Expr p -> Either (Error p) Type
infer_snd_type fs env e =
    infer_type fs env e >>= \t -> case t of
        (TPair _ resType) -> Right resType
        _ -> Left $ typeError e t


infer_match_type :: FuncTypeSymTab p -> Environment -> Expr p -> NilClause p -> ConsClause p -> Either (Error p) Type
infer_match_type fs env e (nilE) (xVar, xsVar, consE) =
    infer_type fs env e >>= \t -> case t of
        TList lT ->
            compareTypes e (TList lT) t >>
            infer_type fs env nilE >>= \nT ->
                 -- TODO: is it correct? return cT or nT ?
                let extEnvTmp = extendEnv env xVar (typeToPrimitive lT) in
                let extEnv = extendEnv extEnvTmp xsVar (typeToPrimitive t) in
                infer_type fs env consE >>= \cT -> return nT
        _ -> Left $ typeError e t


infer_func_app_type :: FuncTypeSymTab p -> Environment -> FSym -> Expr p -> Either (Error p) Type
infer_func_app_type fs env fsym e = case lookupTypeSymTab fs fsym of
    Nothing -> Left $ TypeError (getData e) ("Function does not exist " ++ fsym)
    Just func -> infer_type fs env e >>= func


infer_type :: FuncTypeSymTab p -> Environment -> Expr p -> Either (Error p) Type
infer_type fs env e = case e of
    ENum p n -> Right TInt
    EBool p b -> Right TBool
    EVar p var -> case (lookupEnv env var) of
        Just val -> Right $ primitiveToType val
        Nothing -> Left $ TypeError p ("Unbound variable " ++ var)
    EUnit p -> Right TUnit
    EPair p e1 e2 -> infer_pair_type fs env e1 e2
    ENil p t -> Right $ TList t
    EFst p e -> infer_fst_type fs env e
    ESnd p e -> infer_snd_type fs env e
    ECons p e1 e2 -> infer_cons_type fs env e1 e2
    EMatchL p e nilC consC -> infer_match_type fs env e nilC consC
    EBinary p op e1 e2 -> infer_binary_expr_type fs env op e1 e2
    EUnary p op e -> infer_unary_expr_type fs env op e
    ELet p var varExpr e -> infer_let_expr_type fs env var varExpr e
    EIf p cond tE fE -> infer_cond_expr_type fs env cond tE fE
    EApp p fsym e -> infer_func_app_type fs env fsym e


-- Convert variable list to environment

varToEnvTuple :: Var -> (Var, Primitive)
varToEnvTuple var = (var, VInt 1)

varsToEnv :: [Var] -> Environment
varsToEnv vars = Map.fromList . map varToEnvTuple $ vars


funcToTypeSymTab :: [FunctionDef p] -> FuncTypeSymTab p
funcToTypeSymTab fs =
    let funEnv = Map.fromList (map
            (\f -> (funcName f,
                    -- TODO: it is lazy
                    \argT -> let env = Map.fromList [(funcArg f, (typeToPrimitive argT))] in
                             let e = funcBody f in
                             compareTypes e argT (funcArgType f)-- >>
                             -- infer_type funEnv env e >>=
                                -- compareTypes e (funcResType f)
                    ) --TODO
            ) fs)
    in funEnv


{-
-- Main function type checking expression before interpreting
-- [Var] - list of already defined variables (integers)
-}
typecheck :: [FunctionDef p] -> [Var] -> Expr p -> DataTypes.TypeCheckResult p
typecheck fs vars expr = case infer_type fSymTab env expr of
    Right TInt -> DataTypes.Ok
    Right TBool -> DataTypes.Error p "Expression should evaluate to integer!"
    Left (TypeError p msg) -> DataTypes.Error p msg
    where env = varsToEnv vars
          -- TODO: any(fSymTab) == Left err => Left err
          fSymTab = funcToTypeSymTab fs
          p = getData expr
-- typecheck fs vars expr = DataTypes.Ok


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


interpretCons :: FuncSymTab p -> Environment -> Expr p -> Expr p -> Either (Error p) Primitive
interpretCons fs env e1 e2 =
    interpret fs env e1 >>= \x ->
        interpret fs env e2 >>= \xs -> case xs of
            VList xs -> return $ VList (x:xs)
            VNil _ -> return $ VList [x]
            VInt i -> Left $ ZeroDivisionError (getData e1)

            -- VPair p1 p2 -> Left $ ZeroDivisionError (getData e1)
            -- VUnit -> Left $ ZeroDivisionError (getData e1)
            -- VNil t -> Left $ ZeroDivisionError (getData e1)
            -- VInt i -> Left $ ZeroDivisionError (getData e1)
            -- VBool b -> Left $ ZeroDivisionError (getData e1)
            -- VPair p1 p2 -> Left $ ZeroDivisionError (getData e1)
            -- VList [Primitive] -> Left $ ZeroDivisionError (getData e1) ]


interpretPair :: FuncSymTab p -> Environment -> Expr p -> Expr p -> Either (Error p) Primitive
interpretPair fs env e1 e2 =
    interpret fs env e1 >>= \x1 ->
        interpret fs env e2 >>= \x2 -> return $ VPair x1 x2


interpretMatchExpr :: FuncSymTab p -> Environment -> Expr p -> NilClause p -> ConsClause p -> Either (Error p) Primitive
interpretMatchExpr fs env e (nilE) (xVar, xsVar, consE) =
    interpret fs env e >>= \val -> case val of
        VNil _ -> interpret fs env nilE
        VList (x:xs) ->
            let extEnvTmp = extendEnv env xVar x in
            let extEnv = extendEnv extEnvTmp xsVar (VList xs) in
            interpret fs extEnv consE
        VList ([]) -> interpret fs env nilE


interpretFuncApp :: FuncSymTab p -> Environment -> FSym -> Expr p -> Either (Error p) Primitive
interpretFuncApp fs env fsym e =
    interpret fs env e >>= func
    where Just func = lookupSymTab fs fsym


{-
-- Interpreter function
-- Expression result is based on passed environment
-}
interpret :: FuncSymTab p -> Environment -> Expr p -> Either (Error p) Primitive
interpret fs env expr = case expr of
    ENum p n -> Right $ VInt n
    EBool p b -> Right $ VBool b
    EUnit p -> Right VUnit
    ENil p t -> Right $ VNil t
    ECons p e1 e2 -> interpretCons fs env e1 e2
    EPair p e1 e2 -> interpretPair fs env e1 e2
    EFst p e -> interpret fs env e >>= \(VPair x1 _) -> return x1
    ESnd p e -> interpret fs env e >>= \(VPair _ x2) -> return x2
    EVar p var -> Right value where Just value = lookupEnv env var
    EBinary p op expr1 expr2 -> interpretBinExpr fs env op expr1 expr2
    EUnary p op expr -> interpretUnaryOp fs env op expr
    ELet p var varExpr expr -> interpretLetExpr fs env var varExpr expr
    EIf p cond tE fE -> interpretCondExpr fs env cond tE fE
    EMatchL p e nilC consC -> interpretMatchExpr fs env e nilC consC
    EApp p fsym e -> interpretFuncApp fs env fsym e


-- Transform input to internal environment

inputTupleToEnvTuple :: (Var, Integer) -> (Var, Primitive)
inputTupleToEnvTuple (var, n) = (var, VInt n)

inputToEnvironment :: [(Var, Integer)] -> Environment
inputToEnvironment input = Map.fromList . map inputTupleToEnvTuple $ input


-- Transform function definitions to symbol table
funcDefsToSymTab :: [FunctionDef p] -> FuncSymTab p
funcDefsToSymTab fs =
    let funEnv = Map.fromList $ map
            (\f -> (funcName f,
                    \arg -> let env = Map.fromList [(funcArg f, arg)] in
                            let e = funcBody f in
                            interpret funEnv env e)
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
    where env = inputToEnvironment input
          fSymTab = funcDefsToSymTab fs
          result = interpret fSymTab env expr
