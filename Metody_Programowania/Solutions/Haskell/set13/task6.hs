import Control.Applicative
import Control.Monad (liftM, ap)


data Term sig var = Var var
                  | FunSym sig [Term sig var]


instance Functor (Term sig) where
    fmap = liftM


instance Applicative (Term sig) where
    pure = return
    (<*>) = ap


instance Monad (Term sig) where
    return x = Var x
    grammar >>= f = case grammar of
        Var var -> f var
        FunSym sig gs -> FunSym sig (map invokeGrammar gs) where
            invokeGrammar = \m -> m >>= f
