import Control.Applicative
import Control.Monad (liftM, ap)


newtype StateComput s a = StateComput { runState :: (s -> (a, s)) }


instance Functor (StateComput s) where
    fmap = liftM


instance Applicative (StateComput s) where
    pure = return
    (<*>) = ap


instance Monad (StateComput s) where
    return x = StateComput (\s -> (x, s))
    (StateComput gen) >>= f = StateComput $ \s ->
                         let (x, s') = gen s
                             (StateComput genAux) = f x
                         in genAux s'


init' :: Int -> StateComput Int ()
init' seed = StateComput (\_ -> ((), seed))

-- random :: Random Int
random :: StateComput Int Int
random = StateComput (\seed ->
    let tmpSeed = 16807 * (seed `mod` 127773)
                - 2836  * (seed `div` 127773)
        newSeed
            | tmpSeed > 0 = tmpSeed
            | otherwise = tmpSeed + 2147483647
    in (newSeed, newSeed))


-- Task 3

type SSC a = StateComput String a


runSSC :: SSC a -> String -> a
runSSC ssc = (fst . runState ssc)


getc :: SSC Char
getc = StateComput (\(x:xs) -> (x, xs))


ungetc :: Char -> SSC ()
ungetc x = StateComput (\xs -> ((), (x:xs)))


isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _  = False


isEOS :: SSC Bool
isEOS = StateComput (\xs -> (isEmpty xs, xs))


countLines :: String -> Int
countLines = runSSC $ lines 0 where
    lines :: Int -> SSC Int
    lines n = do
        b <- isEOS
        if b
            then return n
            else do
                ch <- getc
                lines (if ch == '\n' then n+1 else n)

