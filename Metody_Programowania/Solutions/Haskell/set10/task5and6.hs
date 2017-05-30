{-# LANGUAGE FlexibleInstances #-}

-- Task 5

class Monoid' a where
    (***) :: a -> a -> a
    e :: a

infixl 7 ***
infixr 6 ^^^

(^^^) :: Monoid' a => a -> Integer -> a
a ^^^ 0 = e
a ^^^ n
    | n < 0 = error "only positive nums allowed"
    | n `mod` 2 == 1 = a *** (a ^^^ k) *** (a ^^^ k)
    | otherwise = (a ^^^ k) *** (a ^^^ k)
    where k = n `div` 2


instance Monoid' Integer where
    e = 1
    a *** b = a * b

-- instance Monoid' Integere where
--    e = 0
--    a *** b = a + b

-- instance Monoid' Integer where
--     e = 1
--     a *** b = a * b


-- Task 6
-- ===============================================

data Mtx2x2 a = Mtx2x2 a a a a


instance Monoid' (Mtx2x2 Integer) where
    e = Mtx2x2 1 0 0 1
    (Mtx2x2 a11 a12 a21 a22) *** (Mtx2x2 b11 b12 b21 b22) =
        Mtx2x2 (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
               (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)


getSecondParam :: Mtx2x2 a -> a
getSecondParam (Mtx2x2 a b c d) = b


fib :: [Integer]
fib = [getSecondParam ((Mtx2x2 0 1 1 1) ^^^ n) | n <- [1..]]
