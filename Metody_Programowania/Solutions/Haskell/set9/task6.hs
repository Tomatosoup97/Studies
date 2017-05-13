import Data.List
import Data.Char


-- unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
-- unfoldr f b =
--     case f b of
--         Nothing -> []
--         Just (a, b) -> a : unfoldr f b


integerToString :: Integer -> String

integerToString n =
    (reverse . unfoldr (
        \n -> if n > 0 then
                Just ((intToDigit . fromEnum) (n `mod` 10), n `div` 10)
              else Nothing
    )) n

