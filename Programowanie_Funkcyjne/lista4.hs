-- Mateusz Urbańczyk - Lista 4

-- Zad. 3

data BT a = Empty | Node a (BT a) (BT a)
        deriving (Eq, Ord, Show, Read)

sumBT :: BT Integer -> Integer
sumBT Empty = 0
sumBT (Node x n1 n2) = sum [x, sumBT n1, sumBT n2]

test_sumBT_1 = sumBT (Node 10 (Node 1 Empty Empty) (Node 5 Empty Empty))
test_sumBT_2 = sumBT Empty
test_sumBT_3 = sumBT (Node 10 Empty Empty)


-- Zad. 4

foldBT :: (a -> (b, b) -> b) -> b -> BT a -> b
foldBT f c Empty = c
foldBT f c (Node x n1 n2) = f x (foldBT f c n1, foldBT f c n2)

addBT = \x -> \(b, c) -> x + b + c


-- Zad. 5

test_foldBT_1 = foldBT addBT 0 (Node 10 (Node 1 Empty Empty) (Node 5 Empty Empty))
test_foldBT_2 = foldBT addBT 0 (Node 10 Empty Empty)

consBT = \x -> \(b, c) -> (x:b) ++ c

test_consBT_1 = foldBT consBT [] (Node 10 (Node 1 Empty Empty) (Node 5 Empty Empty))
test_consBT_2 = foldBT consBT [] (Node 10 Empty Empty)


-- Zad. 6

mapBT :: (a -> b) -> BT a -> BT b
mapBT f bt = foldBT (\x -> \(b, c) -> Node (f x) b c) Empty bt

test_mapBT_1 = mapBT (+1) (Node 10 (Node 1 Empty Empty) (Node 5 Empty Empty))
test_mapBT_2 = mapBT (*2) (Node 10 (Node 1 Empty Empty) (Node 5 Empty Empty))
test_mapBT_3 = mapBT (*2) Empty

