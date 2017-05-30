data Cyclist a = Elem (Cyclist a) a (Cyclist a)


fromList :: [a] -> Cyclist a
fromList (x:xs) = c
    where
        c = Elem prev x next
        (next, prev) = fromList' c xs c


fromList' :: Cyclist a -> [a] -> Cyclist a -> (Cyclist a, Cyclist a)
fromList' prev [] next = (next, prev)
fromList' prev (x:xs) next = (current, last)
    where
        current = Elem prev x next'
        (next', last) = fromList' current xs next



forward :: Cyclist a -> Cyclist a
forward (Elem _ _ c2) = c2


backward :: Cyclist a -> Cyclist a
backward (Elem c1 _ _) = c1


label :: Cyclist a -> a
label (Elem c1 a c2) = a



enumInts :: Cyclist Integer
enumInts = Elem (neg enumInts ) 0 (pos enumInts)
    where neg current@(Elem prev x _) = Elem (neg prev) (x-1) current
          pos current@(Elem _ x next) = Elem current (x+1) (pos next)


