data Strom a = Nil | Uzel a [Strom a] deriving (Show)


zipTree :: (a->a->b) -> Strom a -> Strom a -> Strom b

zipTree f Nil _ = Nil
zipTree f _ Nil = Nil
zipTree f (Uzel a as) (Uzel b bs) = Uzel (f a b) (zipWith (zipTree f) as bs)

zipTreeZ :: (a->a->b) -> a -> Strom a -> Strom a -> Strom b
zipTreeZ f d (Uzel a as) (Uzel b bs) = Uzel (f a b) (zipWithZ (zipTreeZ f d) d as bs)


--zipWith na seznamech s defaultni hodnotou
zipWithZ :: (a->a->b) -> a -> [a] -> [a] -> [b]
zipWithZ f d x y | length x > length y  = zipWith f x (dopln y (length x) d)
                 | otherwise            = zipWith f (dopln x (length y) d) y 

--doplni seznam o defaultni hodnoty d na delku 'delka'

dopln s delka d = s ++ (replicate (delka - length s) d)
