import Data.List

--prvek polynomu ma tvar (Exponent, Koeficient)
data Poly = Poly [(Int,Float)]

mult1 a p = map (\x -> (fst a + fst x, snd a * snd x)) p

mult p q = zcel (concat (map (\x -> mult1 x q) p))

zcel p = foldl (zcel0) [] (sort p)

zcel0 m x   | m == [] = m ++ [x]
            | (fst (last m) == fst x) = init m ++ [(fst x, snd (last m) + snd x)]
            | otherwise = m ++ [x]
