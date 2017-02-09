import Data.List

--vytrhni jeden prvek
vytrhni1 s = [(take i s)++(drop (i+1) s) | i <- [0..(length s-1)]]

--vytrhni :: (Num a) => a -> [a] -> [a] --obecnejsi varianta signatury
vytrhni :: Int -> [Int] -> [[Int]]
vytrhni 0 s = [s]
vytrhni x s = concat 
                [map ((take i s)++) (vytrhni (x-1) (drop (i+1) s)) |
                        i <- [0..(length s-x)]]

--vytrhni s Omezenim poctu vytrzenych prvku vedle sebe
-- x : kolik prvku celkem vyndavam, s seznam, n omezeni, i pocitadlo
vytrhniO x s n      = vytrhniO' x s n n

vytrhniO' x s n 0   =   map ((take 1 s)++) (vytrhniO' x (drop 1 s) n n)
vytrhniO' x s n i   =   (vytrhniO' (x-1) (drop 1 s) n i-1)
                        ++
                        concat 
                        [map ((take i s)++) (vytrhniO' (x-1) (drop (i+1) s) n n) |
                        i <- [1..(length s-x)]]



--ceny :: [a] -> [a] -> [(a,[b])]
-- x a y jsou posloupnosti, c cena za skrtani prvku
ceny x y c = sort ( concat [  
                    [ ((sum (zipWith (\x y -> abs(x-y)) xs ys) + (2*v*c)),
                        zip xs ys) |
                        xs <- vytrhni v x, ys <- vytrhni v y] | 
                        v <- [0..(length x)]  ]
                        )
