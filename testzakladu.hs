----uloha3-------------------

----bez-rekurze--------------
prevod1:: Int -> [Int] -> Int
prevod1 n xs = foldl (\x y -> n*x + y) 0 xs

----rekurzivne---------------

prevod1R n xs = prevodR n (reverse xs)

prevodR n [] = 0
prevodR n xs = head xs + n*(prevodR n (tail xs))

----pomoci-mapu--------------

prevodM n xs = sum (map (\x -> n^(snd x) * fst x) (zip xs (reverse [0..(length xs -1)])))

---------------3-d-----------
prevod2 :: Int -> Int -> [Int]
prevod2 n s = reverse (unfold (==0) (\x -> (mod x s, div x s)) n)

unfold :: (t -> Bool) -> (t -> (a, t)) -> t -> [a]
unfold done step x = if done x then []
                        else let (y,ys) = step x
                            in y: unfold done step ys

---------------4-------------                            

data Strom a = Nil | Uzel a [Strom a] deriving (Show)

perm :: [Strom a] -> [[Strom a]]
perm [] = [[]]
perm xs =   concat 
            [map (\p -> [xs!!i]++p) 
                 (perm ((take i xs)++(drop (i+1) xs))) | i <- [0..(length xs)-1]]

isom :: (Eq a) => Strom a -> Strom a -> Bool
isom Nil Nil = True
isom (Uzel x xs) (Uzel y ys) = x==y && (length xs == length ys) && (any (\a -> isomseznamy a xs) (perm ys))


isomseznamy xs ys = all (\x -> isom (fst x) (snd x)) (zip xs ys)









