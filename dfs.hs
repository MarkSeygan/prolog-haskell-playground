

--k klic, e edges, v vrcholy, w nenavstivene vrcholy

--dfs k e v [] = []
dfs k e v w  = foldl (\x y -> dfs y e v x) (remove k w) [snd f | f <- e, fst f == k]

--remove k w vezme k z w
