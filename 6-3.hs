
is_pos_v :: [Int] -> Bool
is_pos_v v = all (>0) v

is_pos_m :: [[Int]] -> Bool
is_pos_m m = all (is_pos_v) m

make_matrix m i j = map (take (j+1)) (take (i+1) m)

kladne_podmatice m = filter (is_pos_m) [(make_matrix m i j) | 
                                                    i <- [0..length(m)], 
                                                    j <- [0..length(m!!0)]]
                                                    
-- je_podmatice m n :: m je podmatice matice n (od prvku 1,1)        
je_podmatice m n | ((map (take (length(m!!0))) (take (length(m)) n))==m)    = True
                 | otherwise                                                = False
