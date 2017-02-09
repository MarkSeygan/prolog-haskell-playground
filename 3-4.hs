
--The Ord class is used for totally ordered datatypes.  
zarad :: (Num a, Ord a) => ([a], [a]) -> [[a]] -> ([[a]], [[a]])
zarad (x,y) s = (   [x]++[m | m <- s, (manhatt x m) <= (manhatt y m)],
                    [y]++[m | m <- s, (manhatt x m) >  (manhatt y m)])

--manhatt spocita vzdalenost manhattanskou metrikou mezi dvema n-ticema
manhatt m n = sum (map (abs) (zipWith (-) m n))
