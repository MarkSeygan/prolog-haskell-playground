--Napište funkci, která převede slovo "abbbccac" na seznam, kde jsou po sobě jdoucí stejné znaky sjednoceny do dvojice (znak, početVýskytů).


zcel t = foldl (zcel_text) [] t

zcel_text m x   | m == []               = m ++ [(x,1)]
                | (fst (last m)) == x   = (init m) ++ [(x, (snd (last m))+1)]
                | otherwise             = m ++ [(x,1)]

zpet s = concat (foldl (zpet_text) [] s)
zpet_text m x = m ++ (replicate (snd x) (fst x) )


zcel_either t = map (zcel_e) (zcel t)                

zcel_e p    | (snd p) == 1  = Left (fst p)
            | otherwise     = Right p
