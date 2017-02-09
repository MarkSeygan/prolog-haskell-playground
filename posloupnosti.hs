
--vraci dvojici (cena techto posloupnosti, index prvku s nejvetsi odchylkou)
vyhodnot :: [Int] -> [Int] -> (Int,Int)
vyhodnot x y = (sum ceny,max_odchylka)
                where   ceny        = zipWith (\x y -> abs(x-y)) x y
                        max_odchylka= snd (maximum (zip ceny [0..length(x)]))



muj_zipWith                            
