
data Strom a = Nil | Uzel a [Strom a] deriving (Show)


vypis :: Strom String -> String
--vypis (Uzel u []) = "<"++u++"></"++u++">"
vypis (Uzel u us) = "<"++u++">"++(vypis' us)++"</"++u++">"

vypis' :: [Strom String] -> String
vypis' us = foldl (++) [] (map vypis us)

