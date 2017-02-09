
data BTree a = Nil | Uzel (BTree a) a (BTree a) deriving (Show)

treeFold :: (a -> b -> a -> a) -> a -> BTree b -> a
treeFold _      fNil Nil            = fNil
treeFold fUzel  fNil (Uzel l u p)   = fUzel (treeFold fUzel fNil l) u (treeFold fUzel fNil p)
        
listyF :: BTree a -> Int
listyF s = treeFold (listyF') 0 s

listyF' 0 _ 0 = 1
listyF' x u y = x+y


--bez-foldu
listy (Uzel Nil _ Nil)  = 1
listy (Uzel x u y)      = listy x + listy y

