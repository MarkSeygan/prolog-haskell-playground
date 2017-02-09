{-------------------------------------------------------------------------------	
	Rozdìlte acykliký orientovaný graf do vrstev tak, že z vrcholù ve vrstvì i 
	vedou hrany pouze do vrstev nižších. Každý vrchol je v minimální vrstvì, ve 
	které mùže být.	
-------------------------------------------------------------------------------}

-- pocet prvokv v zozname
count [] n = n
count (x:xs) n = count xs n+1

-- zo zoznamu vrcholov a hran vyda zoznam vrcholov s poctom predchodcov
poc [] _ = []	
poc (x:xs) list = 
	[(x, count [n | (n,y) <- list, x == y] 0)]
	++ 
	poc xs list
									
-- vyberie vrcholy s 0 predchodcami
vyber0 vrcholy = [x | (x,0) <- vrcholy]

-- zmaze vrcholy s 0 predchodcami
zmaz0 vrcholy = [(x,y) | (x,y) <- vrcholy, y /= 0]

-- znizi pocet predchodcov u vybranych (ktore) vrcholov
zniz [] _ = []							
zniz ((x,y):vrcholy) ktore = 
	[(x,y - count [n | n <- ktore, n == x] 0)]
	++
	zniz vrcholy ktore

--	vyda zoznam naslednikov vrcholov
nasl [] _ = []					
nasl (x:vrcholy) hrany = 
	[b | (a,b) <- hrany, a == x]
	++
	nasl vrcholy hrany
	
-- main
rdov2 [] _ = []					
rdov2 vrcholy hrany = 
	[vyber0 vrcholy]
	++
	rdov2 (zniz (zmaz0 vrcholy) (nasl (vyber0 vrcholy) hrany)) hrany

rdov vrcholy hrany = rdov2 (poc vrcholy hrany) hrany

rdov_ex = rdov [1,2,3,4,5,6,7,8,9,10,11] [(1,2),(1,7),(2,3),(2,5),
	(3,4),(5,6),(6,4),(7,5),(7,8),(8,6),(9,8),(10,11)]

{-------------------------------------------------------------------------------	
	Máte dán n-ární strom. Napište predikát co vrátí seznam cest ze všech 
	listù ke koøeni.
-------------------------------------------------------------------------------}

data Tree a = Leaf a | Branch a [Tree a]

sezcest :: Tree a -> [[a]]
sezcest (Leaf x) =  [[x]]
sezcest (Branch x xs)  = map (++[x]) (fold1 [sezcest y | y <- xs])

fold1 :: [[a]] -> [a]
fold1 [] = []
fold1 (x:xs) = x++(fold1 xs)

{-------------------------------------------------------------------------------	
	Máte dán seznam xs a èíslo n. Napište funkci co vám vrátí takovou 
	podmnožinu xs, že její souèet je <= n a to tak, že co "nejblíže".
-------------------------------------------------------------------------------}

soucet :: [Int] -> Int -> [Int]
soucet  []  _  =  []
soucet  _   0  =  []
soucet (x:xs) n =
	if ((sum batoh1) <= n) then
		if ((sum batoh2) > n) then (batoh1)
		else if ((sum batoh1) >= (sum batoh2)) then (batoh1)
		else (batoh2)
	else (batoh2)
	where 	
		batoh1 = x:(soucet xs (n-x)) 
		batoh2 = soucet xs n

{-------------------------------------------------------------------------------	
	Pro binarni (ne nutne vyhledavaci) strom kterej ma data jen v listech 
	vytvorte predikat, tkerej ten strom prevede na strom s daty i v uzlech, 
	kde hodnota v uzlu je minimum hodnoty obou podstromu.
-------------------------------------------------------------------------------}

data Tree1 a = Leaf1 a | Branch1 (Tree1 a) (Tree1 a) 
data Tree2 a = Leaf2 a | Branch2 (Tree2 a) a (Tree2 a) deriving (Eq, Show)

--preved :: (Tree1 a) -> (Tree2 a)
preved (Leaf1 a) = (Leaf2 a)
preved (Branch1 t1 t2) = (Branch2 (preved t1) (mymin t1 t2) (preved t2))

--mymin :: (Tree1 a)->(Tree1 a)-> a
mymin t1 t2 = min (mymin2 t1) (mymin2 t2)

mymin2 (Leaf1 a) = a
mymin2 (Branch1 t1 t2) = mymin t1 t2

-- prveved (Branch1 (Branch1 (Leaf1 3) (Branch1 (Leaf1 4) (Leaf1 5))) (Branch1 (Branch1 (Leaf1 9) (Leaf1 8)) (Leaf1 7)))

{-------------------------------------------------------------------------------	
	Na vstupu je pøirozené èíslo n, Vygenerujte nekoneènou posloupnost (seznam) 
	seznamù délky n uspoøádanou maximolexikograficky, tj. seznamy jsou 
	uspoøádány nejprve dle maxima a potom lexikograficky.
	Pø.: n = 2 [[0,0],[0,1],[1,0],[0,2],[1,2],[2,0],[2,1]...
-------------------------------------------------------------------------------}

gen 1 k = [[x]|x<-[0..k]]
gen n k = [[x] ++ y|x<-[0..k],n>0,y<-(gen (n-1) k)]

generuj n = [vysledok|k <- [0..], vysledok <- [temp|temp <- (gen n k)],maximum vysledok == k] 

{-------------------------------------------------------------------------------	
	Napište morfologickou funkci
	(Eq a)=>[(String,a)]->[(a,String,b)]->String->[(String,b)]
	Funkce dostane na vstupu slovo (typu String), seznam dvojic kmen (String) 
	a vzor (a) a seznam trojic vzor (a), koncovka (String) a morfologická 
	informace (b). Vydejte seznam všech dvojic (kmen, morfologická informace), 
	kde kmen odpovídá kmenu slova se vzorem vzor a morf. info. vzoru slova s 
	tímto kmenem a pøíslušnou koncovkou.
	
	Zní to dost strašlivì, ale znamená to tohle. Dostanete slovo, k nìmu 
	vyzkoušíte všechna rozdìlení na kmen a koncovku a vydáte ty dvojice 
	(kmen, morf. info.), kde existuje dvojice (kmen, a) v 1. seznamu a 
	zároveò (a,koncovka,b) v 2.
-------------------------------------------------------------------------------}

myfind cast1 cast2 list1 list2 = [(kmen, morinfo) | (kmen, vzor) <- list1, (vzor2, koncovka, morinfo) <- list2, vzor == vzor2, kmen == cast1, koncovka == cast2]

vyberkon _ [] = []
vyberkon 1 (x:xs) = xs
vyberkon n (_:xs) = vyberkon (n-1) xs

rozdel slovo list1 list2 = [myfind (take poc slovo) (vyberkon poc slovo) list1 list2 | poc <- [1..(count slovo 0)-1]]

{-------------------------------------------------------------------------------	
	Je dán seznam S - seznam dvojic prvkù, urèující èásteèné uspoøádání. 
	Vyjmenujte seznam všech permutací s, které splòují podmínku, že pro každou 
	dvojici (a,b) je a v permutaci pøed b. 
-------------------------------------------------------------------------------}

permutations [] = [[]]
permutations xs = [x:p | x <- xs, p <- permutations (filter (/= x) xs)]

check [] _ = True
check (x:xs) (a,b) 
	| x == a = True
	| x == b = False
	| otherwise = check xs (a,b)

check2 _ [] = True
check2 perm (x:xs) =
	if ((check perm x) == True) then
		(check2 perm xs)
	else False		
	
check1 list1 list2 = [a | a <- list1, check2 a list2]

uspor list1 list2 = check1 (permutations list1) list2

-- uspor [1,2,3,4,5] [(1,2),(2,3),(3,4),(5,4)]  

{-------------------------------------------------------------------------------	
	Je dán seznam vektorù. Vyberte z nìho ty prvky, které nejsou dominovány 
	jiným vektorem. (u je dominován v, pokud všechny složky v jsou vìtší (>=) 
	než pøísl. složky u)
-------------------------------------------------------------------------------}

domino [] [] = True
domino (x:xs) (y:ys)
	| y >= x = domino xs ys
	| y < x = False
	
domino2 _ [] = True	
domino2 a (x:xs)
	| (domino a x) == True = False
	| otherwise = (domino2 a xs)
	
domino1 list = [a | a <- list, (domino2 a (filter (/= a) list)) == True]

{-------------------------------------------------------------------------------	
	Jsou dány ceny a objem pøedmìtù a objem batohu. Najdìte nejcennìjší 
	naplnìní batohu. Vydejte jeho cenu a objem.
-------------------------------------------------------------------------------}

--soucet1 :: [(Int, Int)] -> Int -> [Int]
soucet1  []  _  =  []
soucet1  _   0  =  []
soucet1 ((c,o):xs) ob =
	if (objembat1 <= ob) then
		if (objembat2 > ob) then (batoh1)
		else if ((sumbat1) >= (sumbat2)) then (batoh1)
		else (batoh2)
	else (batoh2)
	where 	
		objembat1 = sum [o | (_,o) <- batoh1]
		objembat2 = sum [o | (_,o) <- batoh2]
		sumbat1 = sum [c | (c,_) <- batoh1]
		sumbat2 = sum [c | (c,_) <- batoh2]
		batoh1 = [(c,o)]++(soucet1 xs (ob-o)) 
		batoh2 = soucet1 xs ob

{-------------------------------------------------------------------------------	
	Je dan seznam cisel xs. Najdete vsechny seznamy, ktere vzniknou z daneho 
	seznamu xs tak, ze vybereme nejaky spojity usek seznamu a vlozime ho na 
	stejne misto v obracenem poradi. Pritom cislum ve vybranem seznamu zmenime 
	znamenko.
-------------------------------------------------------------------------------}

mytake _ 0 x = x
mytake 0 k x = take k x
mytake n k (x:xs) = mytake (n-1) k xs

znamienko [] = []
znamienko (x:xs) = [(0-x)]++(znamienko xs)

usek x = [(take m x)++(reverse (znamienko (mytake m k x)))++(mytake (m+k) n x) | k <- [1..n], m <- [0..n-k]]
	where n = count x 0

{-------------------------------------------------------------------------------	
	Najdi k nejmensich cisel v seznamu prirozenych cisel, ktere tam nejsou. 
	(seznam neni setrizen. Lze predpokladat, ze se tam prvky neopakuji)
-------------------------------------------------------------------------------}

qsort [] = []
qsort (x:xs) = 
	(qsort [a | a <- xs, a < x])
	++[x]++
	(qsort [b | b <- xs, b >= x])

najdikmin2 [] _ _ = []
najdikmin2 _ 0 _ = []
najdikmin2 (x:xs) k n
	| n < x = [n]++(najdikmin2 (x:xs) (k-1) (n+1))
	| otherwise	= (najdikmin2 xs k (n+1))
	
najdikmin list k = najdikmin2 (qsort list) k 0

{-------------------------------------------------------------------------------	
	Maté orientovaný graf. Máte množinu vrcholù. Sluète tuto množinu vrcholù do 
	jednoho. (Tzn místo této množiny bude ve výsledném grafu jen jeden nový 
	vrchol, místo všech hran vedoucích z nìjakého vrcholu do/z libovolného 
	vrcholu této množiny jen jedna vedoucí do/z toho nového vrcholu)
-------------------------------------------------------------------------------}

-- not member vraci, zda prvek neni v seznamu
notMember1 :: (Eq a) => a -> [a] -> Bool
notMember1 _ [] = True
notMember1 y (x:xs)
        | y==x = False
        | otherwise = notMember1 y xs
        
spojvrcholy xs hrany novy = 
	[(a,b) | (a,b) <- hrany, (a `notMember1` xs) && (b `notMember1` xs)]
	++
	[(novy,b) | (a,b) <- hrany, (a `member` xs) && (b `notMember1` xs)]
	++
	[(a,novy) | (a,b) <- hrany, (a `notMember1` xs) && (b `member` xs)]

{-------------------------------------------------------------------------------	
	Je dán orientovaný graf a rozklad jeho vrcholù do tøíd ekvivalence. 
	Sestrojte nový graf, ve kterém spojíte všechny vrcholy z jedné tøídy 
	do jednoho vrcholu-reprezentanta. 
-------------------------------------------------------------------------------}

-- ze seznamu prvku vylouci opakujici se prvky
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (x:xs)
        | x `notMember1` xs = x:(uniq xs)
        | otherwise = uniq xs

vyberrepro [] = 0
vyberrepro (x:xs) = x

spoj2 [] x = x
spoj2 (trieda:list) hrany = spoj2 list (uniq (spojvrcholy (trieda) hrany (vyberrepro trieda)))


