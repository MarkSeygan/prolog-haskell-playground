{-------------------------------------------------------------------------------	
	Rozd�lte acyklik� orientovan� graf do vrstev tak, �e z vrchol� ve vrstv� i 
	vedou hrany pouze do vrstev ni���ch. Ka�d� vrchol je v minim�ln� vrstv�, ve 
	kter� m��e b�t.	
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
	M�te d�n n-�rn� strom. Napi�te predik�t co vr�t� seznam cest ze v�ech 
	list� ke ko�eni.
-------------------------------------------------------------------------------}

data Tree a = Leaf a | Branch a [Tree a]

sezcest :: Tree a -> [[a]]
sezcest (Leaf x) =  [[x]]
sezcest (Branch x xs)  = map (++[x]) (fold1 [sezcest y | y <- xs])

fold1 :: [[a]] -> [a]
fold1 [] = []
fold1 (x:xs) = x++(fold1 xs)

{-------------------------------------------------------------------------------	
	M�te d�n seznam xs a ��slo n. Napi�te funkci co v�m vr�t� takovou 
	podmno�inu xs, �e jej� sou�et je <= n a to tak, �e co "nejbl�e".
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
	Na vstupu je p�irozen� ��slo n, Vygenerujte nekone�nou posloupnost (seznam) 
	seznam� d�lky n uspo��danou maximolexikograficky, tj. seznamy jsou 
	uspo��d�ny nejprve dle maxima a potom lexikograficky.
	P�.: n = 2 [[0,0],[0,1],[1,0],[0,2],[1,2],[2,0],[2,1]...
-------------------------------------------------------------------------------}

gen 1 k = [[x]|x<-[0..k]]
gen n k = [[x] ++ y|x<-[0..k],n>0,y<-(gen (n-1) k)]

generuj n = [vysledok|k <- [0..], vysledok <- [temp|temp <- (gen n k)],maximum vysledok == k] 

{-------------------------------------------------------------------------------	
	Napi�te morfologickou funkci
	(Eq a)=>[(String,a)]->[(a,String,b)]->String->[(String,b)]
	Funkce dostane na vstupu slovo (typu String), seznam dvojic kmen (String) 
	a vzor (a) a seznam trojic vzor (a), koncovka (String) a morfologick� 
	informace (b). Vydejte seznam v�ech dvojic (kmen, morfologick� informace), 
	kde kmen odpov�d� kmenu slova se vzorem vzor a morf. info. vzoru slova s 
	t�mto kmenem a p��slu�nou koncovkou.
	
	Zn� to dost stra�liv�, ale znamen� to tohle. Dostanete slovo, k n�mu 
	vyzkou��te v�echna rozd�len� na kmen a koncovku a vyd�te ty dvojice 
	(kmen, morf. info.), kde existuje dvojice (kmen, a) v 1. seznamu a 
	z�rove� (a,koncovka,b) v 2.
-------------------------------------------------------------------------------}

myfind cast1 cast2 list1 list2 = [(kmen, morinfo) | (kmen, vzor) <- list1, (vzor2, koncovka, morinfo) <- list2, vzor == vzor2, kmen == cast1, koncovka == cast2]

vyberkon _ [] = []
vyberkon 1 (x:xs) = xs
vyberkon n (_:xs) = vyberkon (n-1) xs

rozdel slovo list1 list2 = [myfind (take poc slovo) (vyberkon poc slovo) list1 list2 | poc <- [1..(count slovo 0)-1]]

{-------------------------------------------------------------------------------	
	Je d�n seznam S - seznam dvojic prvk�, ur�uj�c� ��ste�n� uspo��d�n�. 
	Vyjmenujte seznam v�ech permutac� s, kter� spl�uj� podm�nku, �e pro ka�dou 
	dvojici (a,b) je a v permutaci p�ed b. 
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
	Je d�n seznam vektor�. Vyberte z n�ho ty prvky, kter� nejsou dominov�ny 
	jin�m vektorem. (u je dominov�n v, pokud v�echny slo�ky v jsou v�t�� (>=) 
	ne� p��sl. slo�ky u)
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
	Jsou d�ny ceny a objem p�edm�t� a objem batohu. Najd�te nejcenn�j�� 
	napln�n� batohu. Vydejte jeho cenu a objem.
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
	Mat� orientovan� graf. M�te mno�inu vrchol�. Slu�te tuto mno�inu vrchol� do 
	jednoho. (Tzn m�sto t�to mno�iny bude ve v�sledn�m grafu jen jeden nov� 
	vrchol, m�sto v�ech hran vedouc�ch z n�jak�ho vrcholu do/z libovoln�ho 
	vrcholu t�to mno�iny jen jedna vedouc� do/z toho nov�ho vrcholu)
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
	Je d�n orientovan� graf a rozklad jeho vrchol� do t��d ekvivalence. 
	Sestrojte nov� graf, ve kter�m spoj�te v�echny vrcholy z jedn� t��dy 
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


