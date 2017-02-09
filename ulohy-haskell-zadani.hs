<3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3

{-------------------------------------------------------------------------------	
	Rozdìlte acykliký orientovaný graf do vrstev tak, že z vrcholù ve vrstvì i 
	vedou hrany pouze do vrstev nižších. Každý vrchol je v minimální vrstvì, ve 
	které mùže být.	
-------------------------------------------------------------------------------}

-- pocet prvokv v zozname


-- zo zoznamu vrcholov a hran vyda zoznam vrcholov s poctom predchodcov

									
-- vyberie vrcholy s 0 predchodcami


-- zmaze vrcholy s 0 predchodcami

-- znizi pocet predchodcov u vybranych (ktore) vrcholov


--	vyda zoznam naslednikov vrcholov

	
-- main

{-------------------------------------------------------------------------------	
	Máte dán n-ární strom. Napište predikát co vrátí seznam cest ze všech 
	listù ke koøeni.
-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------	
	Máte dán seznam xs a èíslo n. Napište funkci co vám vrátí takovou 
	podmnožinu xs, že její souèet je <= n a to tak, že co "nejblíže".
-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------	
	Pro binarni (ne nutne vyhledavaci) strom kterej ma data jen v listech 
	vytvorte predikat, tkerej ten strom prevede na strom s daty i v uzlech, 
	kde hodnota v uzlu je minimum hodnoty obou podstromu.
-------------------------------------------------------------------------------}



--preved :: (Tree1 a) -> (Tree2 a)


--mymin :: (Tree1 a)->(Tree1 a)-> a


-- prveved (Branch1 (Branch1 (Leaf1 3) (Branch1 (Leaf1 4) (Leaf1 5))) (Branch1 (Branch1 (Leaf1 9) (Leaf1 8)) (Leaf1 7)))

{-------------------------------------------------------------------------------	
	Na vstupu je pøirozené èíslo n, Vygenerujte nekoneènou posloupnost (seznam) 
	seznamù délky n uspoøádanou maximolexikograficky, tj. seznamy jsou 
	uspoøádány nejprve dle maxima a potom lexikograficky.
	Pø.: n = 2 [[0,0],[0,1],[1,0],[0,2],[1,2],[2,0],[2,1]...
-------------------------------------------------------------------------------}



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



{-------------------------------------------------------------------------------	
	Je dán seznam S - seznam dvojic prvkù, urèující èásteèné uspoøádání. 
	Vyjmenujte seznam všech permutací s, které splòují podmínku, že pro každou 
	dvojici (a,b) je a v permutaci pøed b. 
-------------------------------------------------------------------------------}



-- uspor [1,2,3,4,5] [(1,2),(2,3),(3,4),(5,4)]  

{-------------------------------------------------------------------------------	
	Je dán seznam vektorù. Vyberte z nìho ty prvky, které nejsou dominovány 
	jiným vektorem. (u je dominován v, pokud všechny složky v jsou vìtší (>=) 
	než pøísl. složky u)
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------	
	Jsou dány ceny a objem pøedmìtù a objem batohu. Najdìte nejcennìjší 
	naplnìní batohu. Vydejte jeho cenu a objem.
-------------------------------------------------------------------------------}

--soucet1 :: [(Int, Int)] -> Int -> [Int]


{-------------------------------------------------------------------------------	
	Je dan seznam cisel xs. Najdete vsechny seznamy, ktere vzniknou z daneho 
	seznamu xs tak, ze vybereme nejaky spojity usek seznamu a vlozime ho na 
	stejne misto v obracenem poradi. Pritom cislum ve vybranem seznamu zmenime 
	znamenko.
-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------	
	Najdi k nejmensich cisel v seznamu prirozenych cisel, ktere tam nejsou. 
	(seznam neni setrizen. Lze predpokladat, ze se tam prvky neopakuji)
-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------	
	Maté orientovaný graf. Máte množinu vrcholù. Sluète tuto množinu vrcholù do 
	jednoho. (Tzn místo této množiny bude ve výsledném grafu jen jeden nový 
	vrchol, místo všech hran vedoucích z nìjakého vrcholu do/z libovolného 
	vrcholu této množiny jen jedna vedoucí do/z toho nového vrcholu)
-------------------------------------------------------------------------------}

-- not member vraci, zda prvek neni v seznamu


{-------------------------------------------------------------------------------	
	Je dán orientovaný graf a rozklad jeho vrcholù do tøíd ekvivalence. 
	Sestrojte nový graf, ve kterém spojíte všechny vrcholy z jedné tøídy 
	do jednoho vrcholu-reprezentanta. 
-------------------------------------------------------------------------------}

-- ze seznamu prvku vylouci opakujici se prvky



