<3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3

{-------------------------------------------------------------------------------	
	Rozd�lte acyklik� orientovan� graf do vrstev tak, �e z vrchol� ve vrstv� i 
	vedou hrany pouze do vrstev ni���ch. Ka�d� vrchol je v minim�ln� vrstv�, ve 
	kter� m��e b�t.	
-------------------------------------------------------------------------------}

-- pocet prvokv v zozname


-- zo zoznamu vrcholov a hran vyda zoznam vrcholov s poctom predchodcov

									
-- vyberie vrcholy s 0 predchodcami


-- zmaze vrcholy s 0 predchodcami

-- znizi pocet predchodcov u vybranych (ktore) vrcholov


--	vyda zoznam naslednikov vrcholov

	
-- main

{-------------------------------------------------------------------------------	
	M�te d�n n-�rn� strom. Napi�te predik�t co vr�t� seznam cest ze v�ech 
	list� ke ko�eni.
-------------------------------------------------------------------------------}



{-------------------------------------------------------------------------------	
	M�te d�n seznam xs a ��slo n. Napi�te funkci co v�m vr�t� takovou 
	podmno�inu xs, �e jej� sou�et je <= n a to tak, �e co "nejbl�e".
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
	Na vstupu je p�irozen� ��slo n, Vygenerujte nekone�nou posloupnost (seznam) 
	seznam� d�lky n uspo��danou maximolexikograficky, tj. seznamy jsou 
	uspo��d�ny nejprve dle maxima a potom lexikograficky.
	P�.: n = 2 [[0,0],[0,1],[1,0],[0,2],[1,2],[2,0],[2,1]...
-------------------------------------------------------------------------------}



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



{-------------------------------------------------------------------------------	
	Je d�n seznam S - seznam dvojic prvk�, ur�uj�c� ��ste�n� uspo��d�n�. 
	Vyjmenujte seznam v�ech permutac� s, kter� spl�uj� podm�nku, �e pro ka�dou 
	dvojici (a,b) je a v permutaci p�ed b. 
-------------------------------------------------------------------------------}



-- uspor [1,2,3,4,5] [(1,2),(2,3),(3,4),(5,4)]  

{-------------------------------------------------------------------------------	
	Je d�n seznam vektor�. Vyberte z n�ho ty prvky, kter� nejsou dominov�ny 
	jin�m vektorem. (u je dominov�n v, pokud v�echny slo�ky v jsou v�t�� (>=) 
	ne� p��sl. slo�ky u)
-------------------------------------------------------------------------------}


{-------------------------------------------------------------------------------	
	Jsou d�ny ceny a objem p�edm�t� a objem batohu. Najd�te nejcenn�j�� 
	napln�n� batohu. Vydejte jeho cenu a objem.
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
	Mat� orientovan� graf. M�te mno�inu vrchol�. Slu�te tuto mno�inu vrchol� do 
	jednoho. (Tzn m�sto t�to mno�iny bude ve v�sledn�m grafu jen jeden nov� 
	vrchol, m�sto v�ech hran vedouc�ch z n�jak�ho vrcholu do/z libovoln�ho 
	vrcholu t�to mno�iny jen jedna vedouc� do/z toho nov�ho vrcholu)
-------------------------------------------------------------------------------}

-- not member vraci, zda prvek neni v seznamu


{-------------------------------------------------------------------------------	
	Je d�n orientovan� graf a rozklad jeho vrchol� do t��d ekvivalence. 
	Sestrojte nov� graf, ve kter�m spoj�te v�echny vrcholy z jedn� t��dy 
	do jednoho vrcholu-reprezentanta. 
-------------------------------------------------------------------------------}

-- ze seznamu prvku vylouci opakujici se prvky



