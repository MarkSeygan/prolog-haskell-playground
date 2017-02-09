% <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 <3 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Zretazi 2 zoznamy	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zretez(S1,S2,S) :- append(S1,S2,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prejde binarny strom a vrati infixovy zapis
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

infix(nil,[]).
infix(t(nil,X,nil),[X]).
infix(t(X,Y,Z),S) :- infix(X,SX), append(SX,[Y],SY),infix(Z,SZ), append(SY,SZ,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Máte zrekonstruovat binární strom z jeho prefixového zápisu. 
%	Na vstupu je v argumentu seznam dvojic - hodnota vrcholu a poèet synù. 
%	Listy mají poèet synù 0 a vnitøní vrcholy 2.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

na_strom(Seznam,Strom) :- na_strom(Seznam,Strom,[]).
na_strom([(X,0)|T], t(nil,X,nil), T).
na_strom([(X,2)|T], t(A,X,B), Zbytek2) :- 
                                na_strom(T, A, Zbytek),
                                na_strom(Zbytek,B,Zbytek2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najde pocet predchodcov vrcholu X
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najde zoznam naslednikov vrcholu X
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Rozdìlte acyklický orientovaný graf do vrstev tak, ze z vrcholù ve vrstve i 
%	vedou hrany pouze do vrstev nizsich. Kazdý vrchol je v minimální vrstve, 
%	ve které mùze být.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	Najde zoznam naslednikov vrcholu X a pocet predchodcov
%


	
%
%	insvp - zaradi vrchol do zoznamu podla poctu predchodcov
%



%
%	sortvp - utriedi zoznam vrcholov podla predchodcov	
%



%
%	Znizi stupen predchodcov
%



%
%	Odoberie vrchly z 0lovymi predchodcami
%


	
%
%	hlavna cast - odoberie nulove vrcholy, znizi stupen ostatnych, atd.
%
	

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Permutacia zoznamu
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pvyber([X|Xs], X, Xs).
pvyber([X|Xs], Y, [X|Out]) :- pvyber(Xs, Y, Out).

perm([],[]).
perm(Xs,[A|Sb]) :- pvyber(Xs,A,B), perm(B,Sb).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Projdete orientovaný graf do hloubky a ke kazdému vrcholu pridejte dve 
%	císla: cas prvního navstívení vrcholu a cas posledního opustení vrcholu. 
%	Cas se zvysuje o 1 pri kazdé události, tj. pruchodem vrcholu. 	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	Vyhlada v zozname vrchol X a vyberie ho, vrati zoznam naslednikov
%

vyber_v([X-XS|S],X,XS,S).
vyber_v([X-XS|S],Y,YS,[X-XS|Zb] ) :- vyber_v(S, Y, YS, Zb).

%	Projde orientovany graf do hlbky

	
%
%	Prejde do hlbky a vrati vstupny a vystupny cas
%
	

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie N-arneho grafu do hlbky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie N-arneho grafu do sirky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie binarneho grafu do hlbky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dán n-ární strom. Pro dané k vrate vechny listy, které jsou 
%	dosaitelné po cestì, na které souèet poøadových èísel vybíraných synù 
%	(zleva od 0) se rovná právì k.	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


		
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dán graf. Zjistìte, zda je bipartitní a vydejte dotvrzující tøídy 
%	rozkladu vrcholù.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%
%	Prida prvok alebo zoznam do zoznamu, bez duplicit
%



%
%	Checkne ci su prvky zoznamu X v Liste, ak aspon jeden - chyba
%



%
%	Vrati mnoziny bipartitneho grafu
%

	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dana hierarchie oken v okennim systemu ve forme n-arniho stromu. 
%	Kazde okno je dano hornim levym a dolnim pravym rohem. 
%	Upravte strom tak, ze zachovate strukturu a listy a do vsech vnitrich 
%	vrcholu stromu date nejmensi okno, ktere obsahuje vsechny podokna.	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Pøidejte ke kadému vrcholu v n-árním stromì dvì pøirozená èísla. 
%	První je èíslo poøadí vrcholu pøi prùchodu preorder, druhé pøi 
%	prùchodu postorder (zleva).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Máte dán n-ární strom, který má v kadém uzlu OKNO (dané souøadnicí levého 
%	horního a pravého dolního rohu). Napite predikát, co tento strom projde a 
%	oøee kadé okno podle jeho pøedka (tzn. to co pøes pøedka "pøeèuhuje" 
%	uøízne a vrátí nový strom). Pokud je celé mimo pøedka, zahoïte uzel i jeho 
%	potomky.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najdi maximalni nezavislou mnozinu v grafu (nemusi byt nejvetsi mozna)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	failne, ak nie je Y (ktory je spojeny hranou z X) v Liste
%
	




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Kartezky soucin 2 grafu (bez orientace hran) 	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	 Prevod n-arniho stromu na binarni
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Dan graf. naleznete v nem vsechny cesty delky 2 a pridejte k puvodnimu 
%	grafu a vratte jako vysledek.
%	
%	Pozn.: graf je zadany vrchol(x, [zoznam naslednikov]), tj. bude fungovat
%	aj pre orientovane grafy, horsie to bude uz s ohodnotenim hran 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najdete sedlovy bod matice
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Otocenie matice doprava
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Maté orientovaný graf. Máte mnoinu vrcholù. Sluète tuto mnoinu vrcholù 
%	do jednoho. (Tzn místo této mnoiny bude ve výsledném grafu jen jeden nový 
%	vrchol, místo vech hran vedoucích z nìjakého vrcholu do/z libovolného 
%	vrcholu této mnoiny jen jedna vedoucí do/z toho nového vrcholu)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



