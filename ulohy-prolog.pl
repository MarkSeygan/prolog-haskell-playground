%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Zretazi 2 zoznamy	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

app([], Y, Y).

app([X|Xs], Y, [X|Out]):-
	app(Xs, Y, Out).

app(X, Y, [X|Y]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prejde binarny strom a vrati infixovy zapis
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

btin(tb(n,Value,n), Value).

btin(tb(n,Value,Right), [Value|Rightout]) :- 
	btin(Right, Rightout).
	
btin(tb(Left,Value,n), [Leftout|Value]) :- 
	btin(Left, Leftout).
	
btin(tb(Left,Value,Right), Out) :- 
	btin(Left, Leftout),
	btin(Right, Rightout),
	app(Leftout, [Value|Rightout], Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Máte zrekonstruovat binární strom z jeho prefixového zápisu. 
%	Na vstupu je v argumentu seznam dvojic - hodnota vrcholu a poèet synù. 
%	Listy mají poèet synù 0 a vnitøní vrcholy 2.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

urobbin([], n, []).

urobbin((X,Y), tb(n,X,n), []) :- 
	Y == 0.

urobbin([(X,Y)|Zoz], tb(Left, X, Right), Zvysok2) :-
	Y == 2,
	urobbin(Zoz, Left, Zvysok),
	urobbin(Zvysok, Right, Zvysok2).
	
urobbin([(X,Y)|Zoz], tb(n,X,n), Zoz) :- 
	Y == 0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najde pocet predchodcov vrcholu X
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pocpred(_, [], 0).

pocpred(X, [h(_,X)|Ys], N) :- 
	pocpred(X, Ys, N1),
	N is N1 + 1.
	
pocpred(X, [h(_,_)|Ys], N) :-
	pocpred(X, Ys, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najde zoznam naslednikov vrcholu X
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nasl(_, [], []).

nasl(X, [h(X,Y)|Ys], [Y|Out]) :-
	nasl(X, Ys, Out).

nasl(X, [h(_,_)|Ys], Out) :-
	nasl(X, Ys, Out).
	
najdinasl([], _, []).
	
najdinasl([X|Xs], Zoz, [v(X,Xzoz)|Out]) :-
	nasl(X, Zoz, Xzoz),
	najdinasl(Xs, Zoz, Out).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Rozdìlte acykliký orientovaný graf do vrstev tak, že z vrcholù ve vrstvì i 
%	vedou hrany pouze do vrstev nižších. Každý vrchol je v minimální vrstvì, 
%	ve které mùže být.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	Najde zoznam naslednikov vrcholu X a pocet predchodcov
%

poc_nasl([], _, []).

poc_nasl([X|[]], Hrany, v(X,N,Z)) :-
	pocpred(X, Hrany, N),
	nasl(X, Hrany, Z).

poc_nasl([X|Xs], Hrany, [v(X,N,Z)|Vr]):-
	pocpred(X, Hrany, N),
	nasl(X, Hrany, Z),
	poc_nasl(Xs, Hrany, Vr).
	
%
%	insvp - zaradi vrchol do zoznamu podla poctu predchodcov
%

insvp(v(X,Xn), [], [v(X,Xn)]).

insvp(v(X,Xn), [v(Y,Yn)|Ys], [v(X,Xn),v(Y,Yn)|Ys]) :-
	Xn =< Yn.

insvp(v(X,Xn), [v(Y,Yn)|Ys], [v(Y,Yn)|Out]) :-
	insvp(v(X,Xn), Ys, Out).

%
%	sortvp - utriedi zoznam vrcholov podla predchodcov	
%

sortvp([], Zoz, Zoz). 

sortvp([X|Xs], Zoz, Out) :-
	insvp(X, Zoz, Out1),
	sortvp(Xs, Out1, Out).

%
%	Znizi stupen predchodcov
%

znizpoc(_, [], 0).

znizpoc(X, [X|Ys], Poc) :-
	znizpoc(X,Ys,Poc1),
	Poc is Poc1 + 1.

znizpoc(X, [_|Ys], Poc) :-
	znizpoc(X,Ys,Poc).

znizsutpen([], _, []).
		
znizsutpen(v(X, N, Zoz), List, v(X, N1, Zoz)) :-
	znizpoc(X, List, Poc),
	N1 is N - Poc.

znizsutpen([v(X, N, Zoz)|Ys], List, [v(X, N1, Zoz)|Out]) :-
	znizpoc(X, List, Poc),
	N1 is N - Poc,
	znizsutpen(Ys, List, Out).

%
%	Odoberie vrchly s 0lovymi predchodcami
%

odober0([], [], [], []).

odober0(v(X,0,Nasl), [], X, Nasl).

odober0(v(X,Y,Z), v(X,Y,Z), [], []).
	
odober0([v(X,0,Nasl)|Xs], Zos, [X|Out], Out2) :-
	odober0(Xs, Zos, Out, NaslOut),
	append(Nasl, NaslOut, Out2).
	
odober0([v(X,N,Nasl)|Xs], [v(X,N,Nasl)|Zos], Out, NaslOut) :-
	odober0(Xs, Zos, Out, NaslOut).
	
%
%	hlavna cast - odoberie nulove vrcholy, znizi stupen ostatnych, atd.
%
	
rdov([], []).
	
rdov(List, [Prve|Out]) :-
	odober0(List, Zos, Prve, Nasl),
	znizsutpen(Zos, Nasl, Zos2),
	rdov(Zos2, Out).
	
rdov_go(V,H,Out):-
	poc_nasl(V,H,Out1),
	rdov(Out1, Out).	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Permutacia zoznamu
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pvyber([X|Xs], X, Xs).

pvyber([X|Xs], Y, [X|Out]) :-
	pvyber(Xs, Y, Out).
	
permutuj([],[]).

permutuj(X, [Y|Out]) :-
	pvyber(X, Y, Out1),
	permutuj(Out1, Out).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Projdìte orientovaný graf do hloubky a ke každému vrcholu pøidejte dvì 
%	èísla: èas prvního navštívení vrcholu a èas polsedního opuštìní vrcholu. 
%	Èas se zvyšuje o 1 pøi každé události, tj. prùchodem vrcholu. 	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	Vyhlada v zozname vrchol X a vyberie ho, vrati zoznam naslednikov
%

vv(_, [], [], []).

vv(X, v(X,Nasl), [], Nasl).

vv(_, v(Y,Nasl), v(Y,Nasl), []).

vv(X, [v(X,Nasl)|List], List, Nasl).

vv(X, [v(Y,Nasl)|List], [v(Y,Nasl)|List1], Nasl2) :-
	vv(X, List, List1, Nasl2).

%
%	Prejde orientovany graf do hlbky
%

prgh([], List, List, []). 

prgh([X], ListIn, ListOut, Out) :-
	(member(v(X,_), ListIn),
		vv(X, ListIn, List1, Nasl),
		prgh(Nasl, List1, ListOut, Out1),	
		app(X, Out1, Out)
	;
		ListOut = ListIn,
		Out = []
	).
	
prgh([X|Xs], ListIn, ListOut, Out) :-
	(member(v(X,_), ListIn),
		vv(X, ListIn, List1, Nasl),
		prgh(Nasl, List1, List2, Out1),	
		app(X, Out1, Out2),
		prgh(Xs, List2, ListOut, Out3),
		append(Out2, Out3, Out)
	;
		prgh(Xs, ListIn, ListOut, Out)
	).
	
%
%	Prejde do hlbky a vrati vstupny a vystupny cas
%
	
prghn(_, [], [], Nin, Nin, []).

prghn([], List, List, Nin, Nin, []).

prghn([X], ListIn, ListOut, Nin, Nout, Out) :-
	(member(v(X,_), ListIn),
		N1 is Nin + 1,
		vv(X, ListIn, List1, Nasl),
		prghn(Nasl, List1, ListOut, N1, N2, Out1),	
		app((X, N1, N2), Out1, Out),
		Nout is N2 + 1
	;
		ListOut = ListIn,
		Out = [],
		Nout = Nin
	).
	
prghn([X|Xs], ListIn, ListOut, Nin, Nout, Out) :-
	(member(v(X,_), ListIn),
		N1 is Nin + 1,
		vv(X, ListIn, List1, Nasl),
		prghn(Nasl, List1, List2, N1, N2, Out1),
		app((X, N1, N2), Out1, Out2),
		N3 is N2 + 1,
		prghn(Xs, List2, ListOut, N3, Nout, Out3),		
		app(Out2, Out3, Out)
	;
		prghn(Xs, ListIn, ListOut, Nin, Nout, Out)
	).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie N-arneho grafu do hlbky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ntph([], []).

ntph(nt(X, Nasl), [X|Out]) :-
	ntph(Nasl, Out).
	
ntph([nt(X, [])|List], [X|Out]) :-
	ntph(List, Out).	
	
ntph([nt(X, Nasl)|List], [X|Out]) :-
	ntph(Nasl, Out1),
	ntph(List, Out2),
	app(Out1, Out2, Out).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie N-arneho grafu do sirky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		
ntps([], []).

ntps([nt(X, Nasl)|List], [X|Out]) :-
	app(List, Nasl, Fronta),
	ntps(Fronta, Out).

ntps_s(nt(X, Nasl), [X|Out]) :-
	ntps(Nasl, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Prehladanie binarneho grafu do hlbky
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
btph(bt(nil, Value, nil), Value).

btph(bt(nil, Value, Right), Out) :- 
	btph(Right, Rightout),
	app(Value, Rightout, Out).
	
btph(bt(Left, Value, nil), Out) :- 
	btph(Left, Leftout),
	app(Leftout, Value, Out).
	
btph(bt(Left, Value, Right), Out) :- 
	btph(Left, Leftout),
	btph(Right, Rightout),
	app(Leftout, Value, Out1),
	app(Out1, Rightout, Out).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dán n-ární strom. Pro dané k vrate všechny listy, které jsou 
%	dosažitelné po cestì, na které souèet poøadových èísel vybíraných synù 
%	(zleva od 0) se rovná právì k.	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ntk([], _, _, []).
	
ntk([nt(X, [])|_], K, K, [X]).
	
ntk([nt(_, [])|List], N, K, Out) :-
	N1 is N + 1,
	ntk(List, N1, K, Out).	

ntk([nt(_, Nasl)|_], K, K, Out) :-
	ntk(Nasl, K, K, Out).

ntk([nt(_, Nasl)|List], N, K, Out) :-
	ntk(Nasl, N, K, Out1),
	N1 is N + 1,
	ntk(List, N1, K, Out2),
	app(Out1, Out2, Out).

ntk_s(nt(_, Nasl), K, Out) :-
	ntk(Nasl, 0, K, Out).
		
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dán graf. Zjistìte, zda je bipartitní a vydejte dotvrzující tøídy 
%	rozkladu vrcholù.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

%
%	Prida prvok alebo zoznam do zoznamu, bez duplicit
%

prid([], Y, Y).

prid([X], Y, Y) :-
	member(X, Y).
	
prid([X], Y, [X|Y]).

prid([X|Xs], Y, Out) :-
	member(X, Y),
	prid(Xs, Y, Out).

prid([X|Xs], Y, Out) :-
	prid(Xs, [X|Y], Out).

prid(X, Y, Y) :-
	member(X, Y).
	
prid(X, Y, [X|Y]).

%
%	Checkne ci su prvky zoznamu X v Liste, ak aspon jeden - chyba
%

check([], _).

check([X], List) :-
	(member(X, List),
		!, fail
	;
		true).
		
check([X|Xs], List) :-
	(member(X, List), 
		!, fail
	;
		check(Xs, List)
	).

%
%	Vrati mnoziny bipartitneho grafu
%

bipg([], G1in, G2in, G1in, G2in).

bipg([v(X,Nasl)], G1in, G2in, G1out, G2out) :-
	(check(Nasl, G1in),
		prid(X, G1in, G1out),
		prid(Nasl, G2in, G2out)
	;
		(check(Nasl, G2in),
			prid(X, G2in, G2out),
			prid(Nasl, G1in, G1out)
		;
			!, fail
		)
	).		

bipg([v(X,Nasl)|List], G1in, G2in, G1out, G2out) :-
	(check(Nasl, G1in),
		prid(X, G1in, G1p),
		prid(Nasl, G2in, G2p),
		(bipg(List, G1p, G2p, G1out, G2out);fail)
	;
		(check(Nasl, G2in),
			prid(X, G2in, G2p),
			prid(Nasl, G1in, G1p),
			bipg(List, G1p, G2p, G1out, G2out)
		;
			!, fail
		)
	).		
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Je dana hierarchie oken v okennim systemu ve forme n-arniho stromu. 
%	Kazde okno je dano hornim levym a dolnim pravym rohem. 
%	Upravte strom tak, ze zachovate strukturu a listy a do vsech vnitrich 
%	vrcholu stromu date nejmensi okno, ktere obsahuje vsechny podokna.	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

porok((Px1,Py1), (Lx1, Ly1), (Px2, Py2), (Lx2, Ly2), (Px, Py), (Lx, Ly)) :-
	(Px1 < Px2, Px = Px1; Px = Px2),
	(Py1 < Py2, Py = Py1; Py = Py2),
	(Lx1 > Lx2, Lx = Lx1; Lx = Lx2),
	(Ly1 > Ly2, Ly = Ly1; Ly = Ly2).

nto(nt(X, P1, L1, Nasl), nt(X, Pout, Lout, Out)) :-
	nto(Nasl, P2, L2, Out),
	porok(P1, L1, P2, L2, Pout, Lout).

nto([nt(X, P1, L1, [])|[]], P1, L1, nt(X, P1, L1, [])).
	
nto([nt(X, P1, L1, [])|List], Pout, Lout, [nt(X, P1, L1, [])|Out]) :-
	nto(List, P2, L2, Out),
	porok(P1, L1, P2, L2, Pout, Lout).	

nto([nt(X, P1, L1, Nasl)|[]], Pout, Lout, nt(X, Pout, Lout, Out1)) :-
	nto(Nasl, P2, L2, Out1),
	porok(P1, L1, P2, L2, Pout, Lout).
		
nto([nt(X, P1, L1, Nasl)|List], Pout, Lout, [nt(X, P1out, L1out, Out1)|Out2]) :-
	nto(Nasl, P2, L2, Out1),
	porok(P1, L1, P2, L2, P1out, L1out),
	nto(List, P3, L3, Out2),
	porok(P1out, L1out, P3, L3, Pout, Lout).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Pøidejte ke každému vrcholu v n-árním stromì dvì pøirozená èísla. 
%	První je èíslo poøadí vrcholu pøi prùchodu preorder, druhé pøi 
%	prùchodu postorder (zleva).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
ntpre([], N, N, A, A, []).

ntpre(nt(X, Nasl), N, N2, A, A2, nt(X, N, A1, Out)) :-
	N1 is N + 1,
	ntpre(Nasl, N1, N2, A, A1, Out),
	A2 is A1 + 1.
	
ntpre([nt(X, [])|List], N, N2, A, A2, [nt(X, N, A, [])|Out]) :-
	N1 is N + 1,
	A1 is A + 1,
	ntpre(List, N1, N2, A1, A2, Out).
	
	
ntpre([nt(X, Nasl)|List], N, N3, A, A3, [nt(X, N, A1, Out1)|Out2]) :-
	N1 is N + 1,
	ntpre(Nasl, N1, N2, A, A1, Out1),
	A2 is A1 + 1,
	ntpre(List, N2, N3, A2, A3, Out2).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Máte dán n-ární strom, který má v každém uzlu OKNO (dané souøadnicí levého 
%	horního a pravého dolního rohu). Napište predikát, co tento strom projde a 
%	oøeže každé okno podle jeho pøedka (tzn. to co pøes pøedka "pøeèuhuje" 
%	uøízne a vrátí nový strom). Pokud je celé mimo pøedka, zahoïte uzel i jeho 
%	potomky.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

porok2((Px1,Py1), (Lx1, Ly1), (Px2, Py2), (Lx2, Ly2), (Px, Py), (Lx, Ly)) :-
		(Px1 < Px2, Px = Px2; Px = Px1),
		(Py1 < Py2, Py = Py2; Py = Py1),
		(Lx1 > Lx2, Lx = Lx2; Lx = Lx1),
		(Ly1 > Ly2, Ly = Ly2; Ly = Ly1),
		(Px > Lx, !, fail; true),
		(Py > Ly, !, fail; true).

ntorez(nt(X, P1, L1, Nasl), nt(X, P1, L1, Out)) :-
	ntorez(Nasl, P1, L1, Out).

ntorez([nt(X, P1, L1, [])|[]], Pin, Lin, nt(X, Pout, Lout, [])) :-
	porok2(P1, L1, Pin, Lin, Pout, Lout).

ntorez([nt(_, _, _, [])|[]], _, _, []).
	
ntorez([nt(X, P1, L1, [])|List], Pin, Lin, [nt(X, Pout, Lout, [])|Out]) :-
	porok2(P1, L1, Pin, Lin, Pout, Lout),
	ntorez(List, Pin, Lin, Out).

ntorez([nt(_, _, _, [])|List], Pin, Lin, Out) :-
	ntorez(List, Pin, Lin, Out).

ntorez([nt(X, P1, L1, Nasl)|[]], Pin, Lin, nt(X, Pout, Lout, Out1)) :-
	porok2(P1, L1, Pin, Lin, Pout, Lout),
	ntorez(Nasl, Pout, Lout, Out1).

ntorez([nt(_, _, _, _)|[]], _, _, []).

ntorez([nt(X, P1, L1, Nasl)|List], Pin, Lin, [nt(X, P1out, L1out, Out1)|Out2]) :-
	porok2(P1, L1, Pin, Lin, P1out, L1out),
	ntorez(Nasl, P1out, L1out, Out1),
	ntorez(List, Pin, Lin, Out2).

ntorez([nt(_, _, _, _)|List], Pin, Lin, Out) :-
	ntorez(List, Pin, Lin, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najdi maximalni nezavislou mnozinu v grafu (nemusi byt nejvetsi mozna)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
%	failne, ak nie je Y (ktory je spojeny hranou s X) v Liste
%
	
member2(_, [], _) :-
	!, fail.
	
member2(X, [h(X, Y)|_], List) :-
	member(Y, List).
	
member2(X, [h(Y, X)|_], List) :-
	member(Y, List).		

member2(X, [h(_, _)|Ys], List) :-
	member2(X, Ys, List).

najdinm([X|Xs], Hrany, List, Out) :-
	member2(X, Hrany, List),
	najdinm(Xs, Hrany, List, Out).
	
najdinm([X|Xs], Hrany, List, [X|Out]) :-
	najdinm(Xs, Hrany, [X|List], Out).
	
najdinm([X], Hrany, List, []) :-
	member2(X, Hrany, List).

najdinm([X], _, _, [X]).

najdinm([], _, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Kartezky soucin 2 grafu (bez orientace hran) 	
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ks2(_, _, [], []).

ks2(X, Y, [h(A, B) | G2], Out) :-
	app([h(X*A, Y*B)], [h(X*B, Y*A)], Out1),
	ks2(X, Y, G2, Out2),
	app(Out1, Out2, Out).

ks([], _, []).

ks(_, [], []).

ks([h(X,Y)|G1], G2, Out) :-
	ks2(X, Y, G2, Out1),
	ks(G1, G2, Out2),
	app(Out1, Out2, Out).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	 Prevod n-arniho stromu na binarni
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ntobin([], nil).

ntobin([nt(X, Nasl)|List], bt(Out1, X, Out2)) :-
	ntobin(Nasl, Out1),
	ntobin(List, Out2).

ntobin(nt(X, Nasl), bt(Out, X, nil)) :-
	ntobin(Nasl, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Dan graf. naleznete v nem vsechny cesty delky 2 a pridejte k puvodnimu 
%	grafu a vratte jako vysledek.
%	
%	Pozn.: graf je zadany vrchol(x, [zoznam naslednikov]), tj. bude fungovat
%	aj pre orientovane grafy, horsie to bude uz s ohodnotenim hran 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gdoh2([X|Xs], G, K, K, [X|Out]) :-
	member(v(X, _), G),
	gdoh2(Xs, G, K, K, Out).

gdoh2([_|Xs], G, K, K, Out) :-
	gdoh2(Xs, G, K, K, Out).

gdoh2([X|Xs], G, N, K, Out) :-
	vv(X, G, G1, Nasl),				%impl. vyssie
	N1 is N + 1,
	gdoh2(Nasl, G1, N1, K, Out1),
	gdoh2(Xs, G, N, K, Out2),
	app(Out1, Out2, Out).

gdoh2([X], _, K, K, [X]).

gdoh2(_, _, _, _, []).
	
gdoh1(X, G, K, v(X, Out)):-
	vv(X, G, G1, Nasl),
	gdoh2(Nasl, G1, 1, K, Out1),
	prid(Out1, Nasl, Out).
	
gdoh([], _, _, []).
	
gdoh([X|Xs], G, K, [Out1|Out2]) :-
	gdoh1(X, G, K, Out1),
	gdoh(Xs, G, K, Out2).
			

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Najdete sedlovy bod matice
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getline([], [], []).

getline([[X|Riadok]|Mat], XOut, MatOut) :-
	getline(Mat, NextX, NextRiadok),
	app(NextX, X, XOut),
	app([Riadok], NextRiadok, MatOut).

najdi([], Bmax1, Bmin1, Bmax1, Bmin1).	

najdi([X|Xs], Bmax1, Bmin1, BmaxOut, BminOut) :-
		(X > Bmax1, Bmax2 = X; Bmax2 = Bmax1),
		(X < Bmin1, Bmin2 = X; Bmin2 = Bmin1),
		najdi(Xs, Bmax2, Bmin2, BmaxOut, BminOut), !.

najdi(X, Bmax1, Bmin1, BmaxOut, BminOut) :-
		(X > Bmax1, BmaxOut = X; BmaxOut = Bmax1),
		(X < Bmin1, BminOut = X; BminOut = Bmin1), !.

najdi1([], [], []).
		
najdi1([[X|Xs]|List], [Bmax1|Bmax2], [Bmin1|Bmin2]) :-
	najdi(Xs, X, X, Bmax1, Bmin1),
	najdi1(List, Bmax2, Bmin2).
	
najdi3([[]|_], _, _, _) :-
	!, fail.
	
najdi3(Mat, Bmax, Bmin, Sedl) :-
	getline(Mat, [X|Riadok], NewMat),
	najdi(Riadok, X, X, Bmax1, Bmin1),
	(member(Bmax1, Bmin),
		Sedl = Bmax1
	;
		(member(Bmin1, Bmax),
			Sedl = Bmin1 
		;
			!, najdi3(NewMat, Bmax, Bmin, Sedl)
		)
	).
	
	
najdi2(Mat, Sedl) :-
	najdi1(Mat, Bmax, Bmin),
	najdi3(Mat, Bmax, Bmin, Sedl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Otocenie matice doprava
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getline1([], [], []).

getline1([[X|Riadok]|Mat], RiadokOut, [Riadok|MatOut]) :-
	getline1(Mat, Xs, MatOut),
	app(Xs, X, RiadokOut).
	
otocmat([[]|_], []).

otocmat(Mat, MatOut) :-
	getline1(Mat, Riadok, NewMat),
	otocmat(NewMat, NewMat1),
	app([Riadok], NewMat1, MatOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Maté orientovaný graf. Máte množinu vrcholù. Sluète tuto množinu vrcholù 
%	do jednoho. (Tzn místo této množiny bude ve výsledném grafu jen jeden nový 
%	vrchol, místo všech hran vedoucích z nìjakého vrcholu do/z libovolného 
%	vrcholu této množiny jen jedna vedoucí do/z toho nového vrcholu)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

x(1). x(2). x(5). x(7). x(6). x(9).
h(1,2). h(1,7). h(2,3). h(2,5). h(3,4). h(5,6). h(6,4). h(7,5). h(7,8). h(8,6). 
h(9,8). h(10,11).

zluc(S):- setof(h2(X,Y),h2(X,Y),S).
h2(X,Y):-h(X,Y),\+ x(X), \+ x(Y).
h2(n,Y):-h(X,Y),x(X), \+ x(Y).
h2(X,n):-h(X,Y),\+ x(X), x(Y).	

%%

v(1,[2,7]). v(2,[1,3,5]). v(3,[2,4]). v(4,[3,6]). v(5,[2,6,7]). v(6,[4,5,8]). 
v(7,[1,5,8]). v(8,[6,7,9]). v(9,[8]). v(10,[11]). v(11, [10]).

spoc([], N, N).

spoc([_|Xs], N, Out) :-
	N1 is N + 1,
	spoc(Xs, N1, Out).

pochran(Nin, Nout) :-
	(v(X, Nasl),
		spoc(Nasl, 0, N),
		assert(v2(X, N, Nasl)),
		(N > Nin, N1 = N; N1 = Nin),
		pochran(N1, Nout)
	;
		Nout = Nin).

pochran_test1(X, N) :- 
	pochran(0, N), 
	bagof(v2(A,Y,Z), v2(A,Y,Z), X).

