%%% Zapoctak, Neproceduralni programovani MFF UK 2015/16
%%% Marek Židek
%%% Pentomino

/*** otocene a osova soumernost jsou stejne ***/

% kazdy obrazec muze byt otocen a i prevracen, jsou ukladany jako seznam ctvercu relativne vzhledem k levemu hornimu ctverci
% muze byt az 8 moznosti otoceni a prevraceni

obrazec(1,'I').
obrazec(2,'P').
obrazec(3,'Y').
obrazec(4,'V').
obrazec(5,'X').
obrazec(6,'L').
obrazec(7,'F').
obrazec(8,'Z').
obrazec(9,'T').
obrazec(10,'W').
obrazec(11,'N').
obrazec(12,'U').

/*
obrazec(1,'I').
obrazec(2,'O').
obrazec(3,'_').
*/
/*
variantyObrazce('O',[[1,0, 2,0, 3,0, 0,1, 1,1, 2,1, 3,1],
           	    [0,1, 0,2, 0,3, 1,0, 1,1, 1,2, 1,3],
           		x,x,x,x,x,x]).

variantyObrazce('_', [[1,0],
           	    [0,1],
           		x,x,x,x,x,x]).
           		*/

/*   00   00     0     0     0      0    0    0
*   00     00   00     00   000   000   000   000
*    0     0     00   00      0    0    0      0
*/
variantyObrazce('F', [[-1,1, -1,2,  0,1, 1,1],
           [ 0,1,  1,1,  1,2, 2,1],
           [-1,1,  0,1,  2,1, 2,2],
           [-2,1, -1,1, -1,2, 0,1],
           [-1,1,  0,1,  0,2, 1,2],
           [-1,2,  0,1,  0,2, 1,1],
           [-1,1,  0,1,  0,2, 2,0],
           [ 1,0,  1,1,  1,2, 2,1]]).


%  0   00000
%  0
%  0
%  0
%  0

% x znaci vyplnovy symbol
variantyObrazce('I',[[1,0, 2,0, 3,0, 4,0],
           	    [0,1, 0,2, 0,3, 0,4],
           		x,x,x,x,x,x]).


% a tak dale pro vsechny v poradi tak, jak je na ceske wikipedii Pentomino
variantyObrazce('L',[[ 1,0,  2,0,  3,0, 3,1],
           [-3,1, -2,1, -1,1, 0,1],
           [ 0,1,  1,0,  2,0, 3,0],
           [ 0,1,  1,1,  2,1, 3,1],
           [ 0,1,  0,2,  0,3, 1,0],
           [ 1,0,  1,1,  1,2, 1,3],
           [ 0,1,  0,2,  0,3, 1,3],
           [-1,3,  0,1,   0,2, 0,3]]).


variantyObrazce('N',[[ 1,0,  1,1,  2,1, 3,1],
           [-1,1,  0,1,  1,0, 2,0],
           [-2,1, -1,1,  0,1, 1,0],
           [ 1,0,  2,0,  2,1, 3,1],
           [-1,2, -1,3,  0,1, 0,2],
           [ 0,1,  0,2,  1,2, 1,3],
           [ 0,1,  1,1,  1,2, 1,3],
           [-1,1, -1,2, -1,3, 0,1]]).


variantyObrazce('P',[[ 0,1,  1,0, 1,1, 2,0],
           [ 0,1,  1,0, 1,1, 2,0],
           [ 1,0,  1,1, 2,0, 2,1],
           [-1,1,  0,1, 1,0, 1,1],
           [ 0,1,  0,2, 1,1, 1,2],
           [-1,1, -1,2, 0,1, 0,2],
           [ 0,1,  0,2, 1,0, 1,1],
           [ 0,1,  1,0, 1,1, 1,2]]).


variantyObrazce('T',[[ 0,1,  0,2, 1,1, 2,1],
           [-2,1, -1,1, 0,1, 0,2],
           [-1,2,  0,1, 0,2, 1,2],
           [ 1,0,  1,1, 1,2, 2,0],
           x,x,x,x]).


variantyObrazce('U',[[0,2, 1,0, 1,1, 1,2],
           [0,1, 0,2, 1,0, 1,2],
           [0,1, 1,0, 2,0, 2,1],
           [0,1, 1,1, 2,0, 2,1],
           x,x,x,x]).


variantyObrazce('V',[
		       [ 1,0,   2,0, 2,1, 2,2],
           [-2,2,  -1,2, 0,1, 0,2],
           [ 0,1,   0,2, 1,2, 2,2],
           [ 0,1,   0,2, 1,0, 2,0],
           x,x,x,x]).


variantyObrazce('W',[[ 1,0,  1,1,  2,1, 2,2],
           [-2,2, -1,1, -1,2, 0,1],
           [-1,1, -1,2,  0,1, 1,0],
           [ 0,1,  1,1,  1,2, 2,2],
           x,x,x,x]).


variantyObrazce('X',[[-1,1, 0,1, 0,2, 1,1],
           x,x,x,x,x,x,x]).



variantyObrazce('Y',[[ 0,1,  0,2, 0,3, 1,1],
           [ 1,0,  1,1, 2,0, 3,0],
           [-1,1,  0,1, 1,1, 1,2],
           [ 1,0,  2,0, 2,1, 3,0],
           [-2,1, -1,1, 0,1, 1,1],
           [ 0,1,  0,2, 0,3, 1,2],
           [-1,2,  0,1, 0,2, 0,3],
           [-1,1,  0,1, 0,2, 0,3]]).



variantyObrazce('Z',[[ 0,1,  1,1,  2,1, 2,2],
           [-2,1, -2,2, -1,1, 0,1],
           [-1,2, 0,1,  0,2, 1,0],
           [ 1,0,  1,1,  1,2, 2,2],
           x,x,x,x]).

moznost(1).
moznost(2).
moznost(3).
moznost(4).
moznost(5).
moznost(6).
moznost(7).
moznost(8).

% plocha(+userInput, -Plocha, -Sirka) Plocha jako list listu
plocha(1, Plocha, Sirka) :- plocha(0, 6, 10, Plocha), Sirka = 10.
plocha(2, Plocha, Sirka) :- plocha(0, 5, 12, Plocha), Sirka = 12.
plocha(3, Plocha, Sirka) :- plocha(0, 4, 15, Plocha), Sirka = 15.
plocha(4, Plocha, Sirka) :- plocha(0, 3, 20, Plocha), Sirka = 20.

%testovaci
plocha(5, Plocha, Sirka) :- plocha(0, 5, 3, Plocha), Sirka= 3.

plocha(Acc, Acc, Sirka, [[X]]) :- X is Sirka * Acc. % posledni prvek u plochy je pocet dilku.
plocha(Acc, Vyska, Sirka, [X|Plocha]) :- Acc1 is Acc + 1, makeRow(Sirka, X), plocha(Acc1, Vyska, Sirka, Plocha).

% makeRow(+pocetPrvku, -X)
makeRow(PocetPrvku, [0|Tail]) :- PocetPrvku > 0, Count is PocetPrvku-1, makeRow(Count, Tail).
makeRow(0, []).


solve1(Plocha, _,_, 12, _, Vysledek) :- Plocha = Vysledek.
solve1(Plocha, Sirka, CisloObrazce, Acc, Pouzite, Vysledek) :-
                    last(Plocha, [PocetDilku]), obrazec(CisloObrazce, O), nebylPouzit(O, Pouzite), for(Dilek, 0, PocetDilku), moznost(Opt),
                      zkusVlozit(O, Opt, Plocha, Sirka, Dilek, NovaPlocha), heuristika1(NovaPlocha, Sirka, 0, PocetDilku), heuristika2(NovaPlocha, Sirka, 0, PocetDilku),
                      heuristika3(NovaPlocha, Sirka, PocetDilku), Acc1 is Acc + 1, CisloObrazce1 is CisloObrazce + 1,
                      solve1(NovaPlocha, Sirka, CisloObrazce1, Acc1, [O|Pouzite], Vysledek).


%Tady je jedno, co do Vysledku ulozime, hlavne neco, aby to findall/3 zaregistroval
solve(_,_,_,12,_,Vysledek) :- Vysledek = 1.
solve(Plocha, Sirka, CisloObrazce, Acc, Pouzite, Vysledek) :-
										last(Plocha, [PocetDilku]), obrazec(CisloObrazce, O), nebylPouzit(O, Pouzite), for(Dilek, 0, PocetDilku), moznost(Opt),
									    zkusVlozit(O, Opt, Plocha, Sirka, Dilek, NovaPlocha), heuristika1(NovaPlocha, Sirka, 0, PocetDilku), heuristika2(NovaPlocha, Sirka, 0, PocetDilku),
									    heuristika3(NovaPlocha, Sirka, PocetDilku), Acc1 is Acc + 1, CisloObrazce1 is CisloObrazce + 1,
									    solve(NovaPlocha, Sirka, CisloObrazce1, Acc1, [O|Pouzite], Vysledek).


%pro BFS na nalezeni sousedu
kolem(Dilek, Sirka, Dalsi) :- Dalsi is Dilek - Sirka.
kolem(Dilek, Sirka, Dalsi) :- Dalsi is Dilek + Sirka.
kolem(Dilek, Sirka, Dalsi) :- Dalsi is Dilek + 1.
kolem(Dilek, Sirka, Dalsi) :- Dalsi is Dilek - 1.

%hleda obklicene dvojice
heuristika2(Plocha, Sirka, Dilek, PocetDilku) :- h2(Plocha, Sirka, Dilek, PocetDilku).
h2(_,_,PocetDilku, PocetDilku) :- !.
h2(Plocha, Sirka, Dilek, PocetDilku) :- AccD is Dilek + 1, \+ heuristikaB(Plocha, Sirka, Dilek, PocetDilku), h2(Plocha, Sirka, AccD, PocetDilku).

heuristikaB(Plocha, Sirka, Dilek, PocetDilku) :- volneMisto(Plocha, Dilek, Sirka), kolem(Dilek, Sirka, Dalsi),
												 volneMisto(Plocha, Dalsi, Sirka), listify(Plocha, List),
												 volneMistoAZaber(List, Dilek, NovyList), volneMistoAZaber(NovyList, Dalsi, NovyList1),
												 last(NovyList1, CelkemPoli), to2dList(NovyList1, Sirka, 0, 0, CelkemPoli,NovaPlocha), 
												 h2VsudeObsazeno(NovaPlocha, Sirka, Dilek), h2VsudeObsazeno(NovaPlocha, Sirka, Dalsi).

												 
h2VsudeObsazeno(Plocha, Sirka, Dilek) :- \+ volneMisto(Plocha, Dilek, Sirka), 
										 U is Dilek - Sirka, \+ volneMisto(Plocha, U, Sirka),
										 D is Dilek + Sirka, \+ volneMisto(Plocha, D, Sirka),
										 R is Dilek + 1, \+ volneMistoH1(Plocha, Dilek, R, Sirka),
										 L is Dilek - 1, \+ volneMistoH1(Plocha, Dilek, L, Sirka).


%hleda obklicene body
heuristika1(Plocha, Sirka, Dilek, PocetDilku) :- h1(Plocha, Sirka, Dilek, PocetDilku).

h1(Plocha, Sirka, PocetDilku, PocetDilku) :- !.
h1(Plocha, Sirka, Dilek, PocetDilku) :- AccD is Dilek + 1, \+ heuristikaA(Plocha, Sirka, Dilek, PocetDilku),
											   h1(Plocha, Sirka, AccD, PocetDilku).

heuristikaA(Plocha, Sirka, Dilek, PocetDilku) :- volneMisto(Plocha, Dilek, Sirka), U is Dilek - Sirka, \+ volneMisto(Plocha, U, Sirka),
										 D is Dilek + Sirka, \+ volneMisto(Plocha, D, Sirka),
										 R is Dilek + 1, \+ volneMistoH1(Plocha, Dilek, R, Sirka),
										 L is Dilek - 1, \+ volneMistoH1(Plocha, Dilek, L, Sirka). 

volneMistoH1(Plocha, D1, D2, Sirka) :- D2 >= 0, Pomocna1 is D1 div Sirka, Pomocna2 is D2 div Sirka, Pomocna2 = Pomocna1, volneMisto(Plocha, D2, Sirka).


%prochazi BFS vsechny volne plochy a chce, aby byly delitelne 5
heuristika3(Plocha, Sirka, PocetDilku) :- heuristika(Plocha, Sirka, 0, PocetDilku).

heuristika(_,_, PocetDilku, PocetDilku) :- !.
heuristika(X) :- X \= 3, X \= 4, X \= 6, X \= 7, X \= 8, X \= 9, X \= 11,
				 X \= 12, X \= 13, X \= 14, X \= 16, X \= 17, X \= 18, X \= 19,
				 X \= 21, X \= 22, X \= 23, X \= 24, X \= 26, X \= 27, X \= 28,
				 X \= 29, X \= 31, X \= 32, X \= 33, X \= 34, X \= 36, X \= 37,
				 X \= 38, X \= 39, X \= 41, X \= 42, X \= 43, X \= 44, X \= 46,
				 X \= 47, X \= 48, X \= 49.
heuristika(Plocha, Sirka, AccDilek, PocetDilku) :- listify(Plocha, List), bFS(Sirka, List, [AccDilek], 0, Kolik, 1), AccD1 is AccDilek + 1, 
												 heuristika(Kolik), heuristika(Plocha, Sirka, AccD1, PocetDilku).


%Kolik -> znaci pocet bodu souvisle nezaplnene plochy -> pote se testuje delitelnost 5ti
%Posledni argument znaci, jestli '1' nalezli jsme novy volny bod ve Fronte, nebo  '0' hledame pro vsechny sousedy a davame do fronty.
bFS(_,_,[],Kolik,Kolik,1).
bFS(Sirka, List, [H|T], Acc, Kolik, 1) :- volneMistoAZaber(List, H, NovaPlocha) -> Acc1 is Acc + 1, bFS(Sirka, NovaPlocha, [H|T], Acc1, Kolik, 0);
										  bFS(Sirka, List, T, Acc, Kolik, 1).

bFS(Sirka, List, [H|T], Acc, Kolik, 0) :- NPoz1 is H + 1, NPoz2 is H - 1, NPoz3 is H + Sirka, NPoz4 is H - Sirka,
										  last(List, PocetDilku),
										  checkOkrajeH(H, NPoz1, Sirka, PocetDilku,Poz1),checkOkrajeH(H, NPoz2,Sirka, PocetDilku,Poz2),
										  checkOkrajeV(H, NPoz3, Sirka, PocetDilku,Poz3),checkOkrajeV(H, NPoz4, Sirka, PocetDilku,Poz4),
										  bFS(Sirka, List, [Poz1,Poz2,Poz3, Poz4|T], Acc, Kolik, 1).

%vertikalni			
checkOkrajeV(Puvodni, Vedlejsi, Sirka, PocetDilku, UpravenaPozice) :- Vedlejsi >= PocetDilku -> UpravenaPozice = -1;
																	 Vedlejsi < 0 -> UpravenaPozice = -1;
																	 UpravenaPozice = Vedlejsi.
%horizontalni
checkOkrajeH(Puvodni, Vedlejsi, Sirka, PocetDilku, UpravenaPozice) :- Vedlejsi >= PocetDilku -> UpravenaPozice = -1;
																	 Vedlejsi < 0 -> UpravenaPozice = -1;
																	 Radek1 is Puvodni div Sirka, Radek2 is Vedlejsi div Sirka,
																	 Radek1 \= Radek2 -> UpravenaPozice = -1;
																	 UpravenaPozice = Vedlejsi.																	

volneMistoAZaber([1|T], 0, _):- !, fail.
volneMistoAZaber([0|T], 0, [1|T]).
volneMistoAZaber([H|T], Pozice, [H|R]) :- I1 is Pozice - 1, volneMistoAZaber(T,I1,R).					

nebylPouzit(_, []) :- !.
nebylPouzit(O, [H|Tail]) :- O \= H, nebylPouzit(O, Tail).

%vybere spravnou varianty obrazce a vlozi do NovaPlocha
zkusVlozit(O, Opt, Plocha, Sirka, Dilek, NovaPlocha) :- variantyObrazce(O, ListVariant), getVariantuObrazce(ListVariant, 1, Opt, X), vloz(X, O, Dilek, Sirka, Plocha, NovaPlocha).

getVariantuObrazce([H|T], Counter, Counter, H).
getVariantuObrazce([H|T], Counter, Opt, X) :- Counter1 is Counter + 1, getVariantuObrazce(T, Counter1, Opt, X).

%Sirka je pro snadne propocitavani prvni souradky u posunu
%fillPlocha zaplni Pismenky pro Obrazec
vloz(Obrazec, ZnakObrazce, Pozice, Sirka, Plocha, NovaPlocha) :- volneMisto(Plocha, Pozice, Sirka), volneMista(Obrazec, Pozice, Sirka, Plocha),
                                                                 fillPlocha(Plocha, ZnakObrazce, Obrazec, Pozice, Sirka, NovaPlocha).


volneMisto(Plocha, Pozice, Sirka) :- Pozice >= 0, last(Plocha, [PocetDilku]), Pozice =< PocetDilku, Radek is Pozice div Sirka,
									 getRow(Plocha, Radek, List), PoziceVRadku is Pozice mod Sirka, getFromRow(List, PoziceVRadku, Hodnota),
									 Hodnota = 0.

volneMista([], _, _, _).
%Down, Right se neunifikuje s variantou 'x', tedy prazdnou variantou Obrazce
%Prvni argument je Obrazec = kam od Vychozi pozice
volneMista([Down,Right|T], Pozice, Sirka, Plocha) :- Radek is Pozice div Sirka, OkrajeCheck is Pozice + Right, OkrajeCheck >= 0, NovyRadek is OkrajeCheck div Sirka,
                           Radek = NovyRadek, NovaPozice is Pozice + (Sirka * Down) + Right, volneMisto(Plocha, NovaPozice, Sirka),
                           volneMista(T, Pozice, Sirka, Plocha).

fillPlocha(Plocha, ZnakObrazce, Obrazec, Pozice, Sirka, NovaPlocha) :- listify(Plocha, PlochavListu), jeden(PlochavListu, ZnakObrazce, Pozice, UpravenaPlocha),
                                                                       fillPlochaList(UpravenaPlocha, ZnakObrazce, Obrazec, Pozice, Sirka, NovaPlocha).
fillPlochaList(Plocha, _, [], _,Sirka,NovaPlocha) :- last(Plocha, CelkemPoli), to2dList(Plocha, Sirka, 0, 0, CelkemPoli,NovaPlocha).
fillPlochaList(Plocha, ZnakObrazce, [Down, Right|T], Pozice, Sirka, NovaPlocha) :- NovaPozice is Pozice + (Sirka * Down) + Right,
																	  jeden(Plocha, ZnakObrazce, NovaPozice, UpravenaPlocha), fillPlochaList(UpravenaPlocha, ZnakObrazce, T, Pozice, Sirka, NovaPlocha).

jeden([_|T], ZnakObrazce, 0, [ZnakObrazce|T]).
jeden([H|T], ZnakObrazce, Pozice, [H|R]) :- I1 is Pozice - 1, jeden(T, ZnakObrazce, I1,R).

listify([], PlochavListu) :- PlochavListu = [].
listify([H|T], PlochavListu) :- append(H, P, PlochavListu), listify(T, P).

to2dList(Plocha, Sirka, Acc, CelkemPoli, CelkemPoli, [[CelkemPoli]]) :- !.
to2dList(Plocha, Sirka, Acc, Do1, CelkemPoli, [Row|T]) :- Od is Acc * Sirka, Acc1 is Acc + 1, Do is Od + Sirka, sublist(Plocha, Od, Do, Row),
										 to2dList(Plocha, Sirka, Acc1, Do, CelkemPoli, T).



sublist(L, M, N, S) :-
    findall(E, (nth1(I, L, E), I > M, I =< N), S).


getRow(Plocha, Radek, List) :- getRow(Plocha, Radek, 0, List).

getRow([H|T], Radek, Radek, List) :- List = H.
getRow([H|T], Radek, Acc, List) :- Acc1 is Acc + 1, getRow(T, Radek, Acc1, List).

getFromRow(List, Pozice, Hodnota) :- getFromRow(List, 0, Pozice, Hodnota).

getFromRow([H|T], Pozice, Pozice, Hodnota) :- Hodnota = H.
getFromRow([H|T], Acc, Pozice, Hodnota) :- Acc1 is Acc + 1, getFromRow(T, Acc1, Pozice, Hodnota).


for(M,M,N):- M < N.
for(I,M,N):- M < N, M1 is M + 1, for(I,M1,N).

main :- repeat,
		write('Resic pentomino'), nl,
		write('Vyber velikost plochy'), nl,
		write(' 1. 6x10'),nl,
		write(' 2. 5x12'),nl,
		write(' 3. 4x15'),nl,
		write(' 4. 3x20'),nl,
		read_int(X),
		integer(X), nl, X>0, X<5, 
		plocha(X, Plocha, Sirka),
    plocha(1, UkazkovaPlocha, 10),
    solve1(UkazkovaPlocha, 10, 1, 0, [], GrafickyVysl), % Solve s pomoci 3 prvnich obrazcu preferovane natocenych pro demonsraci
    printVysl(GrafickyVysl),
		findall(Vysledek, solve(Plocha, Sirka, 1, 0, [], Vysledek), Bag), %hleda vsechny reseni (Extremne zdlouhave)
		length(Bag, Pocet), write(Pocet), nl. %vypise pocet


printVysl([[60]|[]]).
printVysl([H|T]) :- printRow(H), printVysl(T).

printRow([]) :- nl.
printRow([H|T]) :- write(H), write(' '), printRow(T).

read_int(N):- 		read_int(N,0).
read_int(N,N1):-	get0(C),
			((C = 10;C=13), N is N1,!
			;
			C >= "0", C =< "9", N2 is N1*10 + C - "0", read_int(N,N2),!
			).




