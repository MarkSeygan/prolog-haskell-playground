% máte ČUM ([a>-b, a>-c, ...]). Vypište všechny neporovnatelné dvojice (ve formě seznamu), tzn například [b-c, ...]

%vrcholy(+CUM,-SeznamVrcholu)
vrcholy([],[]).
vrcholy([X-Y|C],[X,Y|S]) :- vrcholy(C,S), \+member(X,S), \+member(Y,S) .
vrcholy([X-Y|C],[Y|S]) :-   vrcholy(C,S), member(X,S), \+member(Y,S).
vrcholy([X-Y|C],[X|S]) :-   vrcholy(C,S),  member(Y,S), \+member(X,S).
vrcholy([X-Y|C], S) :-      vrcholy(C,S), member(X,S), member(Y,S).

% jevetsi (+X,+Y,+CUM)
jevetsi(X,Y,C) :- member(X-Y,C).
jevetsi(X,Y,C) :- member(X-A,C), jevetsi(A,Y,C).

% X a Y jsou porovnatelne, pokud mezi nimi existuje cesta
cesta(X,X,C) :- vrcholy(C,V), member(X,V).

cesta(X,Y,C) :- jevetsi(X,Y,C).
cesta(X,Y,C) :- jevetsi(Y,X,C). % misto "je mensi"

%neporovnatelne(-X,-Y,+CUM) :- X a Y jsou v CUM neporovnatelne
neporovnatelne(X,Y,CUM) :-  vrcholy(CUM,V), member(X,V), member(Y,V), 
                            \+cesta(X,Y,CUM).

%kartezsky soucin jednoho prvku se seznamem
kart1(_,[],[],_).
kart1(X,[Y|Ys],[X-Y|S],CUM) :- \+cesta(X,Y,CUM),kart1(X,Ys,S,CUM).
kart1(X,[Y|Ys],S,CUM) :- cesta(X,Y,CUM), kart1(X,Ys,S,CUM).

%kart.soucin seznamu a seznamu
kart([],_,[],_).
kart([V|Vs],W,S,CUM) :- kart1(V,W, S1,CUM), kart(Vs,W,S2,CUM), append(S1,S2,S).

%vypis neporovnatelne prvky v seznamu
neporovnatelneList(CUM,S) :- vrcholy(CUM,V), kart(V,V,S,CUM).










