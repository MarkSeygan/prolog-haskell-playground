
%dvojice(S,A-B) :- member(A,S), member(B,S), A\=B.




kart1(_,[],[]).
kart1(X,[Y|Ys],[[X,Y]|S]) :-    kart1(X,Ys,S), \+member([X,Y],S), X\=Y.
%kart1(X,[Y|Ys],[X-Y|S]) :-      kart1(X,Ys,S), \+member(X-Y,S), member(Y-X,S).
%kart1(X,[Y|Ys],[Y-X|S]) :-      kart1(X,Ys,S), member(X-Y,S), \+member(Y-X,S).
kart1(X,[Y|Ys],S) :-            kart1(X,Ys,S), (member([X,Y],S); X=Y).

kart([],_,[]).
kart([X|Xs],Y,S) :- kart1(X,Y,S1), kart(Xs,Y,S2), append(S1,S2,S).

unzip([A-B],[A],[B]).
unzip([A-B|S],[A|Xs],[B|Ys]) :- unzip(S,Xs,Ys).
