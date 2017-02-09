

%otoc2(+S,-Z). Otoci seznam S a vrati vysledek v Z.
otoc2(S,Z):- otoc2(S,[],Z).
%otoc2(+S, @A, -Z). Otoci seznam S a vrati vysledek v Z, A je akumulator.
otoc2([], A, A). 
otoc2([X|Xs], A, Z):- otoc2(Xs, [X|A], Z).


prvniprvky([],[],[]).
prvniprvky([[A|B]|T],[A|Sa],[B|Sb]) :- prvniprvky(T,Sa,Sb).

%vrstvy(+Sez,-SezIndexu) :- Sez je seznam seznamu (stejne delky)
%                           SezIndexu je seznam n-tych prvku Sezu.
%pr. vrstvy([[1,2,3],[4,5,6]],X). X=[[1,4],[2,5],[3,6]].
vrstvy([],[]).
vrstvy([[]|T],[]) :- vrstvy(T,[]).
vrstvy(P,[X|Xs]) :- prvniprvky(P,X,S), vrstvy(S,Xs). 

%otoc_mat(+Mat,-OtMat) :- otoci matici o 90 stupnu doleva
otoc_matici(M,OM) :- otoc_radky(M,OR), otoc_mat(OR,OM).

otoc_radky([],[]).
otoc_radky([R|Rx],[S|Sx]) :- otoc2(R,S), otoc_radky(Rx,Sx).

otoc_mat(OR,OM) :- vrstvy(OR,OM).  


%spirala(+M,-S) :- "oloupa matici M do seznamu S" z L hor. rohu po smeru hod. ruc.

spirala(M,S) :- spirala(M,[],S).

% s akumulatorem

spirala([H|T],A,S) :- append(A,H,NA), otoc_matici(T,OT), spirala(OT,NA,S).
spirala([],A,A).


