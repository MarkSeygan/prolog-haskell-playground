% Definujte predikat orez(+Strom,+D,+H,-VStrom), ktery ve Stromu (BVS) ponecha
% jen uzly V takove, ze D <= V <= H

% priklad vstupu
% orez(t(t(void,5,void), 10, t(t(void,15,void), 20, t(void,30,void))),10,20,T).
% T = t(void, 10, t(t(void, 15, void), 20, void)).

%------------------------------------------------------------------------------%

% inorderList(+T,-S) :- prevadi strom T na seznam S v inorder poradi
% L -> A -> P
inorderList(nil,[]).
inorderList(t(X,Y,Z),S) :- inorderList(X,S1),inorderList(Z,S2),append(S1,[Y|S2],S).

% pridejBST(+T,+X,+S) :- prida prvek X do stromu T a vrati strom S
pridejBST(nil, X, t(nil,X,nil)).
pridejBST(t(L,U,P),X,t(T,U,P)):-X=<U,pridejBST(L,X,T).
pridejBST(t(L,U,P),X,t(L,U,T)):-X>U,pridejBST(P,X,T).

% seznamNaBST(+S,-T) :- vytvori BST T ze seznamu S
seznamNaBST(S,T):-seznamNaBST(S,nil,T).
seznamNaBST([],T,T).
seznamNaBST([H|Hs],T1,T):-pridejBST(T1,H,T2),seznamNaBST(Hs,T2,T).

% protrid(+S1,+X,+Y,-S2) :- v seznamu S2 jsou prvky P z S1 : X <= P <= Y
protrid([],_,_,[]).
protrid([A|As],X,Y,[A|B])   :- X =< A, A =< Y, protrid(As,X,Y,B).
protrid([A|As],X,Y,B)       :- X > A, protrid(As,X,Y,B).
protrid([A|As],X,Y,B)       :- A > Y, protrid(As,X,Y,B).

orez(T1,D,H,T2) :- inorderList(T1,S1), protrid(S1,D,H,S2), seznamNaBST(S2,T2).

%-------druhe reseni---%

orez2(void,_,_,void).

%koren zustava
orez2(t(X,Y,Z),D,H,t(X2,Y,Z2)) :- D=<Y, Y=<H, orez2(X,D,H,X2), orez2(Z,D,H,Z2).
%rusim koren a cely pravy podstrom
orez2(t(X,Y,_),D,H,X2) :- Y > H, orez2(X,D,H,X2).

%rusim koren a cely levy podstrom
orez2(t(_,Y,Z),D,H,Z2) :- Y < D, orez2(Z,D,H,Z2).







