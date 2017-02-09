% Zkusme si v Prologu praci s binarnimi stromy. Nejjednodussi jsou asi pruchody
% stromem. Zacneme tedy s prevedenim stromu na seznam v inorder poradi.

% inorderList(+T,-S) :- prevadi strom T na seznam S v inorder poradi
% L -> A -> P
inorderList(nil,[]).
inorderList(t(X,Y,Z),S)     :-
                        inorderList(X,S1),inorderList(Z,S2),append(S1,[Y|S2],S).

% preorder(+T,-S) :- prevadi strom T na seznam S v preorder poradi
% A -> L -> P
preorderList(nil,[]).
preorderList(t(X,Y,Z),[Y|S]):-
                        preorderList(X,S1),preorderList(Z,S2),append(S1,S2,S).

% postorder(+T,-S) :- prevadi strom T na seznam S v postorder poradi
% L -> P -> A
postorderList(nil,[]).
postorderList(t(X,Y,Z), S):-
                        postorderList(X,S1),postorderList(Z,S2),append(S1,S2,S3),append(S3,[Y],S).
                        
% pridejBST(+T,+X,+S) :- prida prvek X do stromu T a vrati strom S
pridejBST(nil, X, t(nil,X,nil)).
%pridejBST(t(L,X,P),X,t(L,X,P)). % odkomentovani radky zpusobi, ze se prvky nemohou opakovat 	
pridejBST(t(L,U,P),X,t(T,U,P)):-X=<U,pridejBST(L,X,T). % tohle je pak treba zmenit na X<U
pridejBST(t(L,U,P),X,t(L,U,T)):-X>U,pridejBST(P,X,T).

% Kdyz uz umime pridavat do BST, tak samozrejme umime i postavit BST ze seznamu.

% seznamNaBST(+S,-T) :- vytvori BST T ze seznamu S
seznamNaBST(S,T):-seznamNaBST(S,nil,T).
seznamNaBST([],T,T).
seznamNaBST([H|Hs],T1,T):-pridejBST(T1,H,T2),seznamNaBST(Hs,T2,T).

% Predikat, ktery v seznamu nahradi vsechny vyskyty nejake promenne jinou promennou.

% substituce(+S,+A,+B,-T) :- v seznamu S nahrad vsechny vyskyty A za B a vrat T

substituce([],_,_,[]).
substituce([A|T],A,B,[B|T1]):- !, substituce(T,A,B,T1).
substituce([H|T],A,B,[H|T1]):- substituce(T,A,B,T1).

substituce2([],_,_,[]).
substituce2([A|T],A,B,[B|T1]):-substituce2(T,A,B,T1).
substituce2([H|T],A,B,[H|T1]):-H\=A,substituce2(T,A,B,T1).
