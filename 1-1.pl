% Definujte predikát tranverse(+Strom,-OhodnocenýStrom), který zkopíruje strukturu stromu Strom do OhodnocenýStrom s tím, že ke každému vrcholu přidá číslo N, které znamená pořadí v preOrder průchodu a číslo M, které znamená pořadí v postOrder průchodu. Ideálně jedním průchodem stromem.

% tranverse(t(t(nil,l,nil),v,t(nil,p,nil)),X).
% X = t(t(nil,l-2-1,nil),v-1-3,t(nil,p-3-2,nil))

% preorder(+T,-S,+N,-M) :- prevadi strom T na seznam S v preorder poradi, N je cislo od ktereho se zacina -1, M1 je cislo, kde skoncil
% A -> L -> P
preorderList(T,S) :- preorderList(T,S,0,_).
preorderList(nil,[],N,N).
preorderList(t(X,Y,Z),[Y-N1|S],N,M1) :- N1 is N+1, 
                                        preorderList(X,S1,N1,M),
                                        preorderList(Z,S2,M,M1),
                                        append(S1,S2,S).


% preorderTree(+T1,+N,-M,-T2)
preorderTree(T1,T2) :- preorderTree(T1,0,_,T2).

preorderTree(nil, N, N, nil).
preorderTree(t(X,Y,Z), N, M1, t(X2,Y-N1,Z2)) :- N1 is N+1, 
                                                preorderTree(X,N1,M,X2),
                                                preorderTree(Z,M,M1,Z2).

% postorder(+T,-S) :- prevadi strom T na seznam S v postorder poradi
% L -> P -> A
postorderList(T,S) :- postorderList(T,S,0,_).
postorderList(nil,[],N,N).
postorderList(t(X,Y,Z),S,N,M2) :-   postorderList(X,S1,N,M),
                                    postorderList(Z,S2,M,M1), 
                                    append(S1,S2,S3),
                                    M2 is M1+1,
                                    append(S3,[Y-M2],S).
%postorderTree(+T1,+N,-M,-T2)


postorderTree(T1,T2) :- postorderTree(T1, 0, _, T2).

postorderTree(nil, N, N, nil).
postorderTree(t(X,Y,Z), N, M2, t(X2,Y-M2,Z2)) :-    postorderTree(X, N, M, X2),
                                                    postorderTree(Z, M, M1, Z2),
                                                    M2 is M1+1.
                                        
                                        














                                               
