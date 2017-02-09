% my_flatten(L1,L2) :- the list L2 is obtained from the list L1 by
%    flattening; i.e. if an element of L1 is a list then it is replaced
%    by its elements, recursively. 

% Note: flatten(+List1, -List2) is a predefined predicate

my_flatten(X,[X]) :- \+ is_list(X).
my_flatten([],[]).
my_flatten([X|Xs],Zs) :- my_flatten(X,Y), my_flatten(Xs,Ys), append(Y,Ys,Zs).

%values(+K,+X,-H) vrátí v H seznam hodnot kterých nabývá klíč K v seznamu X.

values(K,X,H) :- my_flatten(X,Y), valuesF(K,Y,H).

valuesF(_,[],[]).
valuesF(K, [K-B|Xs], [B|H])  :- values(K, Xs, H).
valuesF(K, [X-_|Xs], H)      :- K \= X, values(K, Xs, H).

% poridit si seznam klicu (na jeho zaklade doplnit objekty o undefined hodnoty), na ten pak rekurzivne volat values

% S seznam dvojic klic-hodnota, K seznam klicu
seznamklicu([],[]).
seznamklicu([K-_],[K]).

seznamklicu([A-_|S],[A|K])  :- seznamklicu(S,K), \+member(A,K).
seznamklicu([A-_|S],K)      :- seznamklicu(S,K), member(A,K).
