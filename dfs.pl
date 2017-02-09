% dfs(X,Y,Cesta):- Cesta je seznam vrcholů na cestě z X do Y.

dfs(X,Y,Cesta):- dfs(X,Y,[X],C), reverse(C,Cesta).

dfs(X,X,C,C).
dfs(X,Z,Nav,C):- hrana(X,Y), \+ member(Y,Nav), dfs(Y,Z,[Y|Nav],C).
