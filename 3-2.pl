%Mame dany nedokonaly BVS, mame vypsat dvojice vrcholu, ktere porusuji podminky BVS. Kazdy vrchol a list ma nejaku hodnotu - unikatne cele cislo.

%my_max najde nejvetsi hodnotu M ve strome T
my_max(nil,0).
my_max(t(X,Y,Z),M) :- my_max(X,MX), my_max(Z,MZ), MM is max(MX,MZ), M is max(MM,Y).

%najde nejmensi hodnotu ve strome
my_min(nil,1000000).
my_min(t(X,Y,Z),M) :- my_min(X,MX), my_min(Z,MZ), MM is min(MX,MZ), M is min(MM,Y).

%nedok(+T,-S) najde dvojice vrcholu (hrany), ktere porusuji podminky BVS.
nedok(nil,[]).
nedok(t(X,Y,Z),S) :-    my_max(X,MX), my_min(Z,MZ), MX<Y, Y<MZ, 
                        nedok(X,S1), nedok(Z,S2), append(S1,S2,S).

nedok(t(X,Y,Z),[MX-Y|Sx]) :-    my_max(X,MX), MX>Y, nedok(X,S1), nedok(Z,S2),
                                append(S1,S2,Sx).

nedok(t(X,Y,Z),[MZ-Y|Sx]) :-    my_min(Z,MZ), MZ<Y, nedok(X,S1), nedok(Z,S2),
                                append(S1,S2,Sx).
