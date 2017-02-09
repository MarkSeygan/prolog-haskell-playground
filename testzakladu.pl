%----1A----%

% setrid(+X,-Y) :- Y je seznam přirozených čísel X setříděný vzestupně

%setrizeny([]).
%setrizeny([_]).
%setrizeny([A,B|Xs]) :- A=<B, setrizeny([B|Xs]).

%setrid(X,X) :- setrizeny(X).


setrid([],[]).
setrid([X],[X]).
setrid(X,Y) :-  append(A,[H1,H2|B],X), H1 > H2, !,
                append(A,[H2,H1|B],X1), setrid(X1,Y).
                
%----1B----%                

% Zelený řez
% Nemění deklarativní význam programu,jen odřezává neperspektivní větve výpočtu:
 
% max(X,Y,X) :- X >= Y, ! .
% max(X,Y,Y) :- X < Y.

% Červený řez
% Mění deklarativní význam programu:

% p :- a,b.     % p ⇐ (a∧b)∨c
% p :- c.

% p :- a,!,b.   % p ⇐ (a∧b)∨(¬a∧c)
% p :- c.

%----2----%

% Definujte predikát trans/2, který k zadanému vstupnímu stromu T sestrojí strom téhož tvaru jako T, v němž bylo ohodnocení každého vrcholu nahrazeno maximálním ohodnocením, které se v T vyskytuje.

%my_max najde nejvetsi hodnotu M ve strome T
my_max(nil,0).
my_max(t(X,Y,Z),M) :- my_max(X,MX), my_max(Z,MZ), MM is max(MX,MZ), M is max(MM,Y).

my_max2(nil,0,_,nil).
my_max2(t(X,Y,Z),M,G,t(X2,G,Z2)) :- my_max2(X,MX,G,X2), my_max2(Z,MZ,G,Z2), MM is max(MX,MZ), M is max(MM,Y).

trans(T1,T2) :- my_max2(T1,M,M,T2).


