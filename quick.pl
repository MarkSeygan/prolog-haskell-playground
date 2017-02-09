% Napsat prevod seznamu na rozdilovy seznam je snadne (v linearnim case).

% narozdil(+S,-RS) :- prevadi seznam S na rozdilovy seznam RS
narozdil([],X-X).
narozdil([H|T],[H|S]-X):-narozdil(T,S-X).

% Prevod z rozdiloveho seznamu na obycejny seznam je jeste jednodussi a dokonce
% v konstantnim case).

% naobyc(+RS,S):- prevadi rozdilovy seznam RS na obycejny seznam S
naobyc(X-[],X).

% Kdyz nyni unifikujeme X s libovolnym 
% seznamem (napr. [d,e]), dostaneme [a,b,c,d,e]-[d,e] (v jednom kroku) a vidite,
% ze mame spojeni seznamu (+ nejaky ocasek navic). Jeste zajimavejsi situace
% by nastanala, kdybychom spojili nas puvodni senzam s rozdilovym seznamem, 
% napr. s [d,e|Y]-Y a dostavali jsme [a,b,c,d,e|Y]-Y a tedy zase rozdilovy senzam.
% Takove spojeni se ale da opet napsat velmi snadno

%spojeni(+A,+B,-C) :- do rozd. seznamu C ulozi spojeni rozdilovych seznamu A a B
%spojeni(A-B,B-B1,A-B1).


% rozdel(+S, +Pivot, -Mensi, -Vetsi) :- rozdeli S tak, ze v seznamu Mensi jsou
% 	prvky mensi nebo rovny Pivotu a v seznamu vetai jsou prvky vetai nez Pivot

rozdel([], _, [], []).
rozdel([H|T], Pivot, [H|Mensi], Vetsi):-H=<Pivot,rozdel(T,Pivot,Mensi,Vetsi).
rozdel([H|T], Pivot, Mensi, [H|Vetsi]):-H>Pivot,rozdel(T,Pivot,Mensi,Vetsi).

% spojeni(?X, ?Y, ?Z) :- do seznamu Z ulozi spojeni seznamu X a Y
spojeni([],X,X).
spojeni([H|T], X, [H|S]):-spojeni(T,X,S).

% A ted uz mame vsechno a muzeme klidne napsat quicksort. T\=[] v posledni
% klauzuli zajistuje, ze Proog nebude vracet stejne odpovedi vicekrat, ale 
% neni ho tam nutne mit.

% qsort(+X,-Y) :- do seznamu Y ulozi setrideny seznam X
qsort([],[]).
qsort([A],[A]).
qsort([H|T],X):- T\=[],rozdel(T,H,Mensi,Vetsi),qsort(Mensi,X1),qsort(Vetsi,X2),
				append(X1,[H|X2],X).


