/* Uzávorkování formulí výrokového počtu	

Pracujeme s formulemi výrokového počtu bez proměnných; mohou se v nich tedy vyskytovat jen	
• konstanty true a false,
• logické spojky konjunkce, disjunkce a negace,
• závorky.
Předpokládejme, že spojky nemají předem dány žádnou prioritu či asociativitu, korektní výraz s alespoň dvěma spojkami tedy musí být opatřen závorkami.	

Problém: Na vstupu je zadána posloupnost, v níž jsou pouze logické spojky a konstanty.
• zjistěte, zdali lze vstup opatřit závorkami tak, aby vznikla korektní formule výrokového počtu
• pokud ano, ověřte, zdali existuje nějaké uzávorkování, v němž je formule pravdivá
• pokud ano, jedno takové uzávorkování vypište % pomoci rezu
• pokud ne, oznamte, že takové uzávorkování neexistuje	

Poznámka: 	
• popište formát vstupu, s nímž bude váš predikát / funkce pracovat
• řešení typu hrubá síla se pokuste vylepšit nějakou heuristikou, která by vás přivedla rychleji k cíli	
*/

arit([N],N,N).
arit(L,N,Exp) :- append(S1,S2,L), S1\=[], S2\=[],
                 arit(S1,N1,V1),arit(S2,N2,V2),
                 member(Op,[+,-,*]), Value =.. [Op,N1,N2],
                 N is Value, Exp =.. [Op,V1,V2].
                 
%------------------------------------------------------------------------------%

:- op(200,  fy, non).
:- op(200, xfy, and).
:- op(200, xfy, or ).

% je_fvp(+F) :- F je správně utvořená formule výrokového počtu.
je_fvp(true).
je_fvp(false).
je_fvp(non A):- je_fvp(A).
je_fvp(A and B):- je_fvp(A),je_fvp(B).
je_fvp(A or B):- je_fvp(A),je_fvp(B).

% vyhodnoceni spojek
eval_non(true, false).
eval_non(false, true).

eval_and(true,true,true).
eval_and(true,false,false).
eval_and(false,_,false).

eval_or(true,_,true).
eval_or(true,false,true).
eval_or(false,false,false).

% eval(+F,-H) :- H je hodnota formule F
eval(true,true).
eval(false,false).

eval(non F,H) :- eval(F,HF), eval_non(HF,H).
eval(F and G,H) :- eval(F,HF), eval(G,HG), eval_and(HF,HG,H).
eval(F or G ,H) :- eval(F,HF), eval(G,HG), eval_or(HF, HG,H).

%formule(+ZadanaPosloupnost,+HodnotaFormule,-Uzavorkovani)
formule([V],V,V).

% and, or
formule(L,V,U) :-append(S1,[Op|S2],L), member(Op,[and,or]),
                 S1\=[], S2\=[], formule(S1,N1,V1),
                 formule(S2,N2,V2), Value =.. [Op,N1,N2], eval(Value,V),
                 U =.. [Op,V1,V2].
% negace               
formule(L,V,U) :-append([],[Op|S2],L), member(Op,[non]),
                 S2\=[], formule(S2,N2,V2), Value =.. [Op,N2], eval(Value,V),
                 U =.. [Op,V2].                 

