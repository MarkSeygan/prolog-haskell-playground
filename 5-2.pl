% Najde vsechny nezavisle mnoziny v grafu obsahujici dany vrchol.
% reprezentace dat - vertex(V), edge(U,V)

% edgeG(+A,+B) - splneno, pokud hrana A-B v G
edgeG(A,B):- edge(A,B) ; edge(B,A).

% noedgeG(+A,+B) - splneno neni-li hrana A-B v G
noedge(A,B):- \+ edgeG(A,B).

% is_vertex(+Vs,+V) - splneno, pokud Vs a take [V|Vs] nezavisle mnoziny
is_vertex([],_).
is_vertex([Head|Tail],V):- % prvek lze pridat do nezavisle mnoziny
                        noedge(Head,V),     % pokud neexistuje hrana s hlavou
                        is_vertex(Tail,V).  % a neexistuje hrana s prvkem v tele


% insetG(-IS) - IS je nezavisla mnozina G
insetG(IS):- vertex(V),     % vyber 1. vrchol do nezavisle mnoziny
            insetG([V],IS). % utvor nezavislou mnozinu obsahujici tento vrchol

% insetG(+Vs,-IS) nezavisla mnozina obsahujici vrcholy Vs
insetG(Vs,IS):- vertex(U),          % vyber vrchol
                \+ member(U,Vs),    % ktery jeste neni ve Vs
                is_vertex(Vs,U),    % pokud lze pridat
                insetG([U|Vs],IS).  % rekurze pro vetsi mnozinu

insetG(Vs,Vs). % jiz neslo pridat -> napln vystupni IS

% testovaci data
vertex(a).
vertex(b).
vertex(c).
vertex(d).
vertex(e).
vertex(f).
vertex(g).
vertex(h).
vertex(i).

edge(a,b).
edge(a,c).
edge(a,f).
edge(b,g).
edge(c,d).
edge(c,e).
edge(c,g).
edge(d,e).
edge(h,i).

% testy
is_t1:- insetG(W),write(W).
is_t2:- insetG([a],IS), write(IS).
