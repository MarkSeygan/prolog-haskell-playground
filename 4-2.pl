% Mame zadan graf ve smyslu vrcholu a jeho sousedu 
% (seznam prvku tvaru: oznaceni_vrcholu-[seznam oznaceni jeho sousedu]).
% Definujte predikat troj(+Graf,-Trojuhelniky), kde "Trojuhelniky" je seznam 
% vsech trojic vrcholu, ktere tvori v G trojuhelnik.

% Priklad:
% troj( [a-[b,c], b-[a,c,d], c-[a,b,d], d-[b,c]], T ).
% T = [[a, b, c], [b, c, d]].

%test na trojuhelnik [V1,V2,V3]
je_troj(V1-S1,V2-S2,V3-S3,[V1,V2,V3]) :- member(V2,S1), member(V3,S2), member(V1,S3).

% select(Elem, L1, L2) is true when List1,with Elem removed,results in List2.
         
trojuhelnik2(G,VV) :- select(V1,G,G1), select(V2,G1,G2), select(V3,G2,_),
            je_troj(V1,V2,V3,VV).
            
% mnozina takovych S, pro ktere plati trojuh(G,S) se ulozi do T
troj(G,T) :- setof(S, trojuhelnik2(G,S), T).
