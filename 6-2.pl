
% hranyrezu(+Graf,+V1,+V2,-HranyRezu)
hranyrezu([],_,_,[]).
%rez rika : nejsou splneny podminky jeden vrchol ve V1 a druhy ve V2 -> jdi dal
hranyrezu([X-Y-C|Es],V1,V2,[X-Y-C|Hs]) :-   ((member(X,V1), member(Y,V2));
                                            (member(Y,V1), member(X,V2))),
                                            !, hranyrezu(Es,V1,V2,Hs).
                                            
hranyrezu([_|Es],V1,V2,Hs) :- hranyrezu(Es,V1,V2,Hs).

%neboli
/*hranyrezu([X-Y-_|Es],V1,V2,Hs) :-   ((member(X,V1), member(Y,V1));
                                            (member(X,V2),member(Y,V2))),
                                            hranyrezu(Es,V1,V2,Hs).*/
                                            
%cenarezu(+HranyRezu,-Cena)
cenarezu([],0).
cenarezu([_-_-C1|Es],Cena) :- cenarezu(Es,C2), Cena is C1+C2.

%vymenvrchol(+V1,+V2,V,-VV1,-VV2) :- 
% odeber V z V1 nebo V2, dej ho do druhe skupiny,tyto skupinyvrat v VV1 a VV2.
vymenvrchol(V1,V2,V,VV1,[V|V2]) :- select(V,V1,VV1).
vymenvrchol(V1,V2,V,[V|V1],VV2) :- select(V,V2,VV2).

%zlepsirez(+Graf,+V1,+V2,-OutV)
zlepsirez(G,V1,V2,V) :- hranyrezu(G,V1,V2,HA), cenarezu(HA,CA),
                        vymenvrchol(V1,V2,V,VV1,VV2),
                        hranyrezu(G,VV1,VV2,HB), cenarezu(HB,CB),
                        CB > CA.
                        





















