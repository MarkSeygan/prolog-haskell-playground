% ---Uloha-varianta A: rotace v linearnim case bez zadneho pomocneho predikatu.

rotace_A([X|Xs],Y) :- pridejk(X,Xs,Y).

% pridejk(+X, +S, -Z). Prida X na konec seznamu S a vysledek vraci v Z.
% Pracuje v linearnim case.

pridejk(X,[],[X]).
pridejk(X,[Y|Ys],[Y|Z]):- pridejk(X,Ys,Z).

% --- Uloha-varianta B: rotace v konstantnim case.

% ?- rotace([1,2,3|K]-K, P-K).
%  K = [1|K],
%  P = [2, 3, 1|K].

%var(Term) is True if Term currently is a free variable.
rotace_B(  X-X, X-X) :- var(X). 
rotace_B( [X|Xs]-Y, Xs-Z) :- var(Y), Y = [X|Z].
