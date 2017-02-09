% Uloha: rotace v konstantim case
%% ?- rotace([1,2,3|K]-K, P-K).
%% K = [1|K],
%% P = [2, 3, 1|K].

% narozdil(+S,-RS) :- prevadi seznam S na rozdilovy seznam RS
narozdil([],X-X).
narozdil([H|T],[H|S]-X):-narozdil(T,S-X).

% naobyc(+RS,S):- prevadi rozdilovy seznam RS na obycejny seznam S
naobyc(X-[],X).

%spojeni(+A,+B,-C) :- do rozdil. seznamu C ulozi spojeni rozdil. seznamu A a B
spojeni(A-B,B-B1,A-B1).

%------------------------------------------------------------------------------%
%var(Term) is True if Term currently is a free variable.
rotace(      X-X, X-X) :- var(X). 
rotace([X|Xs]-Y, Xs-Z) :- var(Y), Y = [X | Z].

