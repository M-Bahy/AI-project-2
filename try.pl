:- include('KB.pl').
ids(X,L):-
(call_with_depth_limit(myPredicate(X),L,R), number(R));
(call_with_depth_limit(myPredicate(X),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X,L1)).

bottle_layers(1, Color1, Color2, S):-
    bottle1(Color1, Color2).

bottle_layers(2, Color1, Color2, S):-
    bottle2(Color1, Color2).

bottle_layers(3, Color1, Color2, S):-
    bottle3(Color1, Color2).
    

add_action(Action, PreviousSituation, NewSituation) :-
    pour(A,B) = Action,
    can_pour(A, B, PreviousSituation),
    NewSituation = result(Action, PreviousSituation).

can_pour(From, To, S) :-
    % make sure they are the same color 
    bottle_layers(From, Top1, Bottom1, S),
    bottle_layers(To, Top2, Bottom2, S),
    Bottom1 \= e,
    Top2 = e,
    !.

% add_action(pour(1,3), [[b,r],[b,r],[e,e]], S).

% S = result(pour(1,3), S0).
