:- include('KB.pl').
ids(X,L):-
(call_with_depth_limit(myPredicate(X),L,R), number(R));
(call_with_depth_limit(myPredicate(X),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X,L1)).

bottle_layers(N, Color1, Color2, s0) :-
    (N = 1, bottle1(Color1, Color2));
    (N = 2, bottle2(Color1, Color2));
    (N = 3, bottle3(Color1, Color2)).

bottle_layers(N, Color1, Color2, result(pour(From, To), PreviousState)) :-
    bottle_layers(From, TopFrom, BottomFrom, PreviousState),
    bottle_layers(To, TopTo, BottomTo, PreviousState),
    update_bottles(From, To, TopFrom, BottomFrom, TopTo, BottomTo, NewTopFrom, NewBottomFrom, NewTopTo, NewBottomTo),
    (N = From, Color1 = NewTopFrom, Color2 = NewBottomFrom);
    (N = To, Color1 = NewTopTo, Color2 = NewBottomTo);
    (N \= From, N \= To, bottle_layers(N, Color1, Color2, PreviousState)).

update_bottles(From, To, TopFrom, BottomFrom, TopTo, BottomTo, NewTopFrom, NewBottomFrom, NewTopTo, NewBottomTo) :-
    (TopTo = e, NewTopTo = TopFrom, NewBottomTo = BottomTo, NewTopFrom = e, NewBottomFrom = BottomFrom);
    (BottomTo = e, NewBottomTo = TopFrom, NewTopTo = TopTo, NewTopFrom = e, NewBottomFrom = BottomFrom).

add_action(Action, PreviousSituation, NewSituation) :-
    pour(A,B) = Action,
    can_pour(A, B, PreviousSituation),
    NewSituation = result(Action, PreviousSituation).

can_pour(From, To, S) :-
    bottle_layers(From, Top1, Bottom1, S),
    bottle_layers(To, Top2, Bottom2, S),
    Top2 = e,
    (  
        (Top1 \= e , (  (Bottom2 =e)     ;    (Bottom2 \=e , Top1 = Bottom2)   )   )  
         ;  
        (Bottom1 \= e , (  (Bottom2 =e)     ;    (Bottom2 \=e , Bottom1 = Bottom2)   )   ) 
    ),
    !.

% add_action(pour(1,3), [[b,r],[b,r],[e,e]], S).

% S = result(pour(1,3), S0).
