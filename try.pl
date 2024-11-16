:- include('KB.pl').
ids(X,L):-
(call_with_depth_limit(myPredicate(X),L,R), number(R));
(call_with_depth_limit(myPredicate(X),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X,L1)).

bottle_layers(N, Color1, Color2, S):-
    (
        S = s0 , 
        (
        (N =1 , bottle1(Color1, Color2));
        (N =2 , bottle2(Color1, Color2));
        (N =3 , bottle3(Color1, Color2))
        )
    )
    ;
    (
        S = result(pour(From,To), POTATO) , 
        bottle_layers(1, Top1, Bottom1, POTATO) , 
        bottle_layers(2, Top2, Bottom2, POTATO) , 
        bottle_layers(3, Top3, Bottom3, POTATO) 
    )
    .

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
