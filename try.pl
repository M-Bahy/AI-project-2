:- include('KB.pl').
ids(X,L):-
(call_with_depth_limit(myPredicate(X),L,R), number(R));
(call_with_depth_limit(myPredicate(X),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X,L1)).



current_bottle_colors(Top1,Bottom1,Top2,Bottom2,Top3,Bottom3,s0):-
    bottle1(Top1,Bottom1),
    bottle2(Top2,Bottom2),
    bottle3(Top3,Bottom3).


current_bottle_colors(Top1, Bottom1, Top2, Bottom2, Top3, Bottom3, result(Action, S)) :-
    (
        % Case where the action fails
        (current_bottle_colors(Top1, Bottom1, Top2, Bottom2, Top3, Bottom3, S),
        pour(A, B) = Action,
        \+ can_pour(Top1, Bottom1,Top2,Bottom2,Top3,Bottom3, S))
    ;
        % Case where the action succeeds
        (current_bottle_colors(Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, S),
        pour(A, B) = Action,
        can_pour(Top1Prev,Bottom1Prev,Top2Prev,Bottom2Prev,Top3Prev,Bottom3Prev, S),
        update_bottle_colors(A, B, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3))
    ).

update_bottle_colors(1, 2, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-
    (Top1Prev \= e -> Top1 = e, Bottom1 = Bottom1Prev, Top3= Top3Prev, Bottom3=Bottom3Prev,
    (Bottom2Prev \= e -> Top2 = Top1Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Top1Prev));

    (Top1Prev = e -> Top1 = e, Bottom1 = e, Top3 = Top3Prev, Bottom3 = Bottom3Prev,
    
    (Bottom2Prev \= e -> Top2 = Top1Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Top1Prev)).



    


add_action(Action, PreviousSituation, NewSituation) :-
    pour(A,B) = Action,
    can_pour(A, B, PreviousSituation),
    NewSituation = result(Action, PreviousSituation).


can_pour(Top1,Bottom1,Top2,Bottom2,Top3,Bottom3, S) :-
   
    Top2 = e,
    (  
        (Top1 \= e , (  (Bottom2 =e)     ;    (Bottom2 \=e , Top1 = Bottom2)   )   )  
         ;  
        (Bottom1 \= e , (  (Bottom2 =e)     ;    (Bottom2 \=e , Bottom1 = Bottom2)   )   ) 
    ),
    !.





% add_action(pour(1,3), [[b,r],[b,r],[e,e]], S).

% S = result(pour(1,3), S0).
