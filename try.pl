:- include('KB.pl').
ids(S,L):-
(call_with_depth_limit(goal(S),L,R), number(R));
(call_with_depth_limit(goal(S),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(S,L1)).


current_bottle_colors(Top1,Bottom1,Top2,Bottom2,Top3,Bottom3,s0):-
    bottle1(Top1,Bottom1),
    bottle2(Top2,Bottom2),
    bottle3(Top3,Bottom3),!.


current_bottle_colors(Top1, Bottom1, Top2, Bottom2, Top3, Bottom3, result(Action, S)) :-
    (
        % Case where the action fails
        (current_bottle_colors(Top1, Bottom1, Top2, Bottom2, Top3, Bottom3, S),
        pour(A, B) = Action,
        \+ can_pour(Top1, Bottom1,Top2,Bottom2),
        \+ can_pour(Top2, Bottom2,Top1,Bottom1),
        \+ can_pour(Top1, Bottom1,Top3,Bottom3),
        \+ can_pour(Top3, Bottom3,Top1,Bottom1),
        \+ can_pour(Top2, Bottom2,Top3,Bottom3),
        \+ can_pour(Top3, Bottom3,Top2,Bottom2));    
        % Case where the action succeeds
        (current_bottle_colors(Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, S),
        pour(A, B) = Action,
        (
        
        (
            (A = 1 -> FromTop = Top1Prev, FromBottom = Bottom1Prev);
            (A = 2 -> FromTop = Top2Prev, FromBottom = Bottom2Prev);
            (A = 3 -> FromTop = Top3Prev, FromBottom = Bottom3Prev)
        ),
        (
            (B = 1 -> ToTop = Top1Prev, ToBottom = Bottom1Prev);
            (B = 2 -> ToTop = Top2Prev, ToBottom = Bottom2Prev);
            (B = 3 -> ToTop = Top3Prev, ToBottom = Bottom3Prev)
        )
	    ),
	    can_pour(FromTop, FromBottom, ToTop, ToBottom),
        update_bottle_colors(A, B, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3))
    ).



update_bottle_colors(1, 2, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top1Prev \= e -> Top1 = e, Bottom1 = Bottom1Prev, Top3= Top3Prev, Bottom3=Bottom3Prev,
    ((Bottom2Prev \= e -> Top2 = Top1Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Top1Prev)));

    (Top1Prev = e -> Top1 = e, Bottom1 = e, Top3 = Top3Prev, Bottom3 = Bottom3Prev,
    
    ((Bottom2Prev \= e -> Top2 = Bottom1Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Bottom1Prev)));
    (Bottom1Prev = e -> Top1 = e, Bottom1 = e, Top3 = Top3Prev, Bottom3 = Bottom3Prev).


update_bottle_colors(1, 3, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top1Prev \= e -> Top1 = e, Bottom1 = Bottom1Prev, Top2= Top2Prev, Bottom2=Bottom2Prev,
    ((Bottom3Prev \= e -> Top3 = Top1Prev, Bottom3 = Bottom3Prev);
    (Bottom3Prev = e -> Top3 =e, Bottom3 = Top1Prev)));

    (Top1Prev = e -> Top1 = e, Bottom1 = e, Top2 = Top2Prev, Bottom2 = Bottom2Prev,
    
    ((Bottom3Prev \= e -> Top3 = Bottom1Prev, Bottom3 = Bottom3Prev);
    (Bottom3Prev = e -> Top3 =e, Bottom3 = Bottom1Prev)));
    (Bottom1Prev = e -> Top1 = e, Bottom1 = e, Top2 = Top2Prev, Bottom2 = Bottom2Prev).

update_bottle_colors(2, 1, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top2Prev \= e -> Top2 = e, Bottom2 = Bottom2Prev, Top3= Top3Prev, Bottom3=Bottom3Prev,
    ((Bottom1Prev \= e -> Top1 = Top2Prev, Bottom1 = Bottom1Prev);
    (Bottom1Prev = e -> Top1 =e, Bottom1 = Top2Prev)));

    (Top2Prev = e -> Top2 = e, Bottom2 = e, Top3 = Top3Prev, Bottom3 = Bottom3Prev,
    
    ((Bottom1Prev \= e -> Top1 = Bottom2Prev, Bottom1 = Bottom1Prev);
    (Bottom1Prev = e -> Top1 =e, Bottom1 = Bottom2Prev)));
    (Bottom2Prev = e -> Top2 = e, Bottom2 = e, Top3 = Top3Prev, Bottom3 = Bottom3Prev).


update_bottle_colors(3, 1, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top3Prev \= e -> Top3 = e, Bottom3 = Bottom3Prev, Top2= Top2Prev, Bottom2=Bottom2Prev,
    ((Bottom1Prev \= e -> Top1 = Top3Prev, Bottom1 = Bottom1Prev);
    (Bottom1Prev = e -> Top1 =e, Bottom1 = Top3Prev)));

    (Top3Prev = e -> Top3 = e, Bottom3 = e, Top2 = Top2Prev, Bottom2 = Bottom2Prev,
    
    ((Bottom1Prev \= e -> Top1 = Bottom3Prev, Bottom1 = Bottom1Prev);
    (Bottom1Prev = e -> Top1 =e, Bottom1 = Bottom3Prev)));
    (Bottom3Prev = e -> Top3 = e, Bottom3 = e, Top2 = Top2Prev, Bottom2 = Bottom2Prev).

update_bottle_colors(3, 2, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top3Prev \= e -> Top3 = e, Bottom3 = Bottom3Prev, Top1= Top1Prev, Bottom1=Bottom1Prev,
    ((Bottom2Prev \= e -> Top2 = Top3Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Top3Prev)));

    (Top3Prev = e -> Top3 = e, Bottom3 = e, Top1 = Top1Prev, Bottom1 = Bottom1Prev,
    
    ((Bottom2Prev \= e -> Top2 = Bottom3Prev, Bottom2 = Bottom2Prev);
    (Bottom2Prev = e -> Top2 =e, Bottom2 = Bottom3Prev)));
    (Bottom3Prev = e -> Top3 = e, Bottom3 = e, Top1 = Top1Prev, Bottom1 = Bottom1Prev).

update_bottle_colors(2, 3, Top1Prev, Bottom1Prev, Top2Prev, Bottom2Prev, Top3Prev, Bottom3Prev, Top1, Bottom1, Top2, Bottom2, Top3, Bottom3) :-

    (Top2Prev \= e -> Top2 = e, Bottom2 = Bottom2Prev, Top1= Top1Prev, Bottom1=Bottom1Prev,
    ((Bottom3Prev \= e -> Top3 = Top2Prev, Bottom3 = Bottom3Prev);
    (Bottom3Prev = e -> Top3 =e, Bottom3 = Top2Prev)));

    (Top2Prev = e -> Top2 = e, Bottom2 = e, Top1 = Top1Prev, Bottom1 = Bottom1Prev,
    
    ((Bottom3Prev \= e -> Top3 = Bottom2Prev, Bottom3 = Bottom3Prev);
    (Bottom3Prev = e -> Top3 =e, Bottom3 = Bottom2Prev)));
    (Bottom2Prev = e -> Top2 = e, Bottom2 = e, Top1 = Top1Prev, Bottom1 = Bottom1Prev).


can_pour(Top1,Bottom1,Top2,Bottom2) :-

     Top2 = e,
     (  
         (Top1 \= e , (  (Bottom2 == e)     ;    (Bottom2 \=e , Top1 == Bottom2)   )   )  
       ;  
        (Top1 ==e,Bottom1 \= e , (  (Bottom2 == e)     ;    (Bottom2 \=e , Bottom1 == Bottom2)   )   ) 
    ),
    !.


goal(S) :-
    current_bottle_colors(Top1, Bottom1, Top2, Bottom2, Top3, Bottom3, S),
    isGoal(Top1,Bottom1),
    isGoal(Top2,Bottom2),
    isGoal(Top3,Bottom3),!.
    % remove the cut to get all possible solutions

isGoal(Top,Bottom):-
    write('ISGOALLL'),nl,
    (Top == Bottom).








