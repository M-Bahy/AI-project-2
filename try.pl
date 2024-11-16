:- include('KB.pl').
ids(X,L):-
(call_with_depth_limit(myPredicate(X),L,R), number(R));
(call_with_depth_limit(myPredicate(X),L,R), R=depth_limit_exceeded,
L1 is L+1, ids(X,L1)).

bottle_layers(N, Color1, Color2, S) :-
    (
        % Base case: Initial state (s0)
        S = s0, 
        (
            (N = 1, bottle1(Color1, Color2));
            (N = 2, bottle2(Color1, Color2));
            (N = 3, bottle3(Color1, Color2))
        )
    );
    (
        % Recursive case: Handle the result of pouring
        S = result(pour(From, To), PreviousSituation),
        bottle_layers(1, Top1, Bottom1, PreviousSituation),
        bottle_layers(2, Top2, Bottom2, PreviousSituation),
        bottle_layers(3, Top3, Bottom3, PreviousSituation),

        % Update the bottles based on the pour action
        (
            From = 1, To = 3 -> 
                (Top1 \= e -> Updated1 = (e, Bottom1), Updated3 = (Bottom3, Top1); 
                 Bottom1 \= e -> Updated1 = (Top1, e), Updated3 = (Bottom3, Bottom1)),
                Updated2 = (Top2, Bottom2);
            % Add other cases if necessary
            fail
        ),

        % Assign updated layers based on the bottle index
        (
            N = 1 -> Updated1 = (Color1, Color2);
            N = 2 -> Updated2 = (Color1, Color2);
            N = 3 -> Updated3 = (Color1, Color2)
        )
    ).


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
