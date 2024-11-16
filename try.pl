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
        bottle_layers(3, Top3, Bottom3, POTATO) ,
        latest_bottle_1 = (A,B) ,
        latest_bottle_2 = (C,D) ,
        latest_bottle_3 = (E,F) ,
        
        (
            (
                From = 1 ,
                To = 2 ,(
                (Top1 \= e ,
                 (  (Bottom2 =e , D = Top1 , A = e,(B=Bottom1,C=Top2,E=Top3,F=Bottom3))     ;    (Bottom2 \=e , C = Top1 , A = e,(B=Bottom1,D=Bottom2,E=Top3,F=Bottom3))   )   )
                 ;
                (Bottom1 \= e ,
                    (  (Bottom2 =e , D = Bottom1 , B = e,(A=Top1,C=Top2,E=Top3,F=Bottom3))     ;    (Bottom2 \=e , C = Bottom1 , B = e,(A=Top1,D=Bottom2,E=Top3,F=Bottom3))   )   )

                )
            )
            ;
            (
                From = 1 ,
                To = 3 ,(
                (Top1 \= e ,
                 (  (Bottom3 =e , F = Top1 , A = e,(B=Bottom1,C=Top2,D=Top3,E=Bottom2))     ;    (Bottom3 \=e , E = Top1 , A = e,(B=Bottom1,C=Top2,D=Top3,F=Bottom3))   )   )
                 ;
                (Bottom1 \= e ,
                    (  (Bottom3 =e , F = Bottom1 , B = e,(A=Top1,C=Top2,D=Top3,E=Bottom2))     ;    (Bottom3 \=e , E = Bottom1 , B = e,(A=Top1,C=Top2,D=Top3,F=Bottom3))   )   )

                )
            )
            ;
            (
                From = 2 ,
                To = 1 ,(
                (Top2 \= e ,
                 (  (Bottom1 =e , B = Top2 , C = e,(A=Top1,D=Bottom2,E=Top3,F=Bottom3))     ;    (Bottom1 \=e , A = Top2 , C = e,(B=Bottom1,D=Bottom2,E=Top3,F=Bottom3))   )   )
                 ;
                (Bottom2 \= e ,
                    (  (Bottom1 =e , B = Bottom2 , D = e,(A=Top1,C=Top2,E=Top3,F=Bottom3))     ;    (Bottom1 \=e , A = Bottom2 , D = e,(B=Bottom1,C=Top2,E=Top3,F=Bottom3))   )   )

                )
            )
            ;
            (
                From = 2 ,
                To = 3 ,(
                (Top2 \= e ,
                 (  (Bottom3 =e , F = Top2 , C = e,(A=Top1,B=Bottom1,D=Top3,E=Bottom2))     ;    (Bottom3 \=e , E = Top2 , C = e,(A=Top1,B=Bottom1,D=Top3,F=Bottom3))   )   )
                 ;
                (Bottom2 \= e ,
                    (  (Bottom3 =e , F = Bottom2 , D = e,(A=Top1,B=Bottom1,C=Top2,E=Bottom2))     ;    (Bottom3 \=e , E = Bottom2 , D = e,(A=Top1,B=Bottom1,C=Top2,F=Bottom3))   )   )

                )
            )
            ;
            (
                From = 3 ,
                To = 1 ,(
                (Top3 \= e ,
                 (  (Bottom1 =e , B = Top3 , C = e,(A=Top1,D=Top2,E=Bottom2,F=Bottom3))     ;    (Bottom1 \=e , A = Top3 , C = e,(B=Bottom1,D=Top2,E=Bottom2,F=Bottom3))   )   )
                 ;
                (Bottom3 \= e ,
                    (  (Bottom1 =e , B = Bottom3 , D = e,(A=Top1,C=Top2,E=Bottom2,F=Bottom3))     ;    (Bottom1 \=e , A = Bottom3 , D = e,(B=Bottom1,C=Top2,E=Bottom2,F=Bottom3))   )   )

                )
            )
            ;
            (
                From = 3 ,
                To = 2 ,(
                (Top3 \= e ,
                 (  (Bottom2 =e , D = Top3 , C = e,(A=Top1,B=Bottom1,E=Top2,F=Bottom3))     ;    (Bottom2 \=e , B = Top3 , C = e,(A=Top1,D=Bottom2,E=Top2,F=Bottom3))   )   )
                 ;
                (Bottom3 \= e ,
                    (  (Bottom2 =e , D = Bottom3 , E = e,(A=Top1,B=Bottom1,C=Top2,F=Bottom3))     ;    (Bottom2 \=e , B = Bottom3 , E = e,(A=Top1,C=Top2,D=Bottom2,F=Bottom3))   )   )

                )
            )

        ),
        (
            N = 1 -> Color1 = A, Color2 = B;
            N = 2 -> Color1 = C, Color2 = D;
            N = 3 -> Color1 = E, Color2 = F
        )
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
