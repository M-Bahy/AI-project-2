:- include('KB.pl').

% Define the bottle layers based on the current situation
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
