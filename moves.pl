:- (dynamic tile/5).

adjacents(X, Y, [(X-1, Y),  (X-1, Y+1),  (X, Y-1),  (X, Y+1),  (X+1, Y)]).
    
    X2 =:= X1-1,Y2 =:= Y1,!;
    X2 =:= X1-1,Y2 =:= Y1 + 1,!;
    X2 =:= X1  ,Y2 =:= Y1-1,!;
    X2 =:= X1  ,Y2 =:= Y1+1, !;
    X2 =:= X1+1,Y2 =:= Y1 -1,!;
    X2 =:= X1+1,Y2 =:= Y1.

move_queen([Id, Color, Bug, X1, Y1],X2,Y2, Board):-
    validate_queen_move(Board,X1,Y1,X2,Y2),
    remove([Id, Color, Bug, X1, Y1], New_Board),
    insert([Id, Color, Bug, X2, Y2], New_Board).
