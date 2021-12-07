:- (dynamic tile/5).

adjacents(X, Y, [(X1, Y1),  (X2, Y2),  (X3, Y3),  (X4, Y4),  (X5, Y5),  (X6, Y6)]) :-
    X1 is X-1,
    Y1 is Y,
    X2 is X-1,
    Y2 is Y+1,
    X3 is X,
    Y3 is Y-1,
    X4 is X,
    Y4 is Y+1,
    X5 is X+1,
    Y5 is Y-1,
    X6 is X+1,
    Y6 is Y.
    
position_available(X, Y) :-
    not(tile(_, _, X, Y, _)). 

same_colour(_Color, []).

same_colour(Color, [Ci|R]):-
    nonvar(Ci), !,
    Color == Ci,
    same_colour(Color, R).

same_colour(Color, [Ci|R]):-
    var(Ci),
    same_colour(Color, R).

exist_adjacent([(X1, Y1),  (X2, Y2),  (X3, Y3),  (X4, Y4),  (X5, Y5),  (X6, Y6)], [C1, C2, C3, C4, C5, C6]) :-
    (   tile(_, C1, X1, Y1, _),
        write(C1),!
    ;   tile(_, C2, X2, Y2, _),write(C2),!
    ;   tile(_, C3, X3, Y3, _),write(C3),!
    ;   tile(_, C4, X4, Y4, _),write(C4),!
    ;   tile(_, C5, X5, Y5, _),write(C5),!
    ;   tile(_, C6, X6, Y6, _),write(C6),!
    ).

check_adjacents(X, Y, AdjsColor) :-
    adjacents(X, Y, Adjacents),
    exist_adjacent(Adjacents, AdjsColor).

validate_insertion(Color, X, Y, 0):-
    position_available(X, Y),
    check_adjacents(X, Y, AdjsColor),
    same_colour(Color, AdjsColor).

validate_insertion(_Color, X, Y, 1):-
    position_available(X, Y).


validate_insertion(_Color, X, Y, 2):-
    position_available( X, Y),
    check_adjacents(X, Y, _).

add_tile(Bug, Color, X, Y, Level, Move) :-
    validate_insertion( Color, X, Y, Move),!,
    assert(tile(Bug, Color, X, Y, Level)).


