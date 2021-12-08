:- module(predicates,
          [ position_available/2,
            add_tile/6,
            is_adjacent/4,
            remove_tile/5,
            check_adjacents/3
          ]).


:- (dynamic tile/5).

is_adjacent(X1, Y1, X2, Y2) :-
    (   X2=:=X1-1,
        Y2=:=Y1, !
    ;   X2=:=X1-1,
        Y2=:=Y1+1, !
    ;   X2=:=X1,
        Y2=:=Y1-1, !
    ;   X2=:=X1,
        Y2=:=Y1+1, !
    ;   X2=:=X1+1,
        Y2=:=Y1-1, !
    ;   X2=:=X1+1,
        Y2=:=Y1
    ).

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

same_colour(_Colour, []).

same_colour(Colour, [Ci|R]):-
    nonvar(Ci), !,
    Colour == Ci,
    same_colour(Colour, R).

same_colour(Colour, [Ci|R]):-
    var(Ci),
    same_colour(Colour, R).

exist_adjacent([(X1, Y1),  (X2, Y2),  (X3, Y3),  (X4, Y4),  (X5, Y5),  (X6, Y6)], [C1, C2, C3, C4, C5, C6]) :-
    (   tile(_, C1, X1, Y1, _),
        write(C1),!
    ;   tile(_, C2, X2, Y2, _),write(C2),!
    ;   tile(_, C3, X3, Y3, _),write(C3),!
    ;   tile(_, C4, X4, Y4, _),write(C4),!
    ;   tile(_, C5, X5, Y5, _),write(C5),!
    ;   tile(_, C6, X6, Y6, _),write(C6),!
    ).

check_adjacents(X, Y, AdjsColour) :-
    adjacents(X, Y, Adjacents),
    exist_adjacent(Adjacents, AdjsColour).

validate_insertion(Colour, X, Y, 0):-
    position_available(X, Y),
    check_adjacents(X, Y, AdjsColour),
    same_colour(Colour, AdjsColour).

validate_insertion(_Colour, X, Y, 1):-
    position_available(X, Y).


validate_insertion(_Colour, X, Y, 2):-
    position_available( X, Y),
    check_adjacents(X, Y, _).

add_tile(Bug, Colour, X, Y, Level, Move) :-
    validate_insertion( Colour, X, Y, Move),!,
    assert(tile(Bug, Colour, X, Y, Level)).

remove_tile(Bug, Colour, X, Y, Level):-
    tile(Bug, Colour, X, Y, Level),
    retract(tile(Bug, Colour, X, Y, Level)).
