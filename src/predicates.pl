:- module(predicates,
          [ tile/5,
            is_adjacent/4,
            position_available/2,
            check_adjacents_except/3,
            check_adjacents/3,
            add_tile/5,
            remove_tile/5
          ]).
:- use_module(moves,
            [ get_top_bug/5]).

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

exist_adjacent([(X,Y)|_]):-
    tile(_, _, X, Y, _), !.

exist_adjacent([(_X,_Y)|R]):-
    exist_adjacent(R).

check_adjacents_except(X, Y, Omit) :-
    adjacents(X, Y, Adjacents),
    delete( Adjacents, Omit, AdjacentsToAnalize),
    exist_adjacent(AdjacentsToAnalize).

check_adjacents(X, Y, Adjacents) :-
    adjacents(X, Y, Adjacents),
    exist_adjacent(Adjacents).

same_colour(_Colour, []).

same_colour(Colour, [(X, Y)|R]):-
    tile(_, Colour, X, Y, _ ), !,
    same_colour(Colour, R).

same_colour(Colour, [(X, Y)|R]):-
    not(tile(_, _C, X, Y, _ )),
    same_colour(Colour, R).

validate_insertion(Colour, X, Y, 0):-
    position_available(X, Y),
    check_adjacents(X, Y, Adjs),
    same_colour(Colour, Adjs).

validate_insertion(_Colour, X, Y, 1):-
    position_available(X, Y).

validate_insertion(_Colour, X, Y, 2):-
    position_available( X, Y),
    check_adjacents(X, Y,_).

add_tile(Bug, Colour, X, Y, Move) :-
    validate_insertion( Colour, X, Y, Move),!,
    assert(tile(Bug, Colour, X, Y, 0)).

remove_tile(Bug, Colour, X, Y, Level):-
    get_top_bug(Bug, Colour, X, Y, Level),
    retract(tile(Bug, Colour, X, Y, Level)).
