:- module(validations,
          [ pilled/2,
            validate_grasshoper_move/4,
            not_blocked/5,
            not_blocked_two_level_bridge/5
          ]).

:- use_module(predicates, [tile/5]).

pilled(X, Y) :-
    findall(L,
            tile(_, _, X, Y, L),
            Bag), %Check if it's pilled
    max_list(Bag, MaxLevel),
    MaxLevel>=1. %Bag has at least two elements

validate_grasshoper_move(X1, Y1, X2, Y2) :-
    (   exists_line(X1, Y1, X2, Y2, -1, 0), !
    ;   exists_line(X1, Y1, X2, Y2, 0, -1), !
    ;   exists_line(X1, Y1, X2, Y2, 1, -1), !
    ;   exists_line(X1, Y1, X2, Y2, 1, 0), !
    ;   exists_line(X1, Y1, X2, Y2, 0, 1), !
    ;   exists_line(X1, Y1, X2, Y2, -1, 1), !
    ).

exists_line(X1, Y1, X2, Y2, _, _) :-
    X1=:=X2,
    Y1=:=Y2.

exists_line(X1, Y1, X2, Y2, A1, A2) :-
    tile(_, _, X1, Y1, _),
    NX1 is X1+A1,
    NY2 is Y1+A2,
    exists_line(NX1, NY2, X2, Y2, A1, A2).

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X-1,
    Y2 is Y,!,
    X3 is X-1,
    Y3 is Y+1,
    X4 is X,
    Y4 is Y-1.

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X,
    Y2 is Y-1,!,
    X3 is X-1,
    Y3 is Y,
    X4 is X+1,
    Y4 is Y-1.

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X+1,
    Y2 is Y-1,!,
    X3 is X,
    Y3 is Y-1,
    X4 is X+1,
    Y4 is Y.

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X+1,
    Y2 is Y,!,
    X3 is X+1,
    Y3 is Y-1,
    X4 is X,
    Y4 is Y+1.

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X,
    Y2 is Y+1,!,
    X3 is X+1,
    Y3 is Y,
    X4 is X-1,
    Y4 is Y+1.

bridges_per_direction((X, Y), (X2, Y2), (X3, Y3), (X4, Y4)):-
    X2 is X-1,
    Y2 is Y+1,!,
    X3 is X,
    Y3 is Y+1,
    X4 is X-1,
    Y4 is Y.
        
not_blocked(X1, Y1, X2, Y2, Bridges):- 
    not(bagof([(X3,Y3), (X4,Y4)], 
            (bridges_per_direction((X1,Y1), (X2,Y2), (X3,Y3), (X4,Y4)),
            tile(_, _, X3, Y3, _), tile(_, _, X4, Y4, _)),
            Bridges)).


not_blocked_two_level_bridge(X1, Y1, X2, Y2, Bridges):- 
    not(bagof([(X3,Y3), (X4,Y4)], 
            (bridges_per_direction((X1,Y1), (X2,Y2), (X3,Y3), (X4,Y4)),
            tile(_, _, X3, Y3, 1), tile(_, _, X4, Y4, 1)),
            Bridges)).