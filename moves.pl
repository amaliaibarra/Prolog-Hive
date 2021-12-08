:- use_module(predicates,
              [ tile/5,
                is_adjacent/4,
                position_available/2,
                check_adjacents_except/3,
                check_adjacents/3,
                add_tile/6,
                remove_tile/5
              ]).
:- use_module(validations, [validate_grasshoper_move/4]).

move(X1, Y1, X2, Y2) :-
    position_available(X2, Y2),
    tile(Bug, Colour, X1, Y1, Level),
    move_tile(Bug,
              Colour,
              Level,
              X1,
              Y1,
              X2,
              Y2).



move_tile(grasshopper, Colour, Level, X1, Y1, X2, Y2) :-
    validate_grasshoper_move(X1, Y1, X2, Y2),
    remove_tile(grasshopper, Colour, X1, Y1, Level),
    assert(tile(grasshopper, Colour, X2, Y2, Level)).

move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
    check_adjacents_except(X2, Y2,  (X1, Y1)),
    remove_tile(queen, Colour, X1, Y1, Level),
    assert(tile(queen, Colour, X2, Y2, Level)).


add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).

