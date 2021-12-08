:- use_module(predicates,
              [ position_available/2,
                add_tile/6,
                is_adjacent/4,
                remove_tile/5,
                check_adjacents/3
              ]).

% :- [predicates].
:- (dynamic tile/5).

move(X1, Y1, X2, Y2) :-
    position_available(X2, Y2),
    tile(Bug, Colour, X1, Y1, Level),
    write(["Bug", Bug, "Colour", Colour]),
    move_tile(Bug,
              Colour,
              Level,
              X1,
              Y1,
              X2,
              Y2).

% move_tile(ant,X1, Y1, X2, Y2). 
try_move(queen, Colour, Level, X1, Y1, X2, Y2) :-
    is_adjacent(X1, Y1, X2, Y2),
    write("Is Adjacent\n"),
    remove_tile(queen, Colour, Level, X1, Y1),
    write("Tile Removed\n"),
    check_adjacents(X2, Y2, _),
    write("Destination has adjacents\n"),
    add_tile(queen, Colour, X2, Y2, Level, 0).

move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
    try_move(queen,
             Colour,
             Level,
             X1,
             Y1,
             X2,
             Y2),
    remove_tile(queen, Colour, X1, Y1, Level),
    assert(tile(queen, Colour, X2, Y2, Level)).
    



add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).
