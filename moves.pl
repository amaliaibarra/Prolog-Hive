:- use_module(predicates, [tile/5, is_adjacent/4, position_available/2, check_adjacents_except/3, check_adjacents/3, add_tile/6, remove_tile/5]).

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

move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
    check_adjacents_except(X2, Y2, (X1,Y1)),
    write("exits adjacent to new postion"),
    remove_tile(queen, Colour, X1, Y1, Level),
    write("tile removed from first position"),
    assert(tile(queen, Colour, X2, Y2, Level)),
    write("tile added to last position").

add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).

