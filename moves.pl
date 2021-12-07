:- import([board: position_available/2,
    add_tile/6]).

move(X1,Y1,X2,Y2):-
    position_available(X2,Y2),
    tile(Bug, Colour, X1, Y1, Level),
    move_tile(Bug,Colour,Level, X1, Y1, X2, Y2).

% move_tile(ant,X1, Y1, X2, Y2). 
move_tile(queen,Colour,Level, X1,Y1,X2,Y2):-
    try_move(),
    remove_tile(queen, Colour, Level,X1,Y1),
    add_tile(queen, Colour, Level,X2,Y2).
    

try_move(queen, Colour, Level, X1, Y1, X2, Y2):-
    is_adjacent(X1,Y1, X2,Y2),
    remove_tile(queen, Colour, Level,X1,Y1),
    check_adjacent(X2,Y2),
    add_tile(queen, Colour,X2,Y2,Level,0).

add(Bug, Colour,X2,Y2,Level,Move):-
    add_tile(Bug, Colour,X2,Y2,Level,Move).
