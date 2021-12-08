:- use_module(predicates,
              [ position_available/2,
                add_tile/6,
                is_adjacent/4,
                remove_tile/5,
                check_adjacents/3,
                tile/5,
                check_adjacents_except/3,
                adjacents/3
              ]).

:- use_module(utils, [remove/3]).


exist_adjacent1([]).

exist_adjacent1([(X,Y)|R]):-
    tile(_, _, X, Y, _), !; exist_adjacent1(R).

check_adjacents_except(X, Y, Omit) :-
    adjacents(X, Y, Adjacents),
    write(Adjacents),
    remove(Omit, Adjacents, AdjacentsToAnalize),
    write(AdjacentsToAnalize),
    exist_adjacent1(AdjacentsToAnalize).


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
    remove_tile(queen, Colour, X1, Y1, Level),
    assert(tile(queen, Colour, X2, Y2, Level)).

add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).


% move_tile(ant,X1, Y1, X2, Y2). 
% try_move(queen, Colour, Level, X1, Y1, X2, Y2) :-
%     is_adjacent(X1, Y1, X2, Y2),
%     write("Is Adjacent\n"),
%     remove_tile(queen, Colour, X1, Y1, Level),
%     write("Tile Removed\n"),
%     check_adjacents(X2, Y2, _),
%     write("Destination has adjacents\n").

% % move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
% %     try_move(queen,
% %              Colour,
% %              Level,
% %              X1,
% %              Y1,
% %              X2,
% %              Y2),
% %     write("Moving queen\n"),
% %     assert(tile(queen, Colour, X2, Y2, Level)).

% % move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
% %     write("returning queen to original place"),
% %     assert(tile(queen, Colour, X1, Y1, Level).
