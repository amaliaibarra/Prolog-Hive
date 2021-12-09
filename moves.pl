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

try_move(Bug, Colour, X1, Y1, Level, X2, Y2) :-
    remove_tile(Bug, Colour, X1, Y1, Level),
    write("Tile removed"),
    check_adjacents(X2, Y2, _), !,
    write("Exists adjacents"),
    assert(tile(Bug, Colour, X1, Y1, Level)), !.

try_move(Bug, Colour, X1, Y1, Level, _, _) :-
    not(assert(tile(Bug, Colour, X1, Y1, Level))).

get_top_bug(Bug, Colour, X, Y, Level) :-
    findall(L,
            tile(_, _, X, Y, L),
            Bag),
    max_list(Bag, Level),
    tile(Bug, Colour, X, Y, Level).

move(X1, Y1, X2, Y2) :-
    position_available(X2, Y2),
    get_top_bug(Bug, Colour, X1, Y1, Level),
    try_move(Bug,
             Colour,
             X1,
             Y1,
             Level,
             X2,
             Y2),
    move_tile(Bug,
              Colour,
              Level,
              X1,
              Y1,
              X2,
              Y2), !.

move(X1, Y1, X2, Y2) :-
    write("Beetle Move"),
    not(position_available(X2, Y2)),
    write("Position is not available"),
    get_top_bug(Bug, Colour, X1, Y1, Level),
    Bug==beetle,
    write("There is a beetle in start position"),
    try_move(beetle,
             Colour,
             X1,
             Y1,
             Level,
             X2,
             Y2),
    move_tile(beetle,
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
    % check_adjacents_except(X2, Y2,  (X1, Y1)),
    remove_tile(queen, Colour, X1, Y1, Level),
    assert(tile(queen, Colour, X2, Y2, Level)).

move_tile(beetle, Colour, Level, X1, Y1, X2, Y2) :-
    write("move_tile beetle"),
    % try_move(beetle,
    %          Colour,
    %          X1,
    %          Y1,
    %          Level,
    %          X2,
    %          Y2),
    findall(L,
            tile(_, _, X2, Y2, L),
            Bag),
    NonEmptyBag=[-1|Bag],
    max_list(NonEmptyBag, MaxLevel),
    NewLevel is MaxLevel+1,
    remove_tile(beetle, Colour, X1, Y1, Level),
    assert(tile(beetle, Colour, X2, Y2, NewLevel)).

add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).

