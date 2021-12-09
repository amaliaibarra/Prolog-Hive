:- use_module(predicates,
              [ tile/5,
                is_adjacent/4,
                position_available/2,
                check_adjacents_except/3,
                check_adjacents/3,
                add_tile/6,
                remove_tile/5
              ]).
:- use_module(validations, [pilled/2, validate_grasshoper_move/4]).
:- use_module(bfs, [one_hive_rule_fullfill/5]).


try_move(Bug, Colour, X1, Y1, Level, X2, Y2) :-
    remove_tile(Bug, Colour, X1, Y1, Level),
    check_adjacents(X2, Y2, _), !,
    assert(tile(Bug, Colour, X1, Y1, Level)), !.

try_move(Bug, Colour, X1, Y1, Level, _, _) :-
    not(assert(tile(Bug, Colour, X1, Y1, Level))).

get_top_bug(Bug, Colour, X, Y, Level) :-
    findall(L,
            tile(_, _, X, Y, L),
            Bag),
    max_list(Bag, Level),
    tile(Bug, Colour, X, Y, Level).

%moving pillbug
move(X1, Y1, X2, Y2, X3, Y3) :-
    position_available(X3, Y3), !,
    tile(pillbug, Colour, X1, Y1, 0), !,
    not(pilled(X1, Y1)), !, %pillbug is not blocked
    one_hive_rule_fullfill(pillbug, Colour, X1, Y1, 0),
    move_tile(pillbug,
              X1,
              Y1,
              X2,
              Y2,
              X3,
              Y3,
              _).
    
%Moving general tile to free position
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
    one_hive_rule_fullfill(Bug, Colour, X1, Y1, Level),
    move_tile(Bug,
              Colour,
              Level,
              X1,
              Y1,
              X2,
              Y2), !.

%moving beetle to pilled position
move(X1, Y1, X2, Y2) :-
    not(position_available(X2, Y2)),
    get_top_bug(Bug, Colour, X1, Y1, Level),
    Bug==beetle,
    try_move(beetle,
             Colour,
             X1,
             Y1,
             Level,
             X2,
             Y2),
    one_hive_rule_fullfill(Bug, Colour, X1, Y1, Level),
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
    remove_tile(queen, Colour, X1, Y1, Level),
    assert(tile(queen, Colour, X2, Y2, Level)).

move_tile(beetle, Colour, Level, X1, Y1, X2, Y2) :-
    findall(L,
            tile(_, _, X2, Y2, L),
            Bag),
    NonEmptyBag=[-1|Bag],
    max_list(NonEmptyBag, MaxLevel),
    NewLevel is MaxLevel+1,
    remove_tile(beetle, Colour, X1, Y1, Level),
    assert(tile(beetle, Colour, X2, Y2, NewLevel)).

move_tile(pillbug, Colour, Level, X1, Y1, X2, Y2) :-
    remove_tile(pillbug, Colour, X1, Y1, Level),
    assert(tile(pillbug, Colour, X2, Y2, Level)).

move_tile(pillbug, X1, Y1, X2, Y2, X3, Y3, _) :-
    is_adjacent(X1, Y1, X3, Y3), !,
    is_adjacent(X1, Y1, X2, Y2), !,
    not(pilled(X2, Y2)), !,
    tile(Bug, Colour, X2, Y2, 0), !, %Get tile in X2,Y2
    try_move(Bug, Colour, X2, Y2, 0, X3, Y3), !,
    remove_tile(Bug, Colour, X2, Y2, 0),
    assert(tile(Bug, Colour, X3, Y3, 0)).

add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).

