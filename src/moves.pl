:- module(moves,
          [ get_top_bug/5,
            move/6,
            move/4,
            move/8
          ]).
:- use_module(predicates,
              [ tile/5,
                is_adjacent/4,
                position_available/2,
                check_adjacents_except/3,
                check_adjacents/3,
                remove_tile/5
              ]).
:- use_module(validations, [pilled/2, validate_grasshoper_move/4]).
:- use_module(bfs,
              [ one_hive_rule_fullfill/5,
                exist_path/7,
                exist_3tiles_path/7,
                exist_3tilesjumping_path/7
              ]).


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

common_validation(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow) :-
    try_move(Bug,
             Colour,
             X1,
             Y1,
             Level,
             X2,
             Y2),
    one_hive_rule_fullfill(Bug, Colour, X1, Y1, Level),
    move_tile(MoveToFollow,
              Colour,
              Level,
              X1,
              Y1,
              X2,
              Y2).

validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow) :-
    position_available(X2, Y2), !,
    common_validation(Bug,
                      Colour,
                      X1,
                      Y1,
                      Level,
                      X2,
                      Y2,
                      MoveToFollow).
   
%moving beetle/mosquito to pilled position
validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow):-
    not(position_available(X2, Y2)),
    get_top_bug(Bug, Colour, X1, Y1, Level),
    MoveToFollow == beetle,
    common_validation(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow).

validate_pillbug_power(_Bugname, Colour, X1, Y1, Level, X2, Y2, X3, Y3):-
    position_available(X3, Y3), !,
    not(pilled(X1, Y1)), !, %pillbug is not blocked
    one_hive_rule_fullfill(_Bug, Colour, X2, Y2, Level),
    move_tile(pillbug,
              X1,
              Y1,
              X2,
              Y2,
              X3,
              Y3,
              _).

beetle_move(_B, Colour, Level, X1, Y1, X2, Y2):-
    findall(L,
            tile(_, _, X2, Y2, L),
            Bag),
    NonEmptyBag=[-1|Bag],
    max_list(NonEmptyBag, MaxLevel),
    NewLevel is MaxLevel+1,
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, NewLevel)).

%(X1,Y1): mosquito's coordinates, (X2, Y2): adj to inherit powers of, (X3, Y3):piece to move, (X4, Y4): place to move to
move(X1, Y1, X2, Y2, X3, Y3, X4, Y4):- 
    get_top_bug(Bug1, Colour1, X1, Y1, Level1),
    Bug1 == mosquito,!,
    Level1 == 0,
    is_adjacent(X1, Y1, X2, Y2),
    tile(pillbug, _Colour2, X2, Y2, _Level2),
    validate_pillbug_power(mosquito, Colour1, X1, Y1, Level1, X3, Y3, X4, Y4).

%pillbug (X1, Y1) moves (X2, Y2) to (X3, Y3)
move(X1, Y1, X2, Y2, X3, Y3) :-
    tile(pillbug, Colour, X1, Y1, 0), !,%Or mosquito
    validate_pillbug_power(pillbug, Colour, X1, Y1, 0, X2, Y2, X3, Y3).
    
%(X1,Y1): mosquito's coordinates, (X2, Y2): adj to inherit powers of, (X3, Y3): place to move to
move(X1, Y1, X2, Y2, X3, Y3):- 
    get_top_bug(Bug1, Colour1, X1, Y1, Level1),
    Bug1 == mosquito,!,
    Level1 == 0,
    is_adjacent(X1, Y1, X2, Y2),
    get_top_bug(Bug2, _Colour2, X2, Y2, _Level2),
    not(member(Bug2, [mosquito])),
    validate_general_move(Bug1, Colour1, X1, Y1, Level1, X3, Y3, Bug2),!.

%Moving general tile to free position
move(X1, Y1, X2, Y2) :-
    get_top_bug(Bug, Colour, X1, Y1, Level),
    validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, Bug).

move_tile(grasshopper, Colour, Level, X1, Y1, X2, Y2) :-
    validate_grasshoper_move(X1, Y1, X2, Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
    is_adjacent(X1,Y1, X2, Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

move_tile(mosquito, Colour, Level, X1, Y1, X2, Y2) :-
    Level > 0,
    beetle_move(mosquito, Colour, Level, X1, Y1, X2, Y2).

move_tile(beetle, Colour, Level, X1, Y1, X2, Y2) :-
    is_adjacent(X1,Y1, X2, Y2),
    beetle_move(beetle, Colour, Level, X1, Y1, X2, Y2).

move_tile(pillbug, Colour, Level, X1, Y1, X2, Y2) :-
    is_adjacent(X1,Y1, X2, Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

%check wether or not there is a path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(ant, Colour, Level, X1, Y1, X2, Y2) :-
    exist_path(Bug,
               Colour,
               X1,
               Y1,
               Level,
               X2,
               Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

%check wether or not there is a 3 tiles path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(spider, Colour, Level, X1, Y1, X2, Y2) :-
    exist_3tiles_path(Bug,
                      Colour,
                      X1,
                      Y1,
                      Level,
                      X2,
                      Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

%check wether or not there is a 3 tiles path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(ladybug, Colour, Level, X1, Y1, X2, Y2) :-
    exist_3tilesjumping_path(Bug,
                             Colour,
                             X1,
                             Y1,
                             Level,
                             X2,
                             Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),
    assert(tile(Bug, Colour, X2, Y2, Level)).

move_tile(pillbug, X1, Y1, X2, Y2, X3, Y3, _) :-
    is_adjacent(X1, Y1, X3, Y3), !,
    is_adjacent(X1, Y1, X2, Y2), !,
    not(pilled(X2, Y2)), !,
    tile(Bug, Colour, X2, Y2, 0), !, %Get tile in X2,Y2
    try_move(Bug, Colour, X2, Y2, 0, X3, Y3), !,
    remove_tile(Bug, Colour, X2, Y2, 0),
    assert(tile(Bug, Colour, X3, Y3, 0)).


