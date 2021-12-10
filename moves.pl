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
:- use_module(bfs, [one_hive_rule_fullfill/5, exist_path/7, exist_3tiles_path/7, exist_3tilesjumping_path/7]).


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

common_validation(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow):-
    try_move(Bug,
            Colour,
            X1,
            Y1,
            Level,
            X2,
            Y2),
    one_hive_rule_fullfill(Bug, Colour, X1, Y1, Level),
    write("OHR fullfilled \n"),
    move_tile(MoveToFollow,
        Colour,
        Level,
        X1,
        Y1,
        X2,
        Y2).

validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow):-
    write("Validating general move1 \n"),
    position_available(X2, Y2),!,
    write("Position available \n"),
    common_validation(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow).
   
%moving beetle/mosquito to pilled position
validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow):-
    write("Validating general move2 \n"),
    write("Is beetle or mosquito? \n"),
    not(position_available(X2, Y2)),
    get_top_bug(Bug, Colour, X1, Y1, Level),
    MoveToFollow == beetle,
    % (Bug == beetle, !; Bug == mosquito),%OR MOSQUITO
    write("Confirmed, is beetle or mosquito\n"),
    common_validation(Bug, Colour, X1, Y1, Level, X2, Y2, MoveToFollow).

validate_pillbug_power(_Bugname, Colour, X1, Y1, Level, X2, Y2, X3, Y3):-
    write("Validating pillbug/ mosquito power\n"),
    position_available(X3, Y3), !,
    not(pilled(X1, Y1)), !, %pillbug is not blocked
    write("not blocked \n"),
    one_hive_rule_fullfill(_Bug, Colour, X2, Y2, Level),
    write("One hive rule fullfilled\n"),
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
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR beetle AQUI
    assert(tile(Bug, Colour, X2, Y2, NewLevel)).%QUITAR beetle AQUI

%(X1,Y1): mosquito's coordinates, (X2, Y2): adj to inherit powers of, (X3, Y3):piece to move, (X4, Y4): place to move to
move(X1, Y1, X2, Y2, X3, Y3, X4, Y4):- 
    write("Mosquito: "), write(X1-Y1), write(" as pillbug: "), write(X2-Y2), write(" moving: "), write(X3-Y4), write(" to: "), write(X4-Y4), write("\n"),
    get_top_bug(Bug1, Colour1, X1, Y1, Level1),
    Bug1 == mosquito,!,
    write("Confirmed is moquito \n"),
    Level1 == 0,
    write("In level 0\n"),
    is_adjacent(X1, Y1, X2, Y2),
    tile(pillbug, _Colour2, X2, Y2, _Level2),
    validate_pillbug_power(mosquito, Colour1, X1, Y1, Level1, X3, Y3, X4, Y4).

%pillbug (X1, Y1) moves (X2, Y2) to (X3, Y3)
move(X1, Y1, X2, Y2, X3, Y3) :-
    write("Is pillbug?\n"),
    tile(pillbug, Colour, X1, Y1, 0), !,%Or mosquito
    write("Is pillbug confirmed \n"),
    validate_pillbug_power(pillbug, Colour, X1, Y1, 0, X2, Y2, X3, Y3).
    
%(X1,Y1): mosquito's coordinates, (X2, Y2): adj to inherit powers of, (X3, Y3): place to move to
move(X1, Y1, X2, Y2, X3, Y3):- 
    write("Mosquito moving as adj \n"),
    get_top_bug(Bug1, Colour1, X1, Y1, Level1),
    Bug1 == mosquito,!,
    write("Confirmed, Is mosquito \n"),
    Level1 == 0,
    write("In level 0\n"),
    is_adjacent(X1, Y1, X2, Y2),
    write("Piece to inherit power of is adjacent \n"),
    get_top_bug(Bug2, _Colour2, X2, Y2, _Level2),
    not(member(Bug2, [mosquito])),
    write("Adj is not mosquito \n"),
    write("Adjacent is: "), write(Bug2), write("\n"),
    validate_general_move(Bug1, Colour1, X1, Y1, Level1, X3, Y3, Bug2),!.

%Moving general tile to free position
move(X1, Y1, X2, Y2) :-
    get_top_bug(Bug, Colour, X1, Y1, Level),
    validate_general_move(Bug, Colour, X1, Y1, Level, X2, Y2, Bug).

move_tile(grasshopper, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving grasshopper\n"),
    validate_grasshoper_move(X1, Y1, X2, Y2),
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR Grasshopper AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR Grasshopper AQUI comprobar que al eliminar se le pueda pasar un avariable

move_tile(queen, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving queen\n"),
    is_adjacent(X1,Y1, X2, Y2),
    %a;adir comprobacion de si es adyacente la pos a la que va
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR queen AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR queen AQUI

move_tile(mosquito, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving mosquito in second level(as beetle)\n"),
    beetle_move(mosquito, Colour, Level, X1, Y1, X2, Y2).

move_tile(beetle, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving beetle\n"),
    is_adjacent(X1,Y1, X2, Y2),
    beetle_move(beetle, Colour, Level, X1, Y1, X2, Y2).

move_tile(pillbug, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving pillbug\n"),
    is_adjacent(X1,Y1, X2, Y2),
    %a;adir comprobacion de si es adyacente la pos a la que va
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR pillbug AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR pillbug AQUI

%check wether or not there is a path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(ant, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving ant\n"),
    exist_path(Bug, Colour, X1, Y1, Level,  X2, Y2),%QUITAR ant AQUI
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR ant AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR ant AQUI

%check wether or not there is a 3 tiles path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(spider, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving spider\n"),
    exist_3tiles_path(Bug, Colour, X1, Y1, Level, X2, Y2),%QUITAR spider AQUI
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR spider AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR spider AQUI

%check wether or not there is a 3 tiles path from (X1,Y1) to (X2,Y2) with just empty celds surrounding the hive
move_tile(ladybug, Colour, Level, X1, Y1, X2, Y2) :-
    write("Moving ladybug\n"),
    exist_3tilesjumping_path(Bug, Colour, X1, Y1, Level, X2, Y2),%QUITAR spider AQUI
    remove_tile(Bug, Colour, X1, Y1, Level),%QUITAR ladybug AQUI
    assert(tile(Bug, Colour, X2, Y2, Level)).%QUITAR ladybug AQUI

move_tile(pillbug, X1, Y1, X2, Y2, X3, Y3, _) :-
    write("Pillbug moving tile\n"),
    is_adjacent(X1, Y1, X3, Y3), !,
    is_adjacent(X1, Y1, X2, Y2), !,
    not(pilled(X2, Y2)), !,
    tile(Bug, Colour, X2, Y2, 0), !, %Get tile in X2,Y2
    try_move(Bug, Colour, X2, Y2, 0, X3, Y3), !,
    remove_tile(Bug, Colour, X2, Y2, 0),
    assert(tile(Bug, Colour, X3, Y3, 0)).

add(Bug, Colour, X2, Y2, Level, Move) :-
    add_tile(Bug, Colour, X2, Y2, Level, Move).

