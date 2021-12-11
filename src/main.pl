:- use_module(board,
              [ move_count/1,
                initialize/0,
                update_last_move/7,
                available/2,
                increase_move_count/0,
                increase_bug_count/2,
                check_queen_moves/2,
                is_game_over/0,
                check_turn/1,
                can_move/5,
                print_state/0,
                get_next_colour/1
              ]).

:- use_module(moves, [get_top_bug/5, move/6, move/4,move/8]).
:- use_module(predicates, [tile/5, add_tile/5]).


start():-
    initialize(),
    write("Initialized Game, insert your next move. \n"),
    write("Next player: 'w'. \n").
    

%Check if Bug is available for insertion!!!!!!
insert_to(Bug):-
    move_count(0), %Is first insertion
    get_next_colour(Colour),
    add_tile(Bug, Colour, 0, 0, 1),
    increase_move_count(),
    increase_bug_count(Bug,Colour),
    update_last_move(Bug, Colour, 0, 0,0, false,false),
    print_state().


insert_to(Bug, X, Y):-
    get_next_colour(Colour),
    check_turn(Colour),
    move_count(1), %Is second insertion
    add_tile(Bug, Colour, X, Y, 2),
    increase_move_count(),
    increase_bug_count(Bug,Colour),
    update_last_move(Bug, Colour, X, Y,0, false,false),
    not(is_game_over()),
    !,
    print_state().


insert_to(Bug, X, Y):-
    get_next_colour(Colour),
    check_turn(Colour),
    check_queen_moves(Bug, Colour),
    available(Bug,Colour),
    add_tile(Bug, Colour, X, Y, 0),
    increase_move_count(),
    update_last_move(Bug, Colour, X, Y,0, false,false),!,
    not(is_game_over()),
    print_state().

insert_to(_, _, _, _):-
    is_game_over().


move_to(X1,Y1,X2,Y2):-
    get_top_bug(Bug, Colour, X1, Y1, Level),
    check_turn(Colour),
    check_queen_moves(Bug, Colour),
    can_move(Bug, Colour, X1, Y1,Level ),
    move(X1,Y1,X2,Y2),!,
    tile(Bug2, Colour2, X2, Y2, NewLevel),
    update_last_move(Bug2, Colour2, X2, Y2, NewLevel, false,false),
    increase_move_count(),
    not(is_game_over()),!,
    print_state().

move_to(_,_,_,_):-
    is_game_over().

move_to(X1,Y1,X2,Y2,X3,Y3):-
    get_top_bug(Bug, Colour, X1, Y1, _),
    check_turn(Colour),
    check_queen_moves(Bug, Colour),
    tile(Bug2, Colour2, X2, Y2,Level2),
    can_move(Bug2, Colour2, X2, Y2,Level2 ),
    move(X1,Y1,X2,Y2,X3,Y3),!,
    get_top_bug(Bug3, Colour3, X3, Y3, NewLevel),
    update_last_move(Bug3, Colour3, X3, Y3, NewLevel, true,true),
    increase_move_count(),
    not(is_game_over()),!,
    print_state().

move_to(_,_,_,_,_,_):-
    is_game_over().

move_to(X1,Y1,X2,Y2,X3,Y3,X4,Y4):-
    get_top_bug(Bug1, Colour1, X1, Y1, _),
    check_turn(Colour1),
    check_queen_moves(Bug1, Colour1),
    tile(Bug3, Colour3, X3, Y3,Level3 ),
    can_move(Bug3, Colour3, X3, Y3,Level3 ),
    move(X1,Y1,X2,Y2,X3,Y3,X4,Y4),!,
    get_top_bug(Bug4, Colour4, X4, Y4, Level4),
    update_last_move(Bug4, Colour4, X4, Y4, Level4, true,true),
    increase_move_count(),
    not(is_game_over()),!,
    print_state().

move_to(_,_,_,_,_,_,_,_):-
    is_game_over().
