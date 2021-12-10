:- use_module(board,
          [ initialize/0,
          update_last_move/7,
            available/2,
            increase_move_count/0, 
            check_queen_moves/1,
            is_game_over/1,
            check_turn/1,
            can_move/5,
            print_state/0,
            print_state/1
          ]).

:- use_module(moves,[get_top_bug/5,move/6, move/4]).

start():-
    initialize().

move_to(X1,Y1,X2,Y2):-
    get_top_bug(Bug, Colour, X1, Y1, Level),
    check_turn(Colour),
    check_queen_moves(Colour),
    can_move(Bug, Colour, X1, Y1,Level ),
    move(X1,Y1,X2,Y2),!,
    tile(Bug, Colour, X2, Y2, NewLevel),
    update_last_move(Bug, Colour, X2, Y2, NewLevel, false,false),
    increase_move_count(),
    not(is_game_over(_)),!,
    print_state().

move_to(_,_,_,_):-
    is_game_over(Loser),
    print_state(Loser).