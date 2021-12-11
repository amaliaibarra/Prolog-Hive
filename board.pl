:- module(board,
          [ move_count/1,
            initialize/0,
            available/2,
            update_last_move/7,
            increase_move_count/0,
            increase_bug_count/2,
            check_queen_moves/2,
            is_game_over/1,
            check_turn/1,
            can_move/5,
            print_state/0,
            print_state/1
          ]).
:- use_module(bfs, [adj/2]).
:- use_module(predicates, [tile/5]).

:- (dynamic max_count/2).
:- (dynamic move_count/1).
:- (dynamic queen_in_game/1).
:- (dynamic insertion_count/3).
:- (dynamic last_move/7).

initialize() :-
    %Set number of moves
    assert(move_count(0)),
    
    %Set maximum number of tiles per bug
    assert(max_count(queen, 1)),
    assert(max_count(ant, 3)),
    assert(max_count(grasshopper, 3)),
    assert(max_count(beetle, 2)),
    assert(max_count(spider, 2)),
    assert(max_count(mosquito, 1)),
    assert(max_count(ladybug, 1)),
    assert(max_count(pillbug, 1)),

    %Set current insertions of bugs to 0
    assert(insertion_count(queen, w, 0)),
    assert(insertion_count(queen, b, 0)),
    assert(insertion_count(ant, w, 0)),
    assert(insertion_count(ant, b, 0)),
    assert(insertion_count(grasshopper, w, 0)),
    assert(insertion_count(grasshopper, b, 0)),
    assert(insertion_count(beetle, w, 0)),
    assert(insertion_count(beetle, b, 0)),
    assert(insertion_count(spider, w, 0)),
    assert(insertion_count(spider, b, 0)),
    assert(insertion_count(mosquito, w, 0)),
    assert(insertion_count(mosquito, b, 0)),
    assert(insertion_count(ladybug, w, 0)),
    assert(insertion_count(ladybug, b, 0)),
    assert(insertion_count(pillbug, w, 0)),
    assert(insertion_count(pillbug, b, 0)),
    
    %Set dummy last move
    assert(last_move(-1, -1, a, b, c, d, f)).

available(Bug, Colour) :-
    insertion_count(Bug, Colour, Count), !,
    max_count(Bug, MaxCount), !,
    Count<MaxCount.

update_last_move(Bug, Colour, X, Y, Level, MovedByPillbug, WithDifferentColour) :-
    last_move(Bugi,
              Colouri,
              Xi,
              Yi,
              Leveli,
              Mi,
              Di), !,
    retract(last_move(Bugi,
                      Colouri,
                      Xi,
                      Yi,
                      Leveli,
                      Mi,
                      Di)), !,
    assert(last_move(Bug,
                     Colour,
                     X,
                     Y,
                     Level,
                     MovedByPillbug,
                     WithDifferentColour)), !.

increase_move_count() :-
    move_count(Count),
    retract(move_count(Count)),
    NewCount is Count+1,
    assert(move_count(NewCount)).

increase_bug_count(Bug, Colour) :-
    insertion_count(Bug, Colour, Count), !,
    NewCount is Count+1,
    retract(insertion_count(Bug, Colour, Count)), !,
    assert(insertion_count(Bug, Colour, NewCount)), !.

check_queen_moves(Bug, Colour) :-
    (   tile(queen, Colour, _, _, _),
        write("HERE"), !
    ;   Bug==queen
    ).

check_queen_moves(_, w) :-
    move_count(Count),
    Count<6.

check_queen_moves(_, b) :-
    move_count(Count),
    Count<7.

is_game_over(Colour) :-
    tile(queen, Colour, X, Y, _),
    is_surrounded(X, Y), !.

is_surrounded(X, Y) :-
    findall((Xi, Yi),
            adj((X, Y),  (Xi, Yi)),
            Adjacents),
    write("Queen adjacents that exists \n"),
    write(Adjacents),
    length(Adjacents, 6).

check_turn(b) :-
    move_count(Count),
    write("black turn\n"),
    Count mod 2=:=1,
    write("fua turn\n").


check_turn(w) :-
    move_count(Count),
    write(Count),
    write("white turn\n"),
    Count mod 2=:=0,
    write("fua2 turn\n").

can_move(Bug, Colour, X, Y, Level) :-
    not(last_move(Bug,
                  Colour,
                  X,
                  Y,
                  Level,
                  true,
                  _)), !.

print_state() :-
    write("Current state of game:\n").

print_state(Loser) :-
    write("Current state of game: \n"),
    write("Game over, Loser is: "),
    write(Loser).