:- module(board,
          [ initialize/0,
            available/2,
            increase_count/2
          ]).
:- use_module(bfs, [adj/2]).
:- use_module(predicates, [tile/5]).

:- (dynamic max_count/2).
:- (dynamic move_count/1).
:- (dynamic queen_in_game/1).
:- (dynamic insertion_count/3).

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
    assert(last_move(-1, -1, a, b, c, d)).

available(Bug, Colour) :-
    insertion_count(Bug, Colour, Count),
    max_count(Bug, MaxCount),
    Count<MaxCount.

increase_count(Bug, Colour) :-
    insertion_count(Bug, Colour, Count),
    NewCount=Count+1,
    retract(insertion_count(Bug, Colour, Count)),
    assert(insertion_count(Bug, Colour, NewCount)).

check_queen_moves(Colour) :-
    queen_in_game(Colour).

check_queen_moves(w) :-
    move_count(Count),
    Count<7.

check_queen_moves(b) :-
    move_count(Count),
    Count<8.

is_game_over(Colour) :-
    tile(queen, Colour, X, Y, _),
    is_surrounded(X, Y), !.

is_surrounded(X, Y) :-
    findall((Xi, Yi),
            adj((X, Y),  (Xi, Yi)),
            Adjacents),
    length(Adjacents, 6).
    
