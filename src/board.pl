:- module(board,
          [ move_count/1,
            initialize/0,
            available/2,
            update_last_move/7,
            increase_move_count/0,
            increase_bug_count/2,
            check_queen_moves/2,
            is_game_over/0,
            check_turn/1,
            can_move/5,
            print_state/0,
            get_next_colour/1
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
    assert(last_move(-1, b, a, x, c, d, f)).

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
        !
    ;   Bug==queen
    ).

check_queen_moves(_, w) :-
    move_count(Count),
    Count<6.

check_queen_moves(_, b) :-
    move_count(Count),
    Count<7.

is_game_over() :-
    tie(),!;
    tile(queen, Colour, X, Y, _),
    is_surrounded(X, Y), !,
    write("GAME OVER, LOSER:"),
    write(Colour).

tie():-
    tile(queen, w, X1, Y1, _),
    is_surrounded(X1, Y1),
    tile(queen, b, X2, Y2, _),
    is_surrounded(X2, Y2),
    write("GAME OVER, IT'S A TIE").

is_surrounded(X, Y) :-
    findall((Xi, Yi),
            adj((X, Y),  (Xi, Yi)),
            Adjacents),
    write(Adjacents),
    length(Adjacents, 6).

check_turn(b) :-
    move_count(Count),
    Count mod 2=:=1.


check_turn(w) :-
    move_count(Count),
    write(Count),
    Count mod 2=:=0.
    

can_move(Bug, Colour, X, Y, Level) :-
    not(last_move(Bug,
                  Colour,
                  X,
                  Y,
                  Level,
                  true,
                  _)), !.

print_state() :-
    findall((Bug, Colour,X, Y,Level), tile(Bug, Colour,X, Y,Level), Tiles),
    get_next_colour(NextColour),
    write("Current state of game: \n"),
    print_tiles(Tiles),
    write("Next player: "),
    write(NextColour),
    write("\n").


print_tiles([(Bug, Colour,X, Y,Level)|R]):-
    write("     ["),
    write(X),
    write(","),
    write(Y),
    write("]: "),
    write(Bug),
    write(", Colour: "),
    write(Colour),
    write(", Level: "),
    write(Level), 
    write("\n"), 
    print_tiles(R).

print_tiles([]).

opposite_colour(w, b).
opposite_colour(b, w).

get_next_colour(Colour):-
    last_move(_, LastColour,_,_,_,_,_),
    opposite_colour(LastColour, Colour).

