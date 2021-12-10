:- module(bfs,
          [ adj/2,
            one_hive_rule_fullfill/5
          ]).

:- use_module(predicates, [tile/5, add_tile/6, remove_tile/5]).


adj((X, Y),  (X2, Y2)) :-
    X2 is X-1,
    Y2 is Y,
    tile(_, _, X2, Y2, 0).
adj((X, Y),  (X2, Y2)) :-
    X2 is X,
    Y2 is Y-1,
    tile(_, _, X2, Y2, 0).
adj((X, Y),  (X2, Y2)) :-
    X2 is X+1,
    Y2 is Y-1,
    tile(_, _, X2, Y2, 0).
adj((X, Y),  (X2, Y2)) :-
    X2 is X+1,
    Y2 is Y,
    tile(_, _, X2, Y2, 0).
adj((X, Y),  (X2, Y2)) :-
    X2 is X,
    Y2 is Y+1,
    tile(_, _, X2, Y2, 0).
adj((X, Y),  (X2, Y2)) :-
    X2 is X-1,
    Y2 is Y+1,
    tile(_, _, X2, Y2, 0). 

%Source in the form [X,Y]: Coordinates of the origin, Adj in the form [X,Y]: destination
bfs(Source, Destination):- bfs_1([Source], [Source], Destination).
bfs_1([[X,Y]|_R], _Seen, [X, Y]).
bfs_1([[X,Y]|R], Seen, [Xd, Yd]):- 
    bagof([X2, Y2],
        (adj((X,Y),(X2, Y2)), 
        not(member([X2,Y2], Seen))), Adj),
    append(Seen, Adj, UpdSeen), 
    append(R, Adj, UpdR), !,
    bfs_1(UpdR, UpdSeen, [Xd, Yd]);
    bfs_1(R, Seen, [Xd, Yd]).

diferents((X,Y), (X1,Y1)):- X=\=X1, !; Y=\=Y1.

one_hive_without(X,Y):- 
    findall([Ax, Ay], adj((X,Y),(Ax, Ay)), [[Ax1, Ay1]|R]), 
    findall([X2, Y2], (adj((X,Y),(X2, Y2)), diferents((X2, Y2),(Ax1, Ay1)), bfs([Ax1, Ay1], [X2, Y2])), H), 
    H == R.

% enforcing the ONE HIVE RULE: The hive can't be divided by any movement
%Idea 2: Do a bfs from one of the adjacents and check if the others are reached. If not, the hive is broke.
one_hive_rule_fullfill(_Bug, _Colour, X, Y, _Level) :-
    findall((X2, Y2), adj((X,Y), (X2, Y2)), Adj),
    length(Adj, Len),
    Len =< 1, !.
    
one_hive_rule_fullfill(Bug, Colour, X, Y, Level) :-
    remove_tile(Bug, Colour, X, Y, Level),
    one_hive_without(X,Y), !,
    assert(tile(Bug, Colour, X, Y, Level)), !.

one_hive_rule_fullfill(Bug, Colour, X, Y, Level) :-
    not(assert(tile(Bug, Colour, X, Y, Level))).
