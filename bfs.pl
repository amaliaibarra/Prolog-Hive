:- module(bfs,
          [ one_hive_rule_fullfill/5]).

:- use_module(predicates,
              [ tile/5,
                add_tile/6,
                remove_tile/5
              ]).

% enforcing the ONE HIVE RULE: The hive can't be divided by any movement
% Idea: Do a dfs|bfs (without the tile in the origin) and check if all pieces found are the ones tha findall or bagof returned as tile 
%Idea 2: Do a bfs from one of the adjacents and check if the others are reached. If not, the hive is broke.

adj((X,Y), (X2, Y2)):- write("P1"), X2 is X - 1, Y2 is Y, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- write("P2"), X2 is X, Y2 is Y - 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- write("P3"), X2 is X + 1, Y2 is Y - 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- write("P4"), X2 is X + 1, Y2 is Y, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- write("P5"), X2 is X, Y2 is Y + 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- write("P6\n"), X2 is X - 1, Y2 is Y + 1, tile(_, _, X2, Y2, 0). 

%deben ser los adyacentes q tengan bichos arriba y no contar el que se especifique(q puede ser simplemente quitarlo)

%Source in the form [X,Y]: Coordinates of the origin, Adj in the form [X,Y]: destination
bfs(Source, Destination):- bfs_1([Source], [Source], Destination).
bfs_1([[X,Y]|R], _Seen, [X, Y]):-write("Found objective "), write([[X,Y]|R]), write(", "), write([X]), write("-"), write(Y).
bfs_1([[X,Y]|R], Seen, [Xd, Yd]):- 
    write("entered bfs with\n"), write([[X,Y]|R]), write(Seen), write([Xd, Yd]),
    bagof([X2, Y2],
        (adj((X,Y),(X2, Y2)), 
        not(member([X2,Y2], Seen))), Adj),
    write("Bagof returned"), write((X,Y)), write("'s adjacents': "), write(Adj), write("\n"),
    append(Seen, Adj, UpdSeen), 
    write("Seen: "+ UpdSeen + "\n"),
    append(R, Adj, UpdR), !,
    write("Resto: "+ UpdR+ "\n"),
    bfs_1(UpdR, UpdSeen, [Xd, Yd]);
    write("Bfs again, no habia adyacentes \n"),
    bfs_1(R, Seen, [Xd, Yd]).

diferents((X,Y), (X1,Y1)):- X=\=X1, !; Y=\=Y1.

one_hive_rule_fullfill(_Bug, _Colour, X, Y, _Level) :-
    findall((X2, Y2), adj((X,Y), (X2, Y2)), Adj),
    write("Adjacents:"), write(Adj),
    length(Adj, Len),
    write("\n Adj's Lenght: "), write(Len), write("\n"),
    Len =< 1, write("menor o igual q 1"),!.
    
one_hive_rule_fullfill(Bug, Colour, X, Y, Level) :-
    write("Removing tile\n"),
    remove_tile(Bug, Colour, X, Y, Level),
    one_hive_without(X,Y), !,
    write("Returning tile to original place m2\n"),
    assert(tile(Bug, Colour, X, Y, Level)), !.

one_hive_rule_fullfill(Bug, Colour, X, Y, Level) :-
    write("Returning tile to original place m3\n"),
    not(assert(tile(Bug, Colour, X, Y, Level))).

one_hive_without(X,Y):- 
    findall([Ax, Ay], adj((X,Y),(Ax, Ay)), [[Ax1, Ay1]|R]), 
    write("Adyacentes vivos\n"),
    write([[Ax1, Ay1]|R]),
    write("\n"),
    findall([X2, Y2], (adj((X,Y),(X2, Y2)), diferents((X2, Y2),(Ax1, Ay1)), bfs([Ax1, Ay1], [X2, Y2])), H), 
    write("Nodos alcanzados\n"),
    write(H),
    H == R.
