:- module(bfs,
          [ one_hive_rule_fullfill/5, exist_path/7, exist_3tiles_path/7]).

:- use_module(predicates,
              [ tile/5,
                add_tile/6,
                remove_tile/5,
                check_adjacents/3
              ]).


adj((X,Y), (X2, Y2)):- X2 is X - 1, Y2 is Y, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- X2 is X, Y2 is Y - 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- X2 is X + 1, Y2 is Y - 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- X2 is X + 1, Y2 is Y, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- X2 is X, Y2 is Y + 1, tile(_, _, X2, Y2, 0).
adj((X,Y), (X2, Y2)):- X2 is X - 1, Y2 is Y + 1, tile(_, _, X2, Y2, 0). 

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

%CHECK FOR PATH AROUND THE HIVE FROM SOURCE TO DESTINATION
empty_space_around_hive((X, Y)):- 
    not(tile(_, _, X, Y, 0)), 
    check_adjacents(X, Y,_).

% empty_space_around_hive((X, Y), (X2, Y2)):- 
%     not(tile(_, _, X2, Y2, 0)), 
%     check_adjacents_except(X2, Y2, (X, Y)).

free_adj_tile((X,Y), (X2, Y2)):- X2 is X - 1, Y2 is Y, empty_space_around_hive((X2, Y2)).
free_adj_tile((X,Y), (X2, Y2)):- X2 is X, Y2 is Y-1, empty_space_around_hive((X2, Y2)).
free_adj_tile((X,Y), (X2, Y2)):- X2 is X + 1, Y2 is Y-1, empty_space_around_hive((X2, Y2)).
free_adj_tile((X,Y), (X2, Y2)):- X2 is X + 1, Y2 is Y, empty_space_around_hive((X2, Y2)).
free_adj_tile((X,Y), (X2, Y2)):- X2 is X, Y2 is Y+1, empty_space_around_hive((X2, Y2)).
free_adj_tile((X,Y), (X2, Y2)):- X2 is X - 1, Y2 is Y+1, empty_space_around_hive((X2, Y2)).

%Source in the form [X,Y]: Coordinates of the origin, Adj in the form [X,Y]: destination
bfsft(Source, Destination):- bfsfta([Source], [Source], Destination).
bfsfta([[X,Y]|_R], _Seen, [X, Y]).
bfsfta([[X,Y]|R], Seen, [Xd, Yd]):- 
    write("Next in path: "), write([X,Y]), write("\n"),
    bagof([X2, Y2],
        (free_adj_tile((X,Y),(X2, Y2)), 
        not(member([X2,Y2], Seen))), Adj),
    append(Seen, Adj, UpdSeen), 
    append(R, Adj, UpdR), !,
    write("Updated list:"), write(UpdR), write("\n"),
    bfsfta(UpdR, UpdSeen, [Xd, Yd]);
    bfsfta(R, Seen, [Xd, Yd]).

exist_path_aux(X1, Y1, X2, Y2):- bfsft([X1, Y1], [X2, Y2]), !.

exist_path(Bug, Colour, X1, Y1, Level, X2, Y2) :-
    remove_tile(Bug, Colour, X1, Y1, Level),
    exist_path_aux(X1, Y1, X2, Y2), !,
    assert(tile(Bug, Colour, X1, Y1, Level)), !.

exist_path(Bug, Colour, X1, Y1, Level, _, _) :-
    not(assert(tile(Bug, Colour, X1, Y1, Level))).

add_element_to_list([], _L, []).
add_element_to_list([[X,Y]|R], L, [[X,Y,L]|W]):- add_element_to_list(R, L, W).

%Source in the form [X,Y]: Coordinates of the origin, Adj in the form [X,Y]: destination
bfs3tp([X,Y], Destination):- write("Modifying source in bfs3tp. Source: "), 
                            write([X, Y, 0]), 
                            write(" Seen: "), 
                            write([X, Y]), 
                            write(" Destination "),
                            write(Destination), 
                            write("\n"),
                            bfs3tpa([[X, Y, 0]], [[X, Y]], Destination).

bfs3tpa([[X, Y, 4]|_R], _Seen, [X, Y]):- write("llegue a 4\n"),!,fail.
bfs3tpa([[X, Y, _L]|_R], _Seen, [X, Y]).
bfs3tpa([[X, Y, L]|R], Seen, [Xd, Yd]):- 
    write("Next in path: "), write([X,Y]), write(" Level "), write(L), write("\n"),
    bagof([X2, Y2],
        (free_adj_tile((X,Y),(X2, Y2)), 
        not(member([X2,Y2], Seen))), Adj),
    LP is L+1,
    add_element_to_list(Adj, LP, NewAdj),
    write("Updated list:"), write(NewAdj), write("\n"),
    append(Seen, Adj, UpdSeen), 
    append(R, NewAdj, UpdR), !,
    bfs3tpa(UpdR, UpdSeen, [Xd, Yd]);
    bfs3tpa(R, Seen, [Xd, Yd]).

%arreglar lo de la hormiga de que el tile donde inicia no debe contar como adyacente a uno de sus caminos
exist_3tiles_path_aux(X1, Y1, X2, Y2):- write("Entering 3tp aux \n"), bfs3tp([X1, Y1], [X2, Y2]), !.

exist_3tiles_path(Bug, Colour, X1, Y1, Level, X2, Y2) :-
    remove_tile(Bug, Colour, X1, Y1, Level),
    exist_3tiles_path_aux(X1, Y1, X2, Y2), !,
    assert(tile(Bug, Colour, X1, Y1, Level)), !.

exist_3tiles_path(Bug, Colour, X1, Y1, Level, _, _) :-
    not(assert(tile(Bug, Colour, X1, Y1, Level))).
