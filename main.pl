% initialize_board(B):-B is [].

%checks wether or not a given bug can be added to a given board's column  
validate_insertion_xcol([], [_Id,_Color,_Bug,_X,_Y]).

validate_insertion_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|Z], [Id,Color,Bug,X,Y]):- 
    Yi < Y,!,
    write("Yi<Y "), write(Yi),write("\n"), 
    validate_insertion_xcol(Z, [Id,Color,Bug,X,Y]).

validate_insertion_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|_Z], [_Id,_Color,_Bug,_X,Y]):- 
    Yi > Y,
     write("Yi>Y "), write(Yi),write("\n").

%checks wether or not a given bug can be added to the board
validate_insertion([],[_Id,_Color,_Bug,_X,_Y],_IN, _Board). %First Insertion
validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]|_R], [Id,Color,Bug,X,Y],IN,Board) :-
    X =:= Xi,!,
    write("Xi=X "), write(Xi), write("\n"),
    validate_insertion_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y]),
    check_neighbors(X,Y,Color,Board,IN). 
validate_insertion([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|R], [Id,Color,Bug,X,Y],IN,Board) :- 
    Xi < X,
    !,
    write("Xi<X "),write(Xi),write("\n"),
    validate_insertion(R,[Id, Color,Bug, X,Y],IN,Board).

validate_insertion([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|_R], [_Id,Color,_Bug,X,Y],IN,Board) :- 
    Xi > X,
    write("Xi>X "), write(Xi), write("\n"),
    check_neighbors(X,Y,Color,Board,IN). 

set_true(F):- F = true.
set_false(F):- F = false.


%compare bugs colors x column
check_colour_xcol([], [_Color,_X,_Y],F):- 
    write("No hay nadieeeee ponlo falssoooo\n"),
    set_false(F).

check_colour_xcol([[_Idi,Color,_Bugi,_Xi,Yi]|_Z], [Color,_X,Y],F):- 
    Y =:= Yi,!,
    write("Yi=Y "), write(Yi), write("\n"),
    set_true(F).

check_colour_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|Z], [Color,X,Y],F):- 
    Yi < Y,!, 
    write("Yi<Y "), write(Yi), write("\n"),
    check_colour_xcol(Z, [Color,X,Y],F).

check_colour_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|_Z], [_Color,_X,Y],F):- 
    Yi > Y,
     write("Yi>Y "), write(Yi), write("\n"),
     set_false(F).

%ccompare bug's colors
check_colour([],[_Color,_X,_Y],F):-
    write("No hay nadieeeee ponlo falssoooo\n"),
    set_false(F). %First Insertion

check_colour([[[Idi,Colori,Bugi,Xi,Yi]|Z]|_R], [Color,X,Y],F) :-
    X =:= Xi,
    !,
    write("Xi=X "), write(Xi), write("\n"),
    check_colour_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Color,X,Y],F). 

check_colour([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|R], [Color,X,Y],F) :- 
    Xi < X,
    !,
    write("Xi<X "),write(Xi),write("\n"),
    check_colour(R,[Color, X,Y],F).

check_colour([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|_R], [_Color,X,_Y],F) :- 
    Xi > X,
    write("Xi>X "), write(Xi),write("falsoooo\n"),
    set_false(F). 


same_colors(X,Y,C,B,F):- write("entered same colors \n"),check_colour(B,[C,X,Y],F).

%check if any of the tile's neighbors has a different colour
% check_neighbors(Xi,Yi,C,B,IN):- IN, .
%A LA PRIMERA FICHA DEL JUEGO SE LE DEVUELVE TRUE Y YA, pQ SIEMPRE ES POSIBLE INSERTARLA INDEPENDIENTEMENTE DE DONDE LO HAGA
%Insert second tile in the game (first one for the second player) 

%if this neighbours has a different colour
% inspect_neighbour(Xi,Yi,C,B,FA):-set_true(FA),not(same_colors(Xi,Yi,C,B,FA)),FA, write("FA is true").
% check_neighbors(Xi,Yi,C,B,IN):- IN = 1,!, write("first insertion\n"), B = [[E]],(inspect_neighbour(Xi-1,Yi,C,B,FA1); inspect_neighbour(Xi-1,Yi+1,C,B,FA2) ; inspect_neighbour(Xi,Yi-1,C,B,FA3) ; inspect_neighbour(Xi,Yi+1,C,B,FA4);inspect_neighbour(Xi+1,Yi-1,C,B,FA5); inspect_neighbour(Xi+1,Yi,C,B,FA6)).

check_neighbors(Xi,Yi,C,B,IN):-
     IN = 1,
     !,
     write("first insertion\n"),
     B = [[_E]],
     (write("caso 1: "),not(same_colors(Xi-1,Yi,C,B,FA1)), FA1, var(FA1),!;
     write("caso 2: "),not(same_colors(Xi-1,Yi+1,C,B,FA2)), FA2, var(FA2),!;
     write("caso 3: "),not(same_colors(Xi,Yi-1,C,B,FA3)), write("not same colors"), var(FA3),!,write(FA3), write("var(FA3)\n");
     write("caso 4: "),not(same_colors(Xi,Yi+1,C,B,FA4)), FA4, var(FA4),!;
     write("caso 5: "),not(same_colors(Xi+1,Yi-1,C,B,FA5)), FA5, var(FA5),!;
     write("caso 6: "),not(same_colors(Xi+1,Yi,C,B,FA6)), FA6, var(FA6)).

%Insert other tile
check_neighbors(Xi,Yi,C,B,_):- 
    same_colors(Xi-1,Yi,C,B,FA1),
    same_colors(Xi-1,Yi+1,C,B,FA2),
    same_colors(Xi,Yi-1,C,B,FA3),
    same_colors(Xi,Yi+1,C,B,FA4),
    same_colors(Xi+1,Yi-1,C,B,FA5),
    same_colors(Xi+1,Yi,C,B,FA6),
    (FA1,!; FA2,!; FA3,!; FA4,!; FA5,!; FA6,!).


%board_row, Tile to add, result
add_tile([], [Id,Color,Bug,X,Y],[Id,Color,Bug,X,Y]).

add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],[[Idi,Colori,Bugi,Xi,Yi]|R]):-
     Yi < Y,
    add_tile(Z, [Id,Color,Bug,X,Y],R).

add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],[[Id,Color,Bug,X,Y]|[[Idi,Colori,Bugi,Xi,Yi]|Z]]):- Yi > Y.
% add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],L):- Yi > Y, L is[[Id,Color,Bug,X,Y]|Z].



% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board([Id,Color,Bug,X,Y,First]).

% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board(board,[-1,-1],[Id,Color,Bug,X,Y,First]).

initialize_game():-validate_insertion([[[1,b,a,0,0]],[[2,b,a,1,0]],[[3,b,a,2,1]]],[4,b,a,2,1],1,[[[1,b,a,0,0]],[[2,b,a,1,0]],[[3,b,a,2,1]]]).
