% initialize_board(B):-B is [].

%checks wether or not a given bug can be added to a given board's column  
validate_insertion_xcol([], [_Id,_Color,_Bug,_X,_Y]).

validate_insertion_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|Z], [Id,Color,Bug,X,Y]):- 
    Yi < Y,!,
    validate_insertion_xcol(Z, [Id,Color,Bug,X,Y]).

validate_insertion_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|_Z], [_Id,_Color,_Bug,_X,Y]):- 
    Yi > Y.

%checks wether or not a given bug can be added to the board
validate_insertion([],[_, Color, _Bug, X, Y], IN, Board):- 
    write("No se hizo nadaaa\n"),
    check_neighbors(X,Y,Color,Board,IN). 
validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]|_R], [Id,Color,Bug,X,Y],IN,Board) :-
    X =:= Xi,!,
    validate_insertion_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y]),
    check_neighbors(X,Y,Color,Board,IN). 
validate_insertion([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|R], [Id,Color,Bug,X,Y],IN,Board) :- 
    write("Xi< X\n"),
    Xi < X,
    !,
    validate_insertion(R,[Id, Color,Bug, X,Y],IN,Board).

validate_insertion([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|_R], [_Id,Color,_Bug,X,Y],IN,Board) :- 
    write("Xi > X\n"),
    Xi > X,
    check_neighbors(X,Y,Color,Board,IN). 

set_true(F):- F = true.
set_false(F):- F = false.


%compare bugs colors x column
check_colour_xcol([], [_Color,_X,_Y],F):- 
    set_false(F).

check_colour_xcol([[_Idi,Color,_Bugi,_Xi,Yi]|_Z], [Color,_X,Y],F):- 
    Y =:= Yi,!,
    set_true(F).

check_colour_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|Z], [Color,X,Y],F):- 
    Yi < Y,!, 
    check_colour_xcol(Z, [Color,X,Y],F).

check_colour_xcol([[_Idi,_Colori,_Bugi,_Xi,Yi]|_Z], [_Color,_X,Y],F):- 
    Yi > Y,
     set_false(F).

%ccompare bug's colors
check_colour([],[_Color,_X,_Y],F):-
    set_false(F). %First Insertion

check_colour([[[Idi,Colori,Bugi,Xi,Yi]|Z]|_R], [Color,X,Y],F) :-
    X =:= Xi,
    !,
    check_colour_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Color,X,Y],F). 

check_colour([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|R], [Color,X,Y],F) :- 
    Xi < X,
    !,
    check_colour(R,[Color, X,Y],F).

check_colour([[[_Idi,_Colori,_Bugi,Xi,_Yi]|_Z]|_R], [_Color,X,_Y],F) :- 
    Xi > X,
    set_false(F). 


same_colors(X,Y,C,B,F):- check_colour(B,[C,X,Y],F).

%check if any of the tile's neighbors has a different colour
%Insert second tile in the game (first one for the second player) 
check_neighbors(Xi,Yi,C,B,IN):-
     IN = 1,
     !,
     write("Checking neighbors case 1\n"),
     B = [[_E]],
     (not(same_colors(Xi-1,Yi,C,B,FA1)), var(FA1),!;
      not(same_colors(Xi-1,Yi+1,C,B,FA2)), var(FA2),!;
      not(same_colors(Xi,Yi-1,C,B,FA3)), var(FA3),!;
      not(same_colors(Xi,Yi+1,C,B,FA4)), var(FA4),!;
      not(same_colors(Xi+1,Yi-1,C,B,FA5)), var(FA5),!;
      not(same_colors(Xi+1,Yi,C,B,FA6)), var(FA6)).

%Insert other tile
check_neighbors(Xi,Yi,C,B,_):- 
    write("Checking neighbors case 2\n"),
    same_colors(Xi-1,Yi,C,B,FA1),
    same_colors(Xi-1,Yi+1,C,B,FA2),
    same_colors(Xi,Yi-1,C,B,FA3),
    same_colors(Xi,Yi+1,C,B,FA4),
    same_colors(Xi+1,Yi-1,C,B,FA5),
    same_colors(Xi+1,Yi,C,B,FA6),
    (write("x pasar FA1\n"),FA1,!; FA2,write("x pasar FA1\n"),!; FA3,write("x pasar FA1\n"),!; FA4,write("x pasar FA1\n"),!; FA5,write("x pasar FA1\n"),!; FA6,write("x pasar FA1\n"),!).


%A LA PRIMERA FICHA DEL JUEGO SE LE DEVUELVE TRUE Y YA, pQ SIEMPRE ES POSIBLE INSERTARLA INDEPENDIENTEMENTE DE DONDE LO HAGA

add_tile_xcol([], [Id, Color, Bug, X, Y],[[Id, Color, Bug, X, Y]]).

add_tile_xcol([[ Idi, Colori, Bugi, Xi,Yi]|Z], [Id,Color,Bug,X,Y],[[ Idi, Colori, Bugi, Xi,Yi]|R]):- 
    Yi < Y,!,
    add_tile_xcol(Z, [Id,Color,Bug,X,Y], R).

add_tile_xcol([[Idi, Colori, Bugi, Xi,Yi]|Z], [Id, Color, Bug, X,Y],[[Id, Color, Bug, X,Y],[Idi, Colori, Bugi, Xi,Yi]|Z] ):- 
    Yi > Y.

%checks wether or not a given bug can be added to the board
add_tile([],[Id,Color,Bug,X,Y], [[[Id,Color,Bug,X,Y]]]). %First Insertion

add_tile([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y], [Row|R]) :-
    X =:= Xi,!,
    add_tile_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y], Row).

add_tile([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y],[[[Idi,Colori,Bugi,Xi,Yi]|Z]|W]) :- 
    Xi < X,
    !,
    add_tile(R,[Id, Color,Bug, X,Y], W).

add_tile([[[Idi, Colori, Bugi, Xi, Yi]| Z]| R], [Id, Color, Bug, X, Y],[[[Id, Color, Bug, X, Y]],[[Idi, Colori, Bugi, Xi, Yi]|Z]|R]) :- 
    Xi > X.


insert( [Id, Color, Bug]):- 
    write("Insert first tile\n"),
    add_tile([], [Id, Color, Bug, 0, 0], NewBoard), write(NewBoard).

insert( [Id, Color, Bug, X, Y], [[E]]):- 
    !,
    write("Insert second tile\n"),
    validate_insertion([[E]], [Id, Color, Bug, X, Y], 1, [[E]]), 
    add_tile([[E]], [Id, Color, Bug, X, Y], NewBoard), write(NewBoard).

insert( [Id, Color, Bug, X, Y], Board):- 
    write("Insert other tile\n"),
    validate_insertion(Board, [Id, Color, Bug, X, Y], 2, Board), 
    write("Insertion validated \n"),
    add_tile(Board, [Id, Color, Bug, X, Y], NewBoard), write(NewBoard).


% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board([Id,Color,Bug,X,Y,First]).

% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board(board,[-1,-1],[Id,Color,Bug,X,Y,First]).

initialize_game():-validate_insertion([[[1,b,a,0,0]],[[2,b,a,1,0]],[[3,b,a,2,1]]],[4,b,a,2,1],1,[[[1,b,a,0,0]],[[2,b,a,1,0]],[[3,b,a,2,1]]]).
