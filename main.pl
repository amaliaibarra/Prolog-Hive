initialize_board(B):-B is [].

%checks wether or not a given bug can be added to a given board's column  
validate_insertion_xcol([], [Id,Color,Bug,X,Y]).
validate_insertion_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y]):- Yi < Y,!, write("Yi<Y"), write(Yi), validate_insertion_xcol(Z, [Id,Color,Bug,X,Y]).
validate_insertion_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y]):- Yi > Y, write("Yi>Y"), write(Yi).

%checks wether or not a given bug can be added to the board
validate_insertion([],[Id,Color,Bug,X,Y],IN). %First Insertion

% validate_insertion([[E1]],[Id,Color,Bug,X,Y]). %Second Insertion

validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y],IN) :- X =:= Xi,!, write("Xi=X"), write(Xi), validate_insertion_xcol([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y]),check_neighbours(). 

validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y],IN) :- Xi < X,!, write("Xi<X"),write(Xi),validate_insertion(R,[Id, Color,Bug, X,Y]).

validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y],IN) :- Xi > X, write("Xi>X"), write(Xi), check_neighbors(). 

%check if any of the tile's neighbors has a different colour
check_neighbors(Xi,Yi,C,B).

% validate_insertion([[[Idi,Colori,Bugi,Xi,Yi]|Z]], [Id,Color,Bug,X,Y], [[Id,Color,Bug,X,Y]]) :- X =\= Xi.

% get_row([[[Idi,Colori,Bugi,Xi,Yi]|Z]|R], [Id,Color,Bug,X,Y], [[Idi,Colori,Bugi,Xi,Yi]|Z]) :- X =:= Xi. 


% get_column([[Idi,Colori,Bugi,Xi,Yi]|Z],[Id, Color,Bug, X,Y]).
% validate_insertion([[Idi,Colori,Bugi,Xi,Yi]|Z],[Id, Color,Bug, X,Y])

% update_board(B, [Id,Color,Bug,X,Y,First]).

% method():- write("Term"),readln(Param1).




%board_row, Tile to add, result
add_tile([], [Id,Color,Bug,X,Y],[Id,Color,Bug,X,Y]).
add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],[[Idi,Colori,Bugi,Xi,Yi]|R]):- Yi < Y, add_tile(Z, [Id,Color,Bug,X,Y],R).
add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],[[Id,Color,Bug,X,Y]|[[Idi,Colori,Bugi,Xi,Yi]|Z]]):- Yi > Y.
% add_tile([[Idi,Colori,Bugi,Xi,Yi]|Z], [Id,Color,Bug,X,Y],L):- Yi > Y, L is[[Id,Color,Bug,X,Y]|Z].



% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board([Id,Color,Bug,X,Y,First]).

% insert([Id,Color,Bug,X,Y,First]):- validate_insertion([Id,Color,Bug,X,Y,First]), update_board(board,[-1,-1],[Id,Color,Bug,X,Y,First]).
