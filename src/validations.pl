:- module(validations,
          [ pilled/2,
            validate_grasshoper_move/4
          ]).

:- use_module(predicates, [tile/5]).

pilled(X, Y) :-
    findall(L,
            tile(_, _, X, Y, L),
            Bag), %Check if it's pilled
    write(Bag),
    max_list(Bag, MaxLevel),
    MaxLevel>=1. %Bag has at least two elements
validate_grasshoper_move(X1, Y1, X2, Y2) :-
    (   exists_line(X1, Y1, X2, Y2, -1, 0), !
    ;   exists_line(X1, Y1, X2, Y2, 0, -1), !
    ;   exists_line(X1, Y1, X2, Y2, 1, -1), !
    ;   exists_line(X1, Y1, X2, Y2, 1, 0), !
    ;   exists_line(X1, Y1, X2, Y2, 0, 1), !
    ;   exists_line(X1, Y1, X2, Y2, -1, 1), !
    ).

exists_line(X1, Y1, X2, Y2, _, _) :-
    X1=:=X2,
    Y1=:=Y2.

exists_line(X1, Y1, X2, Y2, A1, A2) :-
    tile(_, _, X1, Y1, _),
    NX1 is X1+A1,
    NY2 is Y1+A2,
    exists_line(NX1, NY2, X2, Y2, A1, A2).