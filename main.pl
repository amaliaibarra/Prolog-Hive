:- use_module(board,
          [ initialize/0,
            available/2,
            increase_count/2
          ]).

start():-
    initialize().