:- module(utils, [remove/3]).

%True if the result of removing an element X of L is R
%remove(X,L,R):- insert(X,R,L).
remove(X,[X|Y],Y).
remove(X,[Z,V|W],[Z|A]):-remove(X,[V,W],A).