%Yelnil Gabo, 70179064, g3d6


%---1---
shuffle([],[],[]).
shuffle([X|T1],[Y|T2],[X,Y|Z]) :- shuffle(T1,T2,Z).

%---2---
double([],[]).
double([X|Xs],[X,X|Ys]) :- double(Xs,Ys).


%---3---
no_duplicates([], []).
no_duplicates([X|Xs], Ys) :- member(X, Xs),no_duplicates(Xs, Ys).
no_duplicates([X|Xs], [X|Ys]) :- nonmember(X, Xs),no_duplicates(Xs, Ys).

member(X,[X|Xs]).
member(X,[Y|Ys]) :- member(X,Ys).

nonmember(_, []).
nonmember(X, [Y|Ys]) :- X\==Y, nonmember(X, Ys).


%---4---
delete_one(X,[X|Xs],Xs).
delete_one(X,[Y|Ys],[Y|Z]) :- delete_one(X,Ys,Z).

same_elements([],[]).
same_elements([X|Xs],Ys) :- delete_one(X,Ys,Zs),same_elements(Xs,Zs).