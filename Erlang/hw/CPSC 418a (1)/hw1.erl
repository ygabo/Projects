-module(hw1).
-export([collapse/1]).
-export([headof/1]).
-export([is_bitonic/1]).
-export([count_peaks/1]).
-export([increasing/1]).
-export([decreasing/1]).
-export([nsum/2]).
-export([nmul/2]).
-export([listmag/1]).
-export([sum_first_n/1]).
-export([checkzeroes/1]).
-export([multh/3]).
-export([multh_all/3]).
-export([com_add/2]).

% YELNIL GABO
% 70179064
% g3d6

% 1. a)
% collapse numbers that are equal and next to each other
% [1,1,1,1,2,2,2,2,3,3,3,3] --> [1,2,3]
% [1,1,1,2,2,2,1,1,1] --> [1,2,1]
collapse([]) -> [];
collapse([H|T]) ->
	case (H =:= headof(T)) of
		true -> collapse(T);
		_Else -> [H | collapse(T)]
	end.

% helper
% return head of a list
headof([]) -> [];
headof([H|_]) -> H.

% 1. b)
% see if list goes like /\ or \/
% increasing then decreasing
% or decreasing then increasing
% bitonic only if theres one peak or valley
is_bitonic([]) -> true;
is_bitonic( N ) -> count_peaks( collapse(N) ) == 1.

% helper
% count number of peaks
count_peaks([]) -> 0;
count_peaks([H|T]) -> 
	case (H < headof(T)) of
		true -> increasing(T);
		_Else -> decreasing(T)
	end.

% helper
% if top found add 1 to count 
% then keep going (to check if it keeps going down
% after that
increasing([]) -> 0 ;
increasing([_H|T]) when T == [] -> 0;
increasing([H|T]) -> 
	case (H < headof(T)) of
		true -> increasing(T);
		_Else -> 1 + decreasing(T)
	end.

% helper
% if bottom found, add 1 to count
% then keep going
decreasing([]) -> 0 ;
decreasing([_H|T]) when T == [] -> 0;
decreasing([H|T]) -> 
	case (H > headof(T)) of
		true -> decreasing(T);
		_Else -> 1 + increasing(T)
	end.

% 2. a)
% -- do addition on reversed lists
% 	since when you add, you start from 
% 	the right going to the left
% -- check to see if any digits need
%	to be carried over
% -- check to see if there are any
% 	leading zeroes, if so, remove them
nsum(X,Y) -> checkzeroes( 
				lists:reverse(
			 checkcarry(
			 nsumrev(
				lists:reverse(X),
				lists:reverse(Y))))).

% helper to help add numbers 
% starting from the original number's
% back side
nsumrev([],[]) -> [];
nsumrev(X,[]) -> X;
nsumrev([],X) -> X;
nsumrev([XH|XT],[YH|YT])
	when XH+YH < 10 -> [XH+YH] ++ nsumrev(XT, YT);
nsumrev([XH|XT],[YH|YT])
	when XH+YH >= 10 -> [((XH+YH) rem 10)] ++ nsumcarry(XT, YT).

% same as above
% call this guy if there needs to be a carry
nsumcarry([],[]) -> [1];
nsumcarry([X|_T],[]) -> [X+1|_T];
nsumcarry([],[X|_T]) -> [X+1|_T];
nsumcarry([XH|XT],[YH|YT])
	when XH+YH+1 < 10 -> [XH+YH+1] ++ nsumrev(XT, YT);
nsumcarry([XH|XT],[YH|YT])
	when XH+YH+1 >= 10 -> [((XH+YH+1) rem 10)] ++ nsumcarry(XT, YT).

% helper
% checks the final result if theres still work
% that needs to be done in regards to carrying
% over 
checkcarry([]) -> [];
checkcarry([H|T]) when (T == []), (H >= 10) -> [(H rem 10), 1]; 
checkcarry([H|T]) when (H < 10)	-> [ H ] ++ checkcarry(T);
checkcarry([H|[X|T]]) when H >= 10 -> [H rem 10] ++ checkcarry([X+1|T]).

% helper
% check leading zeroes in final
% answer
checkzeroes([]) -> [];
checkzeroes([H|T]) when T == [] -> [H];
checkzeroes([H|T]) when H /= 0 -> [H|T];
checkzeroes([H|T]) when H == 0 -> checkzeroes(T).

% 2. b)
% multiply
nmul([],[]) -> [];
nmul([],X) -> X;
nmul(X,[]) -> X;
nmul( X, Y ) ->
	A = listmag(X),
	com_add( multh_all( A, Y, [] ), [] ).

% nmul can also be implemented
% like so:
%nmul( X, Y ) ->
%	A = listmag(X),
%	lists:foldl( 
%		fun(T, Sum) -> nsum(T, Sum) end, [],
%			lists:map(
%				fun(R)-> multh( R, Y,[] ) end, A ) ). 

% helper
% multiply two numbers together
% by adding Y, X number of times, to itself
multh(0,_,N) -> N;
multh(X,Y,N) -> multh(X-1, Y, nsum(Y, N)). 

% helper
% make a list of all the results
% after breaking up X into pieces
% and multiplying it to Y
% eg. [1,2] * [3,4]
%     break up [1,2] to 1 and 2
%     would get [[6,8],[1,2]] since
%     we multiplied 1 to [3,4] and 2 to [3,4]
multh_all([],_,N) -> N;
multh_all(_,[],_N) -> [0];
multh_all( [H|T],Y,N) -> multh_all(T,Y, [multh(H,Y,[]) | N] ). 

% helper
% this will be given a list of lists
% a list is a number and theres a bunch of
% numbers to be added together
com_add([], Y) -> Y;
com_add([X|T], Y) -> com_add(T, nsum(X,Y)).

% helper
% magnitudes of 10
magnitude([])-> 1;
magnitude(0) -> 1;
magnitude( N ) -> 10* magnitude( N-1).

% helper
% convert a list of elements to their proper magnitudes
% their place in the list determines their magnitude
% eg [1,2,1] -> [100, 20, 1]
%	 [3,2,4,5] ->[3000, 200, 40, 5]
listmag([])-> [];
listmag([H|T]) -> [H*magnitude(length(T))] ++ listmag(T).

% 3.
% -- spawns N processes 
% 	each process sends message to main process
% -- message contains two numbers {i, i*2}
% -- i is the number of the process
% -- main process totals up all the numbers sent to it
sum_first_n(N) ->
	MyPid = self(),
	X = sum_first_nh(N, MyPid),
	lists:sum(X).
	
% helper
% that spawns all the processes
sum_first_nh(0,_) -> [0];
sum_first_nh(N, MyPid) ->
	Y = sum_first_nh(N-1, MyPid),
	spawn( fun() -> MyPid !
		{countN, [N, N*2]} end ),
	receive
		{countN, X} -> X ++ Y
	end.

