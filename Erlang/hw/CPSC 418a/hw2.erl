-module(hw2).
-export([max_hr/1]).
-export([max_tr/1]).
-export([max_trh/2]).
-export([bigger/2]).
-export([get_hrt/0]).
-export([get_trt/0]).
-export([hrt_time/1]).
-export([trt_time/1]).
%homework 2 

max_hr([]) -> [];
max_hr([H|T]) -> bigger(H, max_hr(T)).

bigger([],Y) -> Y;
bigger(X,[]) -> X;
bigger(X,Y) when X >= Y -> X;
bigger(X,Y) when X < Y -> Y.

max_tr([]) -> [];
max_tr([H|T]) -> max_trh([H|T],H).

max_trh([],X) -> X;
max_trh([H|T], X) when H >= X -> max_trh(T, H);
max_trh([H|T], X) when H < X -> max_trh(T,X). 

get_hrt() -> 
	LIST = [10,100,1000,10000,100000,1000000,10000000],
	lists:zip( LIST, 
				lists:map(  fun(X) -> 
							hrt_time(X) end,
						LIST) ).

hrt_time(X) ->
	time_it:t( fun() ->
		max_hr( lists:map(
			fun(_) -> 
				random:uniform() end,
		lists:seq(1, X))  ) end).

get_trt() ->
	LIST = [10,100,1000,10000,100000,1000000,10000000],
	lists:zip( LIST, 
				lists:map(  fun(X) -> 
							trt_time(X) end,
						LIST) ).
			
trt_time(X) ->
	time_it:t( fun() ->
		max_tr( lists:map(
			fun(_) -> 
				random:uniform() end,
		lists:seq(1, X))  ) end).