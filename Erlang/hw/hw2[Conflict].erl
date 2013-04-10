-module(hw2).
-export([max_hr/1]).
-export([max_tr/1]).
-export([max_trh/2]).
-export([bigger/2]).
-export([get_hrt/0]).
-export([get_trt/0]).
-export([hrt_time/1]).
-export([trt_time/1]).
-export([tsend/1, child1proc/1, child2proc/2]).
-export([time_send/1, multiple_time_send/1]).
-export([primes/1, primes/2, do_primes/3]).
-export([par_primes/2]).
-export([distribute/3]).
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
	lists:zip( LIST, lists:map( fun(X) -> hrt_time(X) end, LIST) ).

hrt_time(X) -> time_it:t( fun() -> 
				max_hr( lists:map( fun(_) -> random:uniform() end,
									lists:seq(1, X))  ) end).

get_trt() ->
	LIST = [10,100,1000,10000,100000,1000000,10000000],
	lists:zip( LIST, lists:map(  fun(X) -> trt_time(X) end,	LIST) ).
			
trt_time(X) ->
	time_it:t( fun() ->
		max_tr( lists:map( fun(_) -> random:uniform() end,
							lists:seq(1, X))  ) end).
              
%---------------------------------           
   
time_send(N) ->
  X = lists:map(fun(_) -> random:uniform() end, lists:seq(1, N)),
  time_it:t( fun() -> tsend(X) end ).

% 1..N
multiple_time_send(N) ->
  lists:map( fun(X) -> time_send(X) end, lists:seq(0,N, 1000000)).

tsend(X) ->
	OtherPid = spawn( hw2, child1proc, [X]), 
	spawn( hw2, child2proc, [X, OtherPid] ),
	ok.

child1proc(X) -> 
	receive 
		{child2, OtherPid} ->
      max_tr(X),
			OtherPid ! child1
	end.
	
child2proc(X, OtherPid) -> 
	OtherPid ! {child2,self()},
  max_tr(X),
	receive 
		child1 -> ok
	end.
  
%---------------------------------     

% SEQUENTIAL
primes(Lo, Hi) when  is_integer(Lo) and  is_integer(Hi) and  (Lo >  Hi) -> [];
primes(Lo, Hi) when  is_integer(Lo) and  is_integer(Hi) and  (Hi <  5) ->
  lists:filter(fun(E) -> (Lo =<  E)  and  (E  =<  Hi) end, [2,3]);
primes(Lo, Hi) when  is_integer(Lo) and  is_integer(Hi) and  (Lo =<  Hi) ->
  M = trunc(math:sqrt(Hi)),
  SmallPrimes =  primes(2,  M),
  BigPrimes =  do_primes(SmallPrimes,  max(Lo, M+1), Hi),
  if
    (Lo =<  2) -> SmallPrimes ++  BigPrimes;
    (Lo =<  M)  -> lists:filter(fun(E) -> E >=  Lo  end, SmallPrimes) ++  BigPrimes;
    true -> BigPrimes
  end.

primes(N) -> primes(1,N).  %  a simple  default

%   do primes(SmallPrimes, Lo, Hi) the elements of [Lo, ..., Hi] that are not divisible
%             by any element of SmallPrimes.
do_primes(SmallPrimes, Lo, Hi) ->
lists:foldl(fun(P,  L) -> lists:filter(fun(E) -> (E  rem  P)  /= 0  end, L) end,
                          lists:seq(Lo, Hi),
                          SmallPrimes).
                          
% PARALLEL
par_primes(Max, P) ->
  W = workers:create(P),
  ok.

% {workerpool, #of workers}
% N is number of elements
% NW is number of workers
% Key is for retrieval
distribute({[], 0}, List, _Key) -> List;
distribute({[W_head | W_tail], NW}, List, Key) ->
  N = length(List),
  N1 = round(N/NW),                 % elements to give to this worker
  X = lists:sublist(List, N1),
  Y = lists:sublist(List, N+1, N),  % elements left for others
  io:format("The value is: ~p.", [X]),
  distribute({W_tail, NW-1}, Y, Key).  % give rest to other workers

