-module(primesRecursion).
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
-export([par_primes/2, par_primes/3]).
-export([distribute/3, do_primes_parallel/2]).
-export([time_parallel_sieve/1]).
%homework 2 

% 1. a)
% use a helper to compare 
max_hr([]) -> [];
max_hr([H|T]) when is_number(H) -> bigger(H, max_hr(T)).

% helper
% compare two values 
% return bigger one
bigger([],Y) -> Y;
bigger(X,[]) -> X;
bigger(X,Y) when X >= Y -> X;
bigger(X,Y) when X < Y -> Y.

% 1. b)
% figure out max of list 
% using tail recursion
max_tr([]) -> [];
max_tr([H|T]) -> max_trh([H|T],H).

% helper
% check if each element is a number(int or float)
% compare head if bigger than current max, replace max
% if not, continue
max_trh([],X) -> X; 
max_trh([H|T], X) when H >= X, is_number(H) -> max_trh(T, H);
max_trh([H|T], X) when H < X, is_number(H) -> max_trh(T,X). 

% helper
get_hrt() -> 
	LIST = [10,100,1000,10000,100000,1000000,10000000],
	lists:zip( LIST, lists:map( fun(X) -> hrt_time(X) end, LIST) ).

% helper
hrt_time(X) -> time_it:t( fun() -> 
				max_hr( lists:map( fun(_) -> random:uniform() end,
									lists:seq(1, X))  ) end).
% helper
get_trt() ->
	LIST = [10,100,1000,10000,100000,1000000,10000000],
	lists:zip( LIST, lists:map(  fun(X) -> trt_time(X) end,	LIST) ).

% helper
trt_time(X) ->
	time_it:t( fun() ->
		max_tr( lists:map( fun(_) -> random:uniform() end,
							lists:seq(1, X))  ) end).
              
%---------------------------------           

% 2. a)
% time one run of tsend(X)
% X is the length of the list
time_send(N) ->
  X = lists:map(fun(_) -> random:uniform() end, lists:seq(1, N)),
  [{_,Mean},{_,_}] = time_it:t( fun() -> tsend(X) end ),
  Mean.

% continuation of above
% run this to run time_send multiple times
multiple_time_send(N) ->
   lists:map( fun(X) -> time_send(X) end, lists:seq(0,N, 500)).

% 2. b)
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

% PARALLEL
% 3. a)
% parallel prime sieve
% create a list, subdivide it into equal parts for the workers
% broadcast the sublists to their respective workers
% have workers eliminate non-prime from their own lists
% return the remaining numbers
par_primes(Max, P) -> par_primes(1, Max, P).
par_primes(_Min, _Max, P) when P =< 0 -> [];
par_primes(Min, Max, _P) when Min > Max -> [];
par_primes(Min, Max, P) ->
  M = trunc(math:sqrt(Max)), % get square root
  List = lists:seq( max(Min, M+1), Max), % list from root until max
  SmallPrimes = primes(2,M), % get all the small primes
  Distributed_List = distribute( P, List, length(List) ), % divide the list into equal parts for each worker
  W = workers:create(P),
  workers:retrieve(W, fun(_) -> ok end), % sync
  workers:broadcast( W, fun(ProcState1, X) -> % give each worker a list to work on
                          workers:put(ProcState1, 'primes', X) end, 
                        Distributed_List),
  BigPrimes = workers:retrieve( W, fun(ProcState)-> % return all the primes from the list the workers were given
                            case workers:get(ProcState, 'primes') of
                              undefined -> failed;
                              X -> do_primes_parallel(SmallPrimes, X)
                            end
                        end),
  workers:reap(W),
  if % fix the prime list that came back
    (Min =<  2) -> lists:flatten(SmallPrimes ++  BigPrimes);
    (Min =<  M)  -> lists:flatten(lists:filter(fun(E) -> E >=  Min  end, SmallPrimes) ++  BigPrimes);
    true -> lists:flatten(BigPrimes)
  end.

% helper
% take out all the non-prime numbers inisde BigList
do_primes_parallel(SmallPrimes, BigList) ->
lists:foldl(fun(P,  L) -> lists:filter(fun(E) -> (E  rem  P)  /= 0  end, L) end,
                          BigList,
                          SmallPrimes).

% helper
% divide a list into sublists
% divide equally according to number of workers
distribute(0, _List, _) -> [];
distribute(NW, List, Length) ->
  N1 = round(Length/NW),                 % elements to give to this worker
  N2 = Length - N1,
  X = lists:sublist(List, N1),
  Y = lists:sublist(List, N1+1, Length),  % elements left for others
  [X | distribute(NW-1, Y, N2)].  % give rest of list to other workers
  
% helper
% just used for timing
time_parallel_sieve(List) ->
  lists:map( fun(X) -> time_it:t(fun() -> par_primes(1000,100000,X) end) end, List).

% SEQUENTIAL
% this is from the question 
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
                            