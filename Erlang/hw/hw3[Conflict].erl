-module(hw3).
-export([primes/1, primes/2, do_primes/3]).
-export([par_primes/2, par_primes/3]).
-export([distribute/3, do_primes_parallel/2]).
-export([time_parallel_sieve/1]).
-export([sum/0, sum/2, sum/4]).

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
%----------------------------------------------------------------------------------------------------


% 3a
sum() -> sum(100, 10).
sum(N, P) -> sum(1, N, P, 'lol'). 
sum(Min, Max, P, KEY) ->  
  M = trunc(math:sqrt(Max)), % get square root
  SmallPrimes = hw3:primes(2,M), % get all the small primes
  List = lists:seq( Min, Max ), % list from root until max
  Distributed_List = hw3:distribute( P, List, length(List) ), % divide the list into equal parts for each worker
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> 
                         workers:put(ProcState, KEY, X) end, 
                        Distributed_List),        
  workers:broadcast( W, fun(ProcState) -> 
                          case workers:get(ProcState, KEY) of
                            undefined -> failed;
                            X -> do_primes_parallel(SmallPrimes, X)
                          end
                        end ),
 % workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end ),
  wtree:reduce(W,
    fun(ProcState) -> lists:sum( workers:get(ProcState, KEY) ) end,
    fun(L,R) -> L+R end),
  wtree:reap(W).
  

%----------------------------------------------------------------------------------------------------

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

% this is taken from the question 
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

%------------------------------------------------------------------------------------------------------------






















  

  
