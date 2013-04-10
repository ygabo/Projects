-module(sumGapPrimes).
-export([primes/1, primes/2, do_primes/3]).
-export([par_primes/2, par_primes/3]).
-export([distribute/3, do_primes_parallel/2]).
-export([time_parallel_sieve/1]).
-export([sum_wrap/0, sum_wrap/1, sum_wrap/2, sum_wrap/3, sum_wrap/4]).
-export([gap_wrap/0, gap_wrap/1, gap_wrap/2, gap_wrap/3, gap_wrap/4]).
-export([sum/2, largest_gap/2, largest_gap_helper/2, largest_gap_seq/1, largest_gap_seq/2]).
-export([largest_gap_w/1, largest_gap_w/2, bigger_pair/4, smaller/2, bigger/2]).

% Yelnil Gabo
% 70179064
% g3d6

% 4) a.
sum( W, Key ) ->
  wtree:reduce(W,
    fun(ProcState) -> lists:sum( workers:get(ProcState, Key) ) end,
    fun(L, R) -> L+R end ).

% sum wrapper
% wrapper for sum
% creates workers, sets up the list
% and calls sum
% prints out time for sequential
% then the parallel version
sum_wrap() -> sum_wrap(100).
sum_wrap(N) -> sum_wrap(N, 10).
sum_wrap(N, P) -> sum_wrap(N,P, 'lol').
sum_wrap(N, P, Key) -> sum_wrap(2, N, P, Key). 
sum_wrap(Min, Max, P, KEY) when Min < 0 -> sum_wrap(2, Max, P, KEY);
sum_wrap(_, Max, _, _) when Max < 0 -> 0;
sum_wrap(Min, Max, _, _) when Max < Min -> 0;
sum_wrap(Min, Max, P, KEY) ->  
  List = lists:seq( Min, Max ), % list from root until max
  Distributed_List = distribute( P, List, length(List) ), % divide the list into equal parts for each worker
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ),        
  workers:broadcast( W, fun(ProcState) -> case workers:get(ProcState, KEY) of undefined -> failed;
                            X -> workers:put(ProcState, KEY, do_primes_parallel(Max, X) )
                          end
                        end ),
  AllPrimes = lists:flatten(workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end)),
  [{_,Seq},{_,_}] = time_it:t(fun() -> lists:sum(AllPrimes) end, 1.0),
  [{_,Par},{_,_}] = time_it:t(fun() -> sum( W, KEY ) end, 1.0),
  wtree:reap(W),
  io:fwrite("~f\n",[Seq]),
  io:fwrite("~f\n",[Par]).

    
% 4) c.
largest_gap( W, Key ) ->
  {X,Y,_,_} = largest_gap_helper( W, Key ),
  {X,Y}.

largest_gap_helper(W,Key) ->
  wtree:reduce(W,
    fun(ProcState) -> largest_gap_w( workers:get(ProcState, Key) ) end,
    fun([], R) -> R;
      (L, []) -> L;
      ([],[]) -> {0,0,0,0};
      (L, R) ->
      {A,B,C,D} = L,
      {E,F,G,H} = R,
      {I,J} = bigger_pair(A,B,D,G),
      {K,M} = bigger_pair(I,J,E,F),
      {K,M,C,H} end ).

% wrapper for largest gap
% creates workers, sets up the list
% and calls largest_gap
% best to call gap_wrap(N, P, KEY)
% prints out time for sequential
% then the parallel version
gap_wrap() -> gap_wrap(100).
gap_wrap(0) -> {0,0};
gap_wrap(1) -> {0,0};
gap_wrap(N) when N < 0 -> {0,0};
gap_wrap(N) -> gap_wrap(N, 10).
gap_wrap(N, P) when (N < 0) or (P < 1) -> {0,0};
gap_wrap(N, P) -> gap_wrap(N,P, 'lol').
gap_wrap(N, P, _) when (N < 0) or (P < 1) -> {0,0};
gap_wrap(N, P, KEY) -> gap_wrap(2, N, P, KEY).
gap_wrap(_, Max, P, _) when (Max < 0) or (P < 1) -> {0,0};
gap_wrap(Min, Max, P, KEY) when Min < 0 -> gap_wrap(2, Max, P, KEY);
gap_wrap(Min, Max, P, KEY) ->  
  List = lists:seq( Min, Max ), % list from root until max
  Distributed_List = distribute( P, List, length(List) ), % divide the list into equal parts for each worker
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ),        
  workers:broadcast( W, fun(ProcState) -> case workers:get(ProcState, KEY) of undefined -> failed;
                            X -> workers:put(ProcState, KEY, do_primes_parallel(Max, X) )
                          end
                        end ),
  Till = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end)),
  %misc:print(Till),
  AllPrimes = lists:flatten(Till),
  [{_,Seq},{_,_}] = time_it:t(fun() -> largest_gap_seq(AllPrimes) end, 1.0),
  [{_,Par},{_,_}] = time_it:t(fun() -> largest_gap( W, KEY ) end, 1.0),
  io:format("~p\n", [Seq]),
  io:format("~p", [Par]),
  wtree:reap(W).
  %io:fwrite("~f\n",[X]),
  %io:fwrite("~f\n",[Y]).
  
% helper
% get largest gap but with a weird twist
% return largest and smallest element of the list too
% A = smallest element of current list
% B = biggest element of current list
% C,D = pair with biggest gap
% return {C,D,A,B}
largest_gap_w([]) -> [];
largest_gap_w([X]) -> {X,X,X,X};
largest_gap_w([H|T]) -> largest_gap_w( [H|T], {H, hd(T), H, hd(T)} ).
largest_gap_w([], {X,Y,A,B}) -> {X,Y,A,B};
largest_gap_w([T], {X,Y,A,B}) ->
  F = bigger(T,B),
  {X,Y,A,F};
largest_gap_w([H|T], {X,Y,A,B}) -> 
  {C,D} = bigger_pair(H, hd(T), X, Y),
  E = smaller(H,A),
  F = bigger(H,B),
  largest_gap_w(T,{C,D,E,F}).

% helper
% return smaller number
smaller(A,B) ->
  if
    A > B -> B;
    true -> A
  end.

% helper 
% return bigger number
bigger(A,B) ->
  if
    A > B -> A;
    true -> B
  end.

% helper
% return pair with bigger gap
bigger_pair(A,B,C,D) ->
  if
    ( (B - A) >= (D - C) ) -> {A,B};
    true  -> {C,D}
  end.

% 4) d.
% sequential largest gap
largest_gap_seq([]) -> [];
largest_gap_seq([_]) -> [];
largest_gap_seq([H|T]) -> largest_gap_seq( [H|T], {H, hd(T)} ).
largest_gap_seq([], {X,Y}) -> {X,Y};
largest_gap_seq([_], {X,Y}) -> {X,Y};
largest_gap_seq([H|T], {X,Y}) ->
  if
    ( (hd(T) - H ) > (Y - X) ) -> largest_gap_seq(T, {H, hd(T)});
    true  -> largest_gap_seq(T, {X,Y})
  end.
    
%----------------------------------------------------------------------------------------------------

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

% helper
% take out all the non-prime numbers inisde BigList
do_primes_parallel(Max, BigList) ->
  M = trunc(math:sqrt(Max)), % get square root
  SmallPrimes = primes(2,M), % get all the small primes
  lists:foldl(fun(P,  L) -> lists:filter(fun(E) -> (((E  rem  P) /= 0) or (E == P))  end, L) end,
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






















  

  
