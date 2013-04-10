-module(hw4).
-export([distribute/3, distribute_pos/4, interest/2, interest/3]).
-export([scan_rolling/4, balance/4]).
-export([rolling_average/1, rolling_average/2, rolling_average/3]).
-export([index/2, index/3, index/4, first_last/2, first_last/3]).
-export([moving_tot/1, moving_tot/4, last_day/2, last_day/4]).
-export([rolling/1, rolling/2, rolling/4 ]).
-export([balance_wrap/1, balance_wrap/2, balance_wrap/3, balance_wrap/5 ]).
% Yelnil Gabo
% 70179064
% g3d6

% 1) a.

% index(q, A) 
index(_, []) -> undefined;
index(Q, LIST) -> index(Q, LIST, 20).
index(Q, LIST, P) -> index(Q, LIST, P, 'lol').
index(Q, LIST, P, KEY) ->  
  L = LIST,
  Pos = lists:seq( 1, length(L)),
    % divide the list into equal parts for each worker
    % this returns something like -- [ {[indices], [sublist]}, ... ]
  Distributed_List = distribute_pos( P, L, Pos, length(L) ), 
    % create the workers
  W = wtree:create(P),
    % give out the list
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ),
    % get first, last
  {X, Y} = index_reduce( W, KEY, Q ), 
  wtree:reap(W),
    % display 
  {X,Y}.
  
% reduce helper
index_reduce(W, Key, Q) ->
  wtree:reduce(W,
    fun(ProcState) -> first_last( workers:get(ProcState, Key), Q) end,
    fun(L, R) ->
      {A,B} = L,
      {C,D} = R,
      E = smaller(A,C),
      F = bigger(B,D),
      {E,F} end,
    fun(R) -> R end ).
    
% helper
% { first, last } of q 
% were given this --> {[indices], [sublist]}
first_last({[],[]}, _) -> {'+infinity', '-infinity'};
first_last({[Ti], [T]}, Q) ->
  if
    Q == T -> {Ti, Ti};
    true -> {'+infinity', '-infinity'}
  end;
first_last({[Hi|Ti], [H|T]}, Q) -> first_last( {[Hi|Ti], [H|T]}, Q, {'+infinity', '-infinity'} ).
first_last({[],[]}, _, {X,Y}) -> {X,Y};
first_last({[Ti],[T]}, Q, {X,Y}) ->
  if
    T == Q ->
      A = smaller( Ti, X ),
      B = bigger( Ti, Y ),
      {A,B};
    true -> {X,Y}
  end;
first_last({[Hi|Ti], [H|T]}, Q, {X,Y}) -> 
  if
    H == Q ->
      A = smaller(Hi, X),
      B = bigger(Hi, Y),
      first_last({Ti,T}, Q, {A,B});
    true -> first_last({Ti,T}, Q, {X,Y})
  end.

% helper
% return smaller number
smaller(A,B) ->
  if
    B == '+infinity' -> A;
    A > B -> B;
    B > A -> A;
    true -> A
  end.

% helper 
% return bigger number
bigger(A,B) ->
  if
    B == '-infinity' -> A;
    A == '-infinity' -> B;
    A > B -> A;
    true -> B
  end.
    
% helper
% divide a list into sublists
% divide equally according to number of workers
% zip with positions
distribute_pos(0, _List, _Pos, _) -> [];
distribute_pos(NW, List, Pos, Length) ->
  N1 = round(Length/NW),                 % elements to give to this worker
  N2 = Length - N1,
  A = lists:sublist(Pos, N1),
  B = lists:sublist(Pos, N1+1, Length),  % elements left for others
  X = lists:sublist(List, N1),
  Y = lists:sublist(List, N1+1, Length),  % elements left for others
  [ {A,X} | distribute_pos(NW-1, Y, B, N2)].  % give rest of list to other workers
  
  
%~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~
  
% 1) b.

% scan function for rolling average
scan_rolling(W, Key, KeyB, M) ->
  wtree:scan(W,
    fun(ProcState) -> workers:get(ProcState, Key) end,
    fun(ProcState, L ) ->
      A = workers:get(ProcState, Key),
      workers:put( ProcState, KeyB, rolling(A, [], L, M) ) end,
    fun(L,R) ->
    List = L++R,
    N = length(List) - M,
    if
        N > 0 -> lists:nthtail(N,List);
        true -> lists:nthtail(0,List)
    end end,
    []).

% rolling average overall
rolling_average(LIST) -> rolling_average(LIST, 3).
rolling_average(LIST, M) ->  rolling_average(LIST, M, 4).
rolling_average(LIST, M, P) ->  rolling_average(LIST, 'lol', 'lolb', M, P).
rolling_average(LIST, KEY, KEYB, M, P) ->  
  L = LIST,
  Distributed_List = distribute( P, L, length(L) ), % divide the list into equal parts for each worker
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ), 
  scan_rolling(W, KEY, KEYB, M-1),
  Till = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEYB) end)),
  wtree:reap(W),
  (Till).
  
% M here is different from the 
% problem description
% it is M from the descripition is M+1 in my code
rolling([]) -> [];
rolling([T]) -> [T];
rolling([H|T]) -> rolling([H|T], 2).
rolling([], _) -> [];
rolling([T], _) -> T;
rolling([H|T], M) -> rolling([H|T], [], [], M).
rolling([], X, _, _) -> X;
rolling(X, _, _, 0) -> X;
rolling([T], X, LastM, M) -> rolling([], (X ++ [(lists:sum(LastM) + T)/(M+1)] ), [], M);
% When LastM is not full, just add
rolling([H|T], X, LastM, M) when length(LastM) < M -> rolling(T, X ++ [(lists:sum( LastM ) + H)/(M+1)], LastM ++ [H], M );
% When LastM is filled, take the head out
% of the list since it's the oldest one
% and append the latest one that we just did 
% the calculations on
rolling([H|T], X, LastM, M) ->
  [_|Ti] = LastM,
  rolling(T, ( X ++ [ (lists:sum(LastM) + H)/(M+1) ] ), Ti ++ [H], M).

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

%~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~~-~-~-~-~

% 1) c

% scan function for rolling average
balance(W, Key, KeyB, Rate) ->
  wtree:scan(W,
    fun(ProcState) -> workers:get(ProcState, Key) end,
    fun(ProcState, L ) ->
      List = workers:get(ProcState, Key),
      workers:put( ProcState, KeyB, moving_tot(List, Rate, [], L )) end,
    fun(L,R) -> last_day(lists:flatten([L]++[R]), Rate) end,
    []).

% rolling average overall
% Example List
% [{1,17.42},{2,5},{3,-20},{4,1},{4,12.34},{6,-20},{7,10}]
balance_wrap(LIST) -> balance_wrap(LIST, 0.02).
balance_wrap(LIST, R) ->  balance_wrap(LIST, R, 4).
balance_wrap(LIST, R, P) ->  balance_wrap(LIST, R, 'lol', 'lolb', P).
balance_wrap(LIST, R, KEY, KEYB, P) ->  
  L = LIST,
  Distributed_List = distribute( P, L, length(L) ), % divide the list into equal parts for each worker
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ), 
  Toll = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end)),
  balance(W, KEY, KEYB, R),
  Till = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEYB) end)),
  wtree:reap(W),
  (Till).

interest(X, 0) -> X;
interest(0, _) -> 0;
interest(X, R) -> interest(X, R, 1).
interest(X, _, Days) when Days < 0 -> X;
interest(X, R, Days) -> X * math:pow( 1+R, Days).

last_day([], _) -> [];
last_day([H|A], R) ->
  {X, Y} = H,
  last_day(A, R, Y, X).
last_day([], _, Total, Day) -> {Day, Total};
last_day([T], R, Total, CurrentDay) -> 
  {X, Y} = T,
  if
    Total >= 0 -> {X, Y + interest( Total, R, X - CurrentDay )};
    true -> {X, Y + Total}
  end;
last_day([H|T], R, Total, CurrentDay) ->
  {Day, Money} = H,
  if
    Total >= 0 -> last_day( T, R, interest( Total, R, Day - CurrentDay ) + Money, Day );
    true ->  last_day( T, R, (Total + Money), Day )
  end.

moving_tot([]) -> [];
moving_tot([T]) -> [T];
moving_tot([H|T]) -> moving_tot([H|T], 0.02, [], {1,0}).
moving_tot([], _, X, Y) -> X;
moving_tot([H|T], R, X, []) ->
  {Day, Y} = H,
  moving_tot([H|T], R, X, {Day,0});
moving_tot([T], R, X, Last) -> 
  {C, D} = T,
  {E, F} = Last,
  if
    F > 0 -> A = interest( F, R, C - E);
    true -> A = F
  end,
  moving_tot([], R, (X ++ [A+D] ), {C, A+D});
moving_tot([H|T], R, X, Last) -> 
  {C, D} = H,
  {E, F} = Last,
  if
    F > 0 -> A = interest( F, R, C - E);
    true -> A = F
  end,
  moving_tot(T, R, (X ++ [A+D]), {C, A+D}).



  

  
