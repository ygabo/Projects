-module(scan).
-export([distribute/3, distribute_pos/4]).
-export([scan_rolling/4, largest_gap_seq/1, largest_gap_seq/2]).
-export([largest_gap_w/1, largest_gap_w/2, bigger_pair/4, smaller/2, bigger/2]).
-export([ randomlist/2, randomlist/3 ]).
-export([rolling_average/1, rolling_average/2, rolling_average/3, moving_tot/1, moving_tot/3, rolling/1, rolling/2, rolling/4 ]).
% Yelnil Gabo
% 70179064
% g3d6

% 1) a.

% scan function for rolling average
scan_rolling(W, Key, KeyB, M) ->
  wtree:scan(W,
    fun(ProcState) -> workers:get(ProcState, Key) end,
    fun(ProcState, L ) ->
      List = workers:get(ProcState, Key),
      N = length(L) - M,
      if
        N > 0 -> workers:put( ProcState, KeyB, rolling(List, [], lists:nthtail(N,L), M) );
        true -> workers:put( ProcState, KeyB, rolling(List, [], lists:nthtail(0,L), M) )
      end end,
    fun(L,R) -> L++R end,
    []).

% rolling average overall
rolling_average(LIST) -> rolling_average(LIST, 3).
rolling_average(LIST, M) ->  rolling_average(LIST, M, 4).
rolling_average(LIST, M, P) ->  rolling_average(LIST, 'lol', 'lolb', M, P).
rolling_average(LIST, KEY, KEYB, M, P) ->  
  L = LIST,%[1,2,3,4,1,2,3,4,1,2,3,4],
  misc:print(L),
  %List = randomlist(10,10), % list from root until max
  %Pos = lists:seq( 1, length(L)),
  Distributed_List = distribute( P, L, length(L) ), % divide the list into equal parts for each worker
  %Distributed_Pos = distribute( P, Pos, length(Pos) ),
  %Combined_list_with_pos = lists:zip(Distributed_Pos,Distributed_List),
  W = wtree:create(P),
  workers:broadcast( W, fun(ProcState, X) -> workers:put(ProcState, KEY, X) end, Distributed_List ), 
  Toll = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end)),  
  scan_rolling(W, KEY, KEYB, M-1),
  Till = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEYB) end)),
  wtree:reap(W),
  (Till).
  
moving_tot([]) -> [];
moving_tot([T]) -> [T];
moving_tot([H|T]) -> moving_tot([H|T], [], 0).
moving_tot([], X, _) -> X;
moving_tot([H|T], X, Last) -> moving_tot(T, (X ++ [Last+H]), Last+H).

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
% the calc on
rolling([H|T], X, LastM, M) ->
  [_|Ti] = LastM,
  rolling(T, ( X ++ [ (lists:sum(LastM) + H)/(M+1) ] ), Ti ++ [H], M).

  
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

% random list
% L is length
% 0 to N
randomlist(L, N) when L > 0, N > 0 ->
randomlist(L, N, random:seed0()).

randomlist(It, N, S) when It > 0 ->
{Num,S1} = random:uniform_s(N, S),
[Num | randomlist(It-1, N, S1)];
randomlist(0, _, _) ->
[].

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
  













  

  
