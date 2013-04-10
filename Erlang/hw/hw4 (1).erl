-module(hw4).
-export([distribute/3, distribute_pos/4]).
-export([index_reduce/3, largest_gap_seq/1, largest_gap_seq/2]).
-export([smaller/2, bigger/2]).
-export([randomlist/2, randomlist/3 ]).
-export([index/2, index/3, index/4, first_last/2, first_last/3]).

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
  Till = (workers:retrieve(W, fun(ProcState) -> workers:get(ProcState, KEY) end)),
  %misc:print(Till),
  S = index_reduce( W, KEY, Q ), 
  if
    S == undefined -> X = undefined;
    true -> X = S
  end,
  wtree:reap(W),
    % display 
  X.
  
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
    fun(R) -> 
      {X,_} = R,
      if
        X == undefined -> undefined;
        true -> R
      end
    end ).
  
% helper
% { first, last } of q 
% were given this --> {[indices], [sublist]}
first_last({[],[]}, _) -> {undefined, undefined};
first_last({[Ti], [T]}, Q) ->
  if
    Q == T -> {Ti, Ti};
    true -> {undefined, undefined}
  end;
first_last({[Hi|Ti], [H|T]}, Q) -> first_last( {[Hi|Ti], [H|T]}, Q, {undefined, undefined} ).
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
    B == undefined -> A;
    A > B -> B;
    B > A -> A;
    true -> A
  end.

% helper 
% return bigger number
bigger(A,B) ->
  if
    B == undefined -> A;
    A == undefined -> B;
    A > B -> A;
    true -> B
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
  











  

  
