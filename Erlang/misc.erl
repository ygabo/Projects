%% =====================================================================
%% @copyright 2011 Mark R. Greenstreet
%% @author Mark R. Greenstreet <mrg@cs.ubc.ca>
%% @end
%% =====================================================================
%%
%% @doc misc - an assortment of various functions that I've found to
%%   be useful.
%% @end

-module misc.
-export [arity/1, cumsum/1, cumsum/2, index/2, logsteps/2, pow/2].
-export [neighbours/1, print/1, print/2, pr_tl/3].
-export [rlist/1, rlist/2, rlist/3, msg_dump/1, msg_dump/2].
-export [sync/1, sync/2, sync/3, sync/4].

% internal functions, uncomment for debbuging
% -export([rp_pow/3, how_many_runs/3]).

%% @spec arity(Fun) -> integer()
%% @doc Return the arity of <code>Fun</code>.
%%   If <code>Fun</code> is not a function, then the atom
%%   <code>'false'</code> is returned.
arity(F) when is_function(F) -> arity(F,0);
arity(_) -> false.

% arity/2: a helper function for arity/1.  Try successive values for the
% arity until we find the right one.
arity(F,N) when is_function(F,N) -> N;
arity(F,N) -> arity(F, N+1).

%% @spec cumsum(List) -> [number()]
%%   List = [number()]
%% @doc
%%   Return a list where the <code>N</code><sup>th</sup> element of
%%   the result is the sum of the first <code>N</code> elements of
%%   <code>List</code>.  Corresponds to the cumsum function from
%%   Matlab.
cumsum(L) -> cumsum(0, L).

%% @spec cumsum(Acc0, List) -> [number()]
%%   List = [number()]
%% @doc
%%   Return a list where the <code>N</code><sup>th</sup> element of
%%   the result is the <code>Acc0</code> plus the sum of the first
%%   <code>N</code> elements of <code>List</code>.
cumsum(Acc0, L) -> element(1, lists:mapfoldl(
  fun(Elem, Acc) -> S = Elem+Acc, {S,S} end, Acc0, L)).

%% @spec pow(N::number(), M::number()) -> number()
%% @doc <code>N</code> raised to the <code>M</code><sup>th</sup> power.
%%    <ul>
%%      <li>
%%        If <code>M</code> is an integer, then <code>N<sup>M</sup></code>
%%        is computed using the Russian Peasant algorithm with
%%	  <it>O(</it>log <code>N</code><it>)</it> multiplications.
%%        This produces an exact answer when <code>N</code> is an integer.
%%        Furthermore, it avoids an exception when
%%	  <code>N <it>&#60; 0</it></code>.
%%      </li>
%%      <li>
%%        Otherwise, <code>M</code> must be a float, in which case
%%        <code>N<sup>M</sup></code> is computed using
%%        <a href="http://www.erlang.org/doc/man/math.html#pow-2"><code>math:pow/2</code></a>.
%%      </li>
%%    </ul>
%%    If <code>N <it>=0</it></code> and <code>M <it>&#60; 0</it></code>,
%%    then the atom <code>'undefined'</code> is returned.
%%    If <code>N <it>&#60; 0</it></code> and <code>M</code> is a float, then
%%    an arithmetic exception occurs.  Likewise, there is an exception when
%%    <code>M</code> is a float and the <code>N<sup>M</sup></code>
%%    produces a floating point overflow.
pow(N, M) when (is_integer(N) and is_integer(M) and (M >= 0)) ->
  rp_pow(N, M, 1);
pow(0, M) when (M < 0) -> undefined;
pow(N, M) when (M < 0) -> 1 / pow(N, -M);
pow(N, M) when (is_number(N) and (N < 0) and is_integer(M)) ->
  case (M rem 2) of
    0 -> pow(-N, M);
    1 -> -pow(-N, M)
  end;
pow(N, M) when (is_number(N) and (N > 0) and is_number(M)) ->
  math:pow(N,M);
pow(L, M) when (is_list(L) and is_number(M)) -> [ pow(N, M) || N <- L];
pow(N, L) when (is_number(N) and is_list(L)) -> [ pow(N, M) || M <- L];
pow(LN, LM) when (is_list(LN) and is_list(LM)) ->
  [ pow(N, M) || {N, M} <- lists:zip(LN, LM) ].


% rp_pow: integer power, "Russian Peasant" algorithm
% P*N^M is invariant for each recursive call.
rp_pow(_N, 0, P) -> P;
rp_pow(N, M, P) when (M rem 2 == 0) -> rp_pow(N*N, M div 2, P);
rp_pow(N, M, P) when (M rem 2 == 1) -> rp_pow(N*N, M div 2, P*N).


%% @spec index(Key, List) -> integer() | false
%% @doc Return the position of the first occurrence of <code>Key</code>
%%   in <code>List</code>.
%%   If <code>List</code> has no element that matches <code>Key</code>,
%%   then false is returned.
index(Key, List) -> index(Key, List, 1).
index(_Key, [], _N) -> 'false';
index(Key, [Key | _T], N) -> N;
index(Key, [_Other | T], N) -> index(Key, T, N+1).


%% @spec logsteps(Mant, Exp) -> [ number() ]
%%    Mant = [ number() ],
%%    Exp = [ number() ]
%% @doc
%%   For each <code>M</code> in <code>Mant</code> and each <code>E</code> in
%%   <code>Exp</code>, <code>M*10<sup>E</sup></code> is an element of
%%   the return list.
%%   <br/>
%%   Example: <code>logsteps([1,2,5], lists:seq(-1,2) -> [0.1, 0.2, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500]. </code>
%%   <br/>
%%   <code>logsteps</code> can be handy for generating a sequence of
%%   problem sizes for run-time measurements.
logsteps(Mant, Exp) -> [ M*pow(10,E) || E <- Exp, M <- Mant ].


%% @spec neighbours(List) -> TupleList
%% @doc
%%   Return a list of tuples of all adjacent pairs of elements of list.
%%   <br/>
%%   Example: <code>neighbours([1,2,3,5,7,11]) -> [{1,2},{2,3},{3,5},{5,7},{7,11}].</code>
%%   <br/>
%%   If <code>List</code> is empty or a singleton, then <code>TupleList</code>
%%   is empty.
neighbours([]) -> [];
neighbours([H | T]) ->
  element(1, lists:mapfoldl(fun(E, Prev) -> {{Prev, E}, E} end, H, T)).


%% @spec rlist(N, M, State0) -> {List, State1}
%%   N = integer(),
%%   M = integer(),
%%   State0 = ran(),
%%   List = [ integer() ],
%%   State1 = ran()
%% @doc Return a list of N elements where each is chosen randomly from 1..M
%%   using State0 as the starting state for the pseudo-random number generator.
%%   rlist returns the random list and the state for the PRNG at the end of
%%   generating the list.
rlist(N, M, State0)
    when is_integer(N) and is_integer(M) and (N > 0) and (M > 0) ->
  rlist2(N, M, [], State0).

rlist2(0, _M, Rlist, State) -> {Rlist, State};
rlist2(N, M, Rlist, State0) ->
  {Rnum, State1} = random:uniform_s(M, State0),
  rlist2(N-1, M, [Rnum | Rlist], State1).


%% @spec rlist(N, M) -> List
%%   N = integer(),
%%   M = integer(),
%%   List = [ integer() ]
%% @doc Return a list of N elements where each is chosen randomly from 1..M.
rlist(N,M) when is_integer(N) and is_integer(M) and (N > 0) and (M > 0) ->
  rlist2(N, M, []).

rlist2(0, _M, Rlist) -> Rlist;
rlist2(N,  M, Rlist) -> rlist2(N-1, M, [random:uniform(M) | Rlist]).

%% @spec rlist(N) -> List
%% @equiv rlist(N, 10)
rlist(N) -> rlist(N, 10).  % list of N random digits selected from 1..10

%% @spec msg_dump(Who, Patterns) -> failed
%%   Who = string() | nobody,
%%   Patterns = [ string() ]
%% @doc Print the pending messages of a process -- for debugging.
%%   <code>msg_dump</code> prints an error message where each line
%%   is prefixed by the process pid and a colon.  It states that a
%%   receive operation timed out, prints the list of patterns, and
%%   then prints all pending messages for the process.
%%   <ul>
%%     <li> If <code>Who</code> is a string, then the first line printed is
%%     	 <dl><dd><code>"$PID: receive timed out in $Who"</code></dd></dl>
%%       where <code>$PID$</code> is the PID of this process,
%%       and <code>$Who</code> is the string from <code>Who</code>.</li>
%%     <li> If <code>Who</code> is the atom <code>'nobody'</code>,
%%       then the first line printed is
%%       <dl><dd><code>"$PID: receive timed out</code></dd></dl>
%%       where <code>$PID$</code> is the PID of this process.</li>
%%   </ul>
%%   <code>Patterns</code> is the list of strings that describe what
%%   <code>receive</code> was looking for.
%%   <br/>
%%   <code>msg_dump</code> returns the atom <code>'failed'</code>
%%   <br/>
%%   <b>Note:</b> if the desired message were to arrive while we are printing,
%%   then it is possible that the pattern would be displayed, and later
%%   a message that matches that pattern would be printed.
msg_dump(Who, Patterns) ->
  MyPid = self(),
  M0 = io_lib:format("~w: receive timed out", [MyPid]),
  case Who of
    nobody -> io:format("~s~n", [M0]);
    _ -> io:format("~s in ~s~n", [M0, Who])
  end,
  io:format("~w: I was looking for~n", [MyPid]),
  msg_dump_patterns(Patterns, MyPid),
  io:format("~w: My pending messages are~n", [MyPid]),
  msg_dump_messages(MyPid),
  io:format("~w: giving up.~n", [MyPid]),
  failed.


%% @spec msg_dump(Patterns) -> failed
%%   Patterns = [ string() ]
%% @doc Print the pending messages of a process -- for debugging.
%%   <code>msg_dump(Patterns)</code> is equivalent to
%%   <code>msg_dump(nobody, Patterns)</code>.
msg_dump(Patterns) -> msg_dump(nobody, Patterns).

% msg_dump_patterns: a helper function for msg_dump.
%   Print the pattern strings.
msg_dump_patterns([], _MyPid) -> ok;
msg_dump_patterns([H | T], MyPid) ->
  io:format("~w:   ~s~n", [MyPid, H]),
  msg_dump_patterns(T, MyPid).

% msg_dump_messages: a helper function for msg_dump.
%   Print the pending messages of this process.
%
msg_dump_messages(MyPid) ->
  receive
    Msg ->
      io:format("~w:   ~w~n", [MyPid, Msg]),
      msg_dump_messages(MyPid)
    after 0 ->
      io:format("~w:   no more messages~n", [MyPid])
  end.


%% @spec sync(P, TimeOut, SndToken, RcvToken) -> ok | termR() | failed
%%   P = pid(),
%%   TimeOut = integer() | infinity
%%   SndToken = termS(),
%%   RcvToken = termR()
%% @doc  Synchronize with process <code>P</code>.
%%   This process sends <code>SndToken</code> to <code>P</code>, and
%%   <code>P</code> should respond by making a call to one of the versions
%%   of <code>sync</code> with this process as the target process.
%%   <ul>
%%     <li> If we receive <code>RcvToken</code>, we return it.</li>
%%     <li> If we receive the default token, we return <code>'ok'</code>.</li>
%%     <li> If a timout occurs, we print a message dump and return
%%		<code>'failed'</code>.</li>
%%   </ul>
%%   If <code>SndToken</code> is the empty tuple, <code>{}</code>, then
%%   the default <code>msg:sync</code> token is sent.  Likewise, if
%%   <code>RcvToken</code> is the empty tuple, then we only look to receive
%%   the default <code>msg:sync</code> token.
%%   <br/>
%%   A successful return from <code>sync</code> ensures that this process
%%   and process <code>P</code> have both reached the synchronization point
%%   before either proceeds.
%% @todo  Generalize the implementation to provide barriers.
sync(P, TimeOut, {}, {}) ->
  do_sync(P, TimeOut, default_sync(), default_sync(), false);
sync(P, TimeOut, SndToken, {}) ->
  do_sync(P, TimeOut, SndToken, default_sync(), false);
sync(P, TimeOut, {}, RcvToken) ->
  do_sync(P, TimeOut, default_sync(), RcvToken, true);
sync(P, TimeOut, SndToken, RcvToken) ->
  do_sync(P, TimeOut, SndToken, RcvToken, true).

%% @spec sync(P, SndToken, RcvToken) -> ok | termR() | failed
%% @equiv sync(P, infinity, SndToken, RcvToken)
sync(P, SndToken, RcvToken) -> sync(P, infinity, SndToken, RcvToken).

%% @spec sync(P, TimeOut) -> ok | failed
%%   P = pid(),
%%   TimeOut = integer() | infinity
%% @doc  Synchronize with process <code>P</code>.
%%   This process sends a 'sync' message to <code>P</code>, and
%%   <code>P</code> should call this function to send a 'sync'
%%   message to us.  When we receive the message from <code>P</code>
%%   we continue.  This ensures that both processes have made it to
%%   the point of their <code>sync</code> calls before either proceeds.
sync(P, TimeOut) -> sync(P, TimeOut, {}, {}).

%% @spec  sync(P) -> ok | exit | failed
%% @equiv sync(P, infinity)
sync(P) -> sync(P, infinity).

% do_sync(P, TimeOut, SndToken, RcvToken, Flag)
%   Send SndToken to P, then wait to receiv RecvToken or default_sync().
%   If Flag makes sure that we handle the corner case when someone call
%   sync/4 with default_sync() for RcvToken.  In this case, if we receive
%   default_sync(), we return default_sync().  In any other case, if we
%   receive default_sync(), we return the atom 'ok'.
do_sync(P, TimeOut, SndToken, RcvToken, Flag) ->
  P ! {self(), SndToken},
  DefSync = default_sync(),
  receive
    { P, RcvToken } when Flag -> RcvToken;
    { P, DefSync } -> ok
    after TimeOut ->
      P0 = [ io_lib:format("{~w, ~w}", [P, DefSync]) ],
      Patterns = case Flag of
        true  -> [ io_lib:format("{~w, ~w}", [P, RcvToken]) | P0 ];
	false -> P0
      end,
      msg_dump("msg:sync", Patterns),
      failed
  end.

% default_sync() -> the default token for msg:sync messages.
default_sync() -> 'msg:sync'.

%% @spec print(Term) -> ok
%% @equiv print(Term, 100)
print(Term) -> print(Term, 100).

%% @spec print(Term, N) -> ok
%% @doc  Print an erlang term on standard out.
%%   Unlike the erlang shell, print always prints a list of integers as a list of integers.
%%   Thus, <code>print("hello world"</code> is equivalent to
%%   <code>io:format("~s", "[104,101,108,108,111,32,119,111,114,108,100]")</code>.
%%   The point behind this is to prevent lists that really are supposed to be lists of
%%   integers from printing as strings just because every element of the list had a value
%%   in the range 32..127.
%%   <br/>
%%   <code>N</code> gives (roughly) the number of terms to print.  This means that if
%%   you ask to print a really big term, erlang won't be spewing stuff for several hours.
%%   If you want it to print the whole thing, use the atom <code>'infinity'</code> for
%%   <code>N</code>.
print(Term, N) -> pr(Term, N), io:format("~n"), ok.

pr(Int, N) when is_integer(Int) -> io:format("~b", [Int]), N-1;
pr([], N) -> io:format("[]"), N-1;
pr([H | T], N) -> io:format("["), pr_tl(T, pr(H, N-1), "]");
pr(Tuple, N) when is_tuple(Tuple) ->
  case tuple_to_list(Tuple) of
    [] -> io:format("{}"), N-1;
    [H | T] -> io:format("{"), pr_tl(T, pr(H, N-1), "}")
  end;
pr(X, N) -> io:format("~w", X), N-1.

pr_tl([], N, EndString) -> io:format("~s", [EndString]), N;
pr_tl([_ | _], 0, EndString) -> io:format(",...~s", [EndString]), 0;
pr_tl([H | T], N, EndString) -> io:format(","), pr_tl(T, pr(H, N), EndString).
